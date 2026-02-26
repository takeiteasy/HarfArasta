;;;; fontstash.lisp
;;;; Font atlas / glyph packing module for harfarasta

(defpackage #:harfarasta/fontstash
  (:nicknames #:rich-text/fontstash)
  (:use #:cl)
  (:local-nicknames (#:hb #:harfarasta/harfbuzz))
  (:export
   #:atlas-region
   #:atlas-region-x
   #:atlas-region-y
   #:atlas-region-width
   #:atlas-region-height
   #:atlas-entry
   #:atlas-entry-glyph-id
   #:atlas-entry-region
   #:atlas-entry-u0
   #:atlas-entry-v0
   #:atlas-entry-u1
   #:atlas-entry-v1
   #:atlas-entry-fu-x0
   #:atlas-entry-fu-y0
   #:atlas-entry-fu-x1
   #:atlas-entry-fu-y1
   #:font-atlas
   #:font-atlas-width
   #:font-atlas-height
   #:font-atlas-mode
   #:font-atlas-bitmap
   #:make-font-atlas
   #:atlas-add-glyph
   #:atlas-add-glyphs
   #:atlas-add-text
   #:atlas-add-glyph-scaled
   #:atlas-add-chars
   #:atlas-lookup
   #:atlas-to-png))

(in-package #:harfarasta/fontstash)

;;; --- Data structures ---

(defstruct atlas-region
  "A rectangle within the atlas texture."
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defstruct atlas-entry
  "A glyph packed into the atlas."
  (glyph-id 0 :type fixnum)
  (region nil :type (or null atlas-region))
  (u0 0.0 :type single-float)
  (v0 0.0 :type single-float)
  (u1 0.0 :type single-float)
  (v1 0.0 :type single-float)
  ;; Font-unit bounds of the rendered bitmap region (msdfgen convention).
  ;; shape_x = (pixel_x + 0.5 + tx) / scale
  ;; At pixel 0: fu-x0 = (0.5 + tx) / scale
  ;; At pixel w: fu-x1 = (w + 0.5 + tx) / scale
  (fu-x0 0.0 :type single-float)
  (fu-y0 0.0 :type single-float)
  (fu-x1 0.0 :type single-float)
  (fu-y1 0.0 :type single-float))

(defstruct (font-atlas (:constructor %make-font-atlas))
  "A texture atlas for packing rendered glyphs."
  (bitmap nil :type (or null harfarasta:bitmap))
  (width 512 :type fixnum)
  (height 512 :type fixnum)
  (mode :sdf :type keyword)
  (entries (make-hash-table) :type hash-table)
  (skyline nil :type list)
  (padding 1 :type fixnum))

;;; --- Skyline rectangle packing ---

(defun %skyline-init (width)
  "Initialize skyline with a single span covering the full width at height 0."
  (list (cons 0 0) (cons width 0)))

(defun %skyline-find-best (skyline rect-w rect-h atlas-w atlas-h)
  "Find the best position to place a rectangle of RECT-W x RECT-H.
Returns (VALUES best-x best-y span-index) or NIL if it doesn't fit."
  (let ((best-x nil)
        (best-y nil)
        (best-idx nil)
        (best-waste most-positive-fixnum)
        (n (length skyline)))
    (loop for i from 0 below n
          for span = (nth i skyline)
          for sx = (car span)
          do ;; Check if rect fits horizontally starting at sx
             (when (<= (+ sx rect-w) atlas-w)
               ;; Find max height under this rect across all spans it covers
               (let ((max-h 0)
                     (fits t))
                 (loop for j from i below n
                       for sp = (nth j skyline)
                       for sp-x = (car sp)
                       for sp-h = (cdr sp)
                       while (< sp-x (+ sx rect-w))
                       do (when (> sp-h max-h)
                            (setf max-h sp-h))
                       finally (when (> (+ max-h rect-h) atlas-h)
                                 (setf fits nil)))
                 (when (and fits (<= (+ max-h rect-h) atlas-h))
                   (let ((waste max-h))
                     (when (< waste best-waste)
                       (setf best-x sx
                             best-y max-h
                             best-idx i
                             best-waste waste)))))))
    (if best-x
        (values best-x best-y best-idx)
        nil)))

(defun %skyline-update (skyline x y w h)
  "Update skyline after placing a rect at (X,Y) with size W x H.
Returns the new skyline."
  (let* ((new-top (+ y h))
         (right (+ x w))
         ;; Find the height at position 'right' in the original skyline
         ;; (needed to restore the skyline after the placed rect ends)
         (h-at-right 0))
    ;; Walk original skyline to find height at 'right'
    (loop for i from 0 below (length skyline)
          for span = (nth i skyline)
          for next = (if (< (1+ i) (length skyline))
                         (nth (1+ i) skyline)
                         nil)
          do (when (and (>= right (car span))
                        (or (null next) (< right (car next))))
               (setf h-at-right (cdr span))
               (return)))
    ;; Build new skyline: keep spans strictly before x, insert new span,
    ;; insert right-edge restore span, keep spans at or after right
    (let ((result nil))
      ;; Spans strictly before the placed rect
      (dolist (span skyline)
        (when (< (car span) x)
          (push span result)))
      ;; The placed rect's top edge
      (push (cons x new-top) result)
      ;; Restore height after the rect (unless a span already starts there)
      (unless (find right skyline :key #'car :test #'=)
        (push (cons right h-at-right) result))
      ;; Spans at or after the right edge of the placed rect
      (dolist (span skyline)
        (when (>= (car span) right)
          (push span result)))
      ;; Sort by x, then merge consecutive same-height spans
      (setf result (sort result #'< :key #'car))
      (let ((merged nil))
        (dolist (s result)
          (if (and merged (= (cdr (first merged)) (cdr s)))
              nil
              (push s merged)))
        (nreverse merged)))))

;;; --- Internal helpers ---

(defun %blit-bitmap (dst dst-x dst-y dst-w src src-w src-h channels)
  "Copy SRC bitmap data into DST at position (DST-X, DST-Y).
DST is the destination data array, SRC is the source data array.
DST-W is the destination atlas width. CHANNELS is the number of channels."
  (loop for sy from 0 below src-h
        for dy = (+ dst-y sy)
        do (loop for sx from 0 below src-w
                 for dx = (+ dst-x sx)
                 do (loop for c from 0 below channels
                          for src-idx = (+ (* (+ sx (* sy src-w)) channels) c)
                          for dst-idx = (+ (* (+ dx (* dy dst-w)) channels) c)
                          do (setf (aref dst dst-idx) (aref src src-idx))))))

(defun %render-glyph (font glyph-id w h mode)
  "Render a glyph bitmap using the specified mode. Returns a bitmap or NIL."
  (ecase mode
    (:sdf (harfarasta:glyph-to-sdf font glyph-id w h))
    (:msdf (harfarasta:glyph-to-msdf font glyph-id w h))
    (:bitmap (harfarasta:glyph-to-bitmap font glyph-id w h))
    (:bitmap-fast (harfarasta:glyph-to-bitmap-fast font glyph-id w h))))

;;; --- Public API ---

(defun make-font-atlas (&key (width 512) (height 512) (mode :sdf) (padding 1))
  "Create an empty font atlas of the given dimensions.
MODE is :sdf, :msdf, or :bitmap, controlling how glyphs are rendered.
PADDING is the number of pixels between packed glyphs."
  (let ((channels (if (eq mode :msdf) 3 1)))
    (%make-font-atlas
     :bitmap (harfarasta:make-bitmap width height channels)
     :width width
     :height height
     :mode mode
     :entries (make-hash-table)
     :skyline (%skyline-init width)
     :padding padding)))

(defun atlas-add-glyph (atlas font glyph-id w h)
  "Render glyph GLYPH-ID from FONT at size W x H and pack it into ATLAS.
Returns an ATLAS-ENTRY on success, or NIL if the glyph is blank or doesn't fit."
  ;; Return existing entry if already packed
  (let ((existing (gethash glyph-id (font-atlas-entries atlas))))
    (when existing (return-from atlas-add-glyph existing)))
  (let ((glyph-bmp (%render-glyph font glyph-id w h (font-atlas-mode atlas))))
    (when (null glyph-bmp) (return-from atlas-add-glyph nil))
    (let* ((pad (font-atlas-padding atlas))
           (padded-w (+ w (* 2 pad)))
           (padded-h (+ h (* 2 pad))))
      (multiple-value-bind (px py)
          (%skyline-find-best (font-atlas-skyline atlas)
                              padded-w padded-h
                              (font-atlas-width atlas)
                              (font-atlas-height atlas))
        (when (null px) (return-from atlas-add-glyph nil))
        ;; Update skyline
        (setf (font-atlas-skyline atlas)
              (%skyline-update (font-atlas-skyline atlas)
                               px py padded-w padded-h))
        ;; Blit glyph bitmap into atlas at padded position
        (let ((gx (+ px pad))
              (gy (+ py pad))
              (channels (harfarasta:bitmap-channels glyph-bmp)))
          (%blit-bitmap (harfarasta:bitmap-data (font-atlas-bitmap atlas))
                        gx gy (font-atlas-width atlas)
                        (harfarasta:bitmap-data glyph-bmp)
                        w h channels)
          ;; Create entry with UV coordinates
          (let* ((aw (coerce (font-atlas-width atlas) 'single-float))
                 (ah (coerce (font-atlas-height atlas) 'single-float))
                 (entry (make-atlas-entry
                         :glyph-id glyph-id
                         :region (make-atlas-region :x gx :y gy :width w :height h)
                         :u0 (/ (coerce gx 'single-float) aw)
                         :v0 (/ (coerce gy 'single-float) ah)
                         :u1 (/ (coerce (+ gx w) 'single-float) aw)
                         :v1 (/ (coerce (+ gy h) 'single-float) ah))))
            (setf (gethash glyph-id (font-atlas-entries atlas)) entry)
            entry))))))

(defun atlas-add-glyphs (atlas font glyph-ids w h)
  "Add multiple glyphs to ATLAS. GLYPH-IDS is a list of glyph ID integers.
Returns a list of ATLAS-ENTRY objects (NIL entries for blank glyphs)."
  (mapcar (lambda (gid) (atlas-add-glyph atlas font gid w h)) glyph-ids))

(defun atlas-add-text (atlas font text w h &key direction script language basic)
  "Shape TEXT with FONT and add all unique glyphs to ATLAS at size W x H.
Returns a list of ATLAS-ENTRY objects for the shaped glyphs."
  (let* ((shaped (harfarasta:shape-text font text :direction direction
                                                   :script script :language language
                                                   :basic basic))
         (seen (make-hash-table))
         (entries nil))
    (dolist (sg shaped)
      (let ((gid (harfarasta:shaped-glyph-glyph-id sg)))
        (unless (gethash gid seen)
          (setf (gethash gid seen) t)
          (push (atlas-add-glyph atlas font gid w h) entries))))
    (nreverse entries)))

(defun atlas-lookup (atlas glyph-id)
  "Look up a glyph in the atlas by GLYPH-ID. Returns an ATLAS-ENTRY or NIL."
  (gethash glyph-id (font-atlas-entries atlas)))

(defun atlas-to-png (atlas file)
  "Export the atlas texture as a PNG image to FILE."
  (let* ((bmp (font-atlas-bitmap atlas))
         (w (harfarasta:bitmap-width bmp))
         (h (harfarasta:bitmap-height bmp))
         (channels (harfarasta:bitmap-channels bmp))
         (data (harfarasta:bitmap-data bmp))
         (color-type (if (= channels 3) :truecolor :grayscale))
         (png (make-instance 'zpng:png
                             :width w :height h
                             :color-type color-type))
         (image (zpng:data-array png)))
    ;; Copy float data [0,1] to uint8 [0,255], flipping Y for PNG (top-down)
    (loop for y from 0 below h
          for src-y = (- h 1 y) ; flip Y: bitmap is bottom-up, PNG is top-down
          do (loop for x from 0 below w
                   do (loop for c from 0 below channels
                            for src-idx = (+ (* (+ x (* src-y w)) channels) c)
                            for val = (round (* (max 0.0 (min 1.0 (aref data src-idx))) 255.0))
                            do (setf (aref image y x c) val))))
    (zpng:write-png png file)))

;;; --- Scaled glyph atlas helpers ---

(declaim (inline %smoothstep-atlas))
(defun %smoothstep-atlas (edge0 edge1 x)
  "Smoothstep interpolation for SDF thresholding."
  (let ((t-val (max 0.0 (min 1.0 (/ (- x edge0) (- edge1 edge0))))))
    (* t-val t-val (- 3.0 (* 2.0 t-val)))))

(defun %render-glyph-sdf-scaled (shape scale bw bh tx ty)
  "Render SHAPE as a 1-channel SDF coverage bitmap using explicit SCALE and translate TX/TY.
BW and BH are the output bitmap dimensions in pixels.
Returns a harfarasta:bitmap with per-pixel coverage values [0,1]."
  (let* ((range (/ 2.0d0 scale))
         (sdf (harfarasta:generate-sdf-from-shape shape bw bh
                                                  :range range :scale scale
                                                  :translate-x tx :translate-y ty))
         (sdf-data (harfarasta:bitmap-data sdf))
         (bmp (harfarasta:make-bitmap bw bh 1))
         (bmp-data (harfarasta:bitmap-data bmp))
         (edge-w (coerce (/ 0.5d0 (* scale range)) 'single-float)))
    (loop for i from 0 below (length sdf-data)
          for sd single-float = (aref sdf-data i)
          do (setf (aref bmp-data i)
                   (coerce (- 1.0 (%smoothstep-atlas (- 0.5 edge-w) (+ 0.5 edge-w) sd))
                           'single-float)))
    bmp))

(defun atlas-add-glyph-scaled (atlas font glyph-id pixels-per-em &optional (padding 2))
  "Render GLYPH-ID at PIXELS-PER-EM scale, auto-sizing the bitmap from shape bounds.
PIXELS-PER-EM is the desired render size in pixels per em (e.g. 64).
PADDING is the number of extra pixels around the glyph shape for SDF range (default 2).
Stores font-unit bounds in the atlas-entry via fu-x0/y0/x1/y1.
Returns ATLAS-ENTRY on success, or NIL if the glyph is blank or the atlas is full."
  ;; Return existing entry if already packed
  (let ((existing (gethash glyph-id (font-atlas-entries atlas))))
    (when existing (return-from atlas-add-glyph-scaled existing)))
  ;; Get the glyph outline shape
  (let ((shape (harfarasta:glyph-to-shape font glyph-id)))
    (when (null shape) (return-from atlas-add-glyph-scaled nil))
    (multiple-value-bind (min-x min-y max-x max-y)
        (harfarasta:shape-bounds shape)
      ;; Derive upem from the HarfBuzz font scale setting
      (let* ((upem (cffi:with-foreign-objects ((x :int) (y :int))
                     (hb:hb-font-get-scale font x y)
                     (cffi:mem-ref x :int)))
             ;; pixels per font unit
             (scale (/ (coerce pixels-per-em 'double-float)
                       (coerce upem 'double-float)))
             (shape-w (coerce (- max-x min-x) 'double-float))
             (shape-h (coerce (- max-y min-y) 'double-float))
             ;; Bitmap sized to glyph bounds plus SDF padding
             (bw (max 4 (+ (ceiling (* shape-w scale)) (* 2 padding))))
             (bh (max 4 (+ (ceiling (* shape-h scale)) (* 2 padding))))
             ;; msdfgen convention: shape_x = (pixel_x + 0.5 + tx) / scale
             ;; At pixel_x = padding, shape_x ≈ min-x:
             ;; tx = min-x * scale - padding
             (tx (- (* (coerce min-x 'double-float) scale)
                    (coerce padding 'double-float)))
             (ty (- (* (coerce min-y 'double-float) scale)
                    (coerce padding 'double-float))))
        ;; Render glyph bitmap; :sdf uses explicit scale, :bitmap-fast uses winding raster,
        ;; others use auto-scale via %render-glyph
        (let ((glyph-bmp (let ((mode (font-atlas-mode atlas)))
                           (cond ((eq mode :sdf)
                                  (%render-glyph-sdf-scaled shape scale bw bh tx ty))
                                 ((eq mode :bitmap-fast)
                                  (harfarasta:shape-to-bitmap-fast shape bw bh :scale scale :tx tx :ty ty))
                                 (t (%render-glyph font glyph-id bw bh mode))))))
          (when (null glyph-bmp) (return-from atlas-add-glyph-scaled nil))
          (let* ((pad (font-atlas-padding atlas))
                 (padded-w (+ bw (* 2 pad)))
                 (padded-h (+ bh (* 2 pad))))
            (multiple-value-bind (px py)
                (%skyline-find-best (font-atlas-skyline atlas)
                                    padded-w padded-h
                                    (font-atlas-width atlas)
                                    (font-atlas-height atlas))
              (when (null px) (return-from atlas-add-glyph-scaled nil))
              ;; Update skyline
              (setf (font-atlas-skyline atlas)
                    (%skyline-update (font-atlas-skyline atlas)
                                     px py padded-w padded-h))
              ;; Blit glyph bitmap into atlas at padded position
              (let ((gx (+ px pad))
                    (gy (+ py pad))
                    (channels (harfarasta:bitmap-channels glyph-bmp)))
                (%blit-bitmap (harfarasta:bitmap-data (font-atlas-bitmap atlas))
                              gx gy (font-atlas-width atlas)
                              (harfarasta:bitmap-data glyph-bmp)
                              bw bh channels)
                ;; Compute UV and font-unit bounds
                (let* ((aw (coerce (font-atlas-width atlas) 'single-float))
                       (ah (coerce (font-atlas-height atlas) 'single-float))
                       (entry (make-atlas-entry
                               :glyph-id glyph-id
                               :region (make-atlas-region :x gx :y gy :width bw :height bh)
                               :u0 (/ (coerce gx 'single-float) aw)
                               :v0 (/ (coerce gy 'single-float) ah)
                               :u1 (/ (coerce (+ gx bw) 'single-float) aw)
                               :v1 (/ (coerce (+ gy bh) 'single-float) ah)
                               ;; Font-unit bounds from msdfgen convention
                               :fu-x0 (coerce (/ (+ 0.5d0 tx) scale) 'single-float)
                               :fu-y0 (coerce (/ (+ 0.5d0 ty) scale) 'single-float)
                               :fu-x1 (coerce (/ (+ (coerce bw 'double-float) 0.5d0 tx) scale) 'single-float)
                               :fu-y1 (coerce (/ (+ (coerce bh 'double-float) 0.5d0 ty) scale) 'single-float))))
                  (setf (gethash glyph-id (font-atlas-entries atlas)) entry)
                  entry)))))))))

(defun atlas-add-chars (atlas font string pixels-per-em &key (padding 2) basic)
  "Shape STRING and add all unique glyphs to ATLAS at PIXELS-PER-EM scale.
PADDING is the extra pixel border around each glyph bitmap for SDF range (default 2).
Returns a list of ATLAS-ENTRY objects (NIL for blank or non-fitting glyphs)."
  (let* ((shaped (harfarasta:shape-text font string :basic basic))
         (seen (make-hash-table))
         (entries nil))
    (dolist (sg shaped)
      (let ((gid (harfarasta:shaped-glyph-glyph-id sg)))
        (unless (gethash gid seen)
          (setf (gethash gid seen) t)
          (push (atlas-add-glyph-scaled atlas font gid pixels-per-em padding) entries))))
    (nreverse entries)))
