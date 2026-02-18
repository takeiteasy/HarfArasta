;;;; src/export/export.lisp
;;;; PNG and OBJ export for harfarasta

(defpackage #:harfarasta/export
  (:nicknames #:rich-text/export)
  (:use #:cl)
  (:local-nicknames (#:hb #:harfarasta/harfbuzz))
  (:export
   #:render-string))

(in-package #:harfarasta/export)

;;; --- PNG export ---

(defun %render-string-png (file font text pixel-height color
                           &key (alignment :left) fallback-fonts line-height
                                max-width (wrap :word) png-size)
  "Render TEXT using FONT to a PNG file at FILE."
  (let* ((upem (cffi:with-foreign-objects ((x :int) (y :int))
                 (hb:hb-font-get-scale font x y)
                 (cffi:mem-ref x :int)))
         (scale (/ (coerce pixel-height 'double-float)
                   (coerce upem 'double-float)))
         (max-width-fu (when max-width (round (/ max-width scale))))
         ;; shape-text now handles multiline and alignment via skip glyphs
         (glyphs (rich-text:shape-text font text
                                       :alignment alignment
                                       :line-height line-height
                                       :max-width max-width-fu :wrap wrap
                                       :fallback-fonts fallback-fonts))
         ;; Build glyph-data: (pen-x pen-y shape gid)
         ;; pen-x, pen-y in font units; pen-y is Y-down positive (0 = first line)
         (shape-cache (make-hash-table :test 'equal))
         (glyph-data
           (let ((cursor-x 0) (cursor-y 0))
             (loop for sg in glyphs
                   for skip     = (rich-text:shaped-glyph-skip sg)
                   for gid      = (rich-text:shaped-glyph-glyph-id sg)
                   for gly-font = (or (rich-text:shaped-glyph-font sg) font)
                   for pen-x    = (+ cursor-x (rich-text:shaped-glyph-x-offset sg))
                   for pen-y    = (+ cursor-y (rich-text:shaped-glyph-y-offset sg))
                   do (incf cursor-x (rich-text:shaped-glyph-x-advance sg))
                      (incf cursor-y (rich-text:shaped-glyph-y-advance sg))
                   unless skip
                     nconc (let* ((key   (cons gid (cffi:pointer-address gly-font)))
                                  (shape (or (gethash key shape-cache)
                                             (setf (gethash key shape-cache)
                                                   (rich-text:glyph-to-shape gly-font gid)))))
                             (when shape (list (list pen-x pen-y shape gid)))))))
         (padding 1.0)
         ;; pen-y is Y-down; font glyph coords are Y-up.
         ;; Effective screen Y = (font-y - pen-y) * scale, then image-flipped.
         (bounds (let ((vmin-x 0) (vmax-x 0)
                       (vmin-y 0) (vmax-y pixel-height))
                   (dolist (entry glyph-data (list vmin-x vmax-x vmin-y vmax-y))
                     (let* ((pen-x-fu (first entry))
                            (pen-y    (second entry))
                            (shape    (third entry))
                            (pen-x-px (round (* pen-x-fu scale))))
                       (multiple-value-bind (min-x min-y max-x max-y)
                           (harfarasta::shape-bounds shape)
                         (setf vmin-x (min vmin-x (+ pen-x-px (floor (* min-x scale))
                                                     (- (ceiling padding))))
                               vmax-x (max vmax-x (+ pen-x-px (ceiling (* max-x scale))
                                                     (ceiling padding)))
                               vmin-y (min vmin-y (+ (floor (* (- min-y pen-y) scale))
                                                     (- (ceiling padding))))
                               vmax-y (max vmax-y (+ (ceiling (* (- max-y pen-y) scale))
                                                     (ceiling padding)))))))))
         (x-offset (if (< (first bounds) 0) (- (first bounds)) 0))
         (y-offset (if (< (third bounds) 0) (- (third bounds)) 0))
         (canvas-width  (if (consp png-size)
                            (first png-size)
                            (max 1 (+ (second bounds) x-offset))))
         (canvas-height (if (consp png-size)
                            (second png-size)
                            (max 1 (+ (fourth bounds) y-offset)))))
    (let* ((png (make-instance 'zpng:png
                               :width canvas-width :height canvas-height
                               :color-type :truecolor-alpha))
           (image (zpng:data-array png))
           (cr (first color)) (cg (second color)) (cb (third color))
           (sdf-cache (make-hash-table :test 'equal)))
      (dolist (entry glyph-data)
        (let* ((pen-x-fu (first entry))
               (pen-y    (second entry))
               (shape    (third entry))
               (gid      (fourth entry))
               (pen-x-px (+ (round (* pen-x-fu scale)) x-offset)))
          (multiple-value-bind (min-x min-y max-x max-y)
              (harfarasta::shape-bounds shape)
            (let* ((gx0 (+ pen-x-px (floor (* min-x scale)) (- (ceiling padding))))
                   (gy0 (+ (floor (* (- min-y pen-y) scale)) (- (ceiling padding))))
                   (gx1 (min canvas-width
                             (+ pen-x-px (ceiling (* max-x scale)) (ceiling padding))))
                   (gy1 (min canvas-height
                             (+ (ceiling (* (- max-y pen-y) scale)) (ceiling padding))))
                   (gw (max 1 (- gx1 gx0)))
                   (gh (max 1 (- gy1 gy0)))
                   (sdf-scale scale)
                   (sdf-tx (coerce (- gx0 (* pen-x-fu scale)) 'double-float))
                   (sdf-ty (coerce (+ gy0 (* pen-y scale)) 'double-float))
                   (sdf-range (/ 2.0d0 sdf-scale)))
              (when (and (> gw 0) (> gh 0)
                         (< gx0 canvas-width) (> gx1 0)
                         (< (+ gy0 y-offset) canvas-height)
                         (> (+ gy1 y-offset) 0))
                (let* ((cache-key (list gid gw gh))
                       (sdf (or (gethash cache-key sdf-cache)
                                (setf (gethash cache-key sdf-cache)
                                      (harfarasta::generate-sdf-from-shape
                                       shape gw gh
                                       :range sdf-range :scale sdf-scale
                                       :translate-x sdf-tx :translate-y sdf-ty))))
                       (sdf-data (harfarasta::bitmap-data sdf))
                       (edge-w (coerce (/ 0.5d0 (* sdf-scale sdf-range)) 'single-float)))
                  (loop for sy from 0 below gh
                        for cy = (- canvas-height 1 (+ gy0 y-offset sy))
                        when (and (>= cy 0) (< cy canvas-height))
                          do (loop for sx from 0 below gw
                                   for cx = (+ gx0 sx)
                                   when (and (>= cx 0) (< cx canvas-width))
                                     do (let* ((sd (aref sdf-data (+ sx (* sy gw))))
                                               (coverage (- 1.0 (%smoothstep (- 0.5 edge-w) (+ 0.5 edge-w) sd)))
                                               (alpha (round (* (max 0.0 (min 1.0 coverage)) 255.0))))
                                          (when (> alpha 0)
                                            (let ((old-a (aref image cy cx 3)))
                                              (if (zerop old-a)
                                                  (setf (aref image cy cx 0) cr
                                                        (aref image cy cx 1) cg
                                                        (aref image cy cx 2) cb
                                                        (aref image cy cx 3) alpha)
                                                  (let* ((sa (/ alpha 255.0))
                                                         (da (/ old-a 255.0))
                                                         (out-a (+ sa (* da (- 1.0 sa)))))
                                                    (setf (aref image cy cx 0) cr
                                                          (aref image cy cx 1) cg
                                                          (aref image cy cx 2) cb
                                                          (aref image cy cx 3) (round (* out-a 255))))))))))))))))
      (zpng:write-png png file))))

(declaim (inline %smoothstep))
(defun %smoothstep (edge0 edge1 x)
  (let ((t-val (max 0.0 (min 1.0 (/ (- x edge0) (- edge1 edge0))))))
    (* t-val t-val (- 3.0 (* 2.0 t-val)))))

;;; --- OBJ export ---

(defun %render-string-obj (file font text size &key depth alignment line-height fallback-fonts
                                                    max-width (wrap :word))
  "Render TEXT using FONT to a Wavefront OBJ file at FILE.
SIZE is the scale factor applied to font-unit coordinates.
DEPTH, when non-NIL, extrudes the mesh along Z in the same units as SIZE.
ALIGNMENT is :left (default), :center, or :right.
LINE-HEIGHT is the Y distance between lines in font units (default = upem).
MAX-WIDTH triggers word wrapping in output units; WRAP is :word or :glyph."
  (let* ((upem (cffi:with-foreign-objects ((x :int) (y :int))
                 (hb:hb-font-get-scale font x y)
                 (cffi:mem-ref x :int)))
         (scale (/ (coerce size 'single-float)
                   (coerce upem 'single-float)))
         ;; Convert depth from output units to font units so that after
         ;; the OBJ export multiplies by scale, z comes out correct.
         (depth-fu (when depth (/ (coerce depth 'single-float) scale)))
         (max-width-fu (when max-width (round (/ max-width scale))))
         (meshes (rich-text:text-to-meshes font text :depth depth-fu
                                           :alignment alignment
                                           :line-height line-height
                                           :max-width max-width-fu :wrap wrap
                                           :fallback-fonts fallback-fonts))
         (global-vertex-offset 0))
    (with-open-file (out file :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (format out "# Wavefront OBJ exported by harfarasta/export~%")
      (format out "# Text: ~A~%~%" text)
      (loop for entry in meshes
            for glyph-idx from 0
            for pen-x = (first entry)
            for pen-y = (second entry)
            for vertices = (third entry)
            for indices = (fourth entry)
            for stride = (if depth 3 2)
            for vert-count = (/ (length vertices) stride)
            do
               (format out "# Glyph ~D~%" glyph-idx)
               ;; Write vertices; pen-y is Y-down so subtract to convert to Y-up OBJ space
               (loop for i from 0 below (length vertices) by stride
                     for vx = (+ (* (aref vertices i) scale) (* pen-x scale))
                     for vy = (- (* (aref vertices (1+ i)) scale) (* pen-y scale))
                     for vz = (if depth (* (aref vertices (+ i 2)) scale) 0.0)
                     do (format out "v ~F ~F ~F~%" vx vy vz))
               ;; Write faces (1-indexed)
               (loop for i from 0 below (length indices) by 3
                     for i0 = (+ (aref indices i) global-vertex-offset 1)
                     for i1 = (+ (aref indices (1+ i)) global-vertex-offset 1)
                     for i2 = (+ (aref indices (+ i 2)) global-vertex-offset 1)
                     do (format out "f ~D ~D ~D~%" i0 i1 i2))
               (incf global-vertex-offset vert-count)
               (terpri out)))))

;;; --- Public API ---

(defun render-string (text file &key (as :png) font-path (family "Arial")
                                     (weight :regular) (size 64)
                                     (color '(255 255 255)) depth (alignment :left)
                                     line-height fallback-fonts
                                     max-width (wrap :word) png-size)
  "Render TEXT to FILE in the specified format.
AS is :png or :obj.
Font can be specified by FONT-PATH or by FAMILY/WEIGHT for discovery.
SIZE is the pixel height for PNG or scale factor for OBJ.
COLOR is an (R G B) list (0-255) used for PNG output.
DEPTH, when non-NIL, extrudes OBJ meshes along Z.
ALIGNMENT is :left (default), :center, or :right (supports #\Newline in TEXT).
LINE-HEIGHT is the Y distance between lines in font units (default = upem).
FALLBACK-FONTS is a list of hb_font_t pointers tried when a glyph is missing.
MAX-WIDTH triggers automatic word wrapping; pixels for PNG, output units for OBJ.
WRAP controls the wrap mode: :word (default, break at word boundaries) or
:glyph (break at any glyph boundary, allowing mid-word breaks).
PNG-SIZE, when a list '(W H), fixes the canvas to exactly W x H pixels (PNG only);
nil or :relative uses auto-fit sizing."
  (let ((path (if font-path
                  (pathname font-path)
                  (rich-text:find-font-path :family family :weight weight))))
    (rich-text:with-font (f path)
      (ecase as
        (:png (%render-string-png file f text size color
                                  :alignment alignment :fallback-fonts fallback-fonts
                                  :line-height line-height :max-width max-width
                                  :wrap wrap :png-size png-size))
        (:obj (%render-string-obj file f text size :depth depth
                                  :alignment alignment :line-height line-height
                                  :fallback-fonts fallback-fonts
                                  :max-width max-width :wrap wrap))))))
