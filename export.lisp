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

(defun %render-string-png (file font text pixel-height color)
  "Render TEXT using FONT to a PNG file at FILE.
PIXEL-HEIGHT is the height of the output image in pixels.
COLOR is an (r g b) list with values 0-255."
  (let* ((glyphs (rich-text:shape-text font text))
         (upem (cffi:with-foreign-objects ((x :int) (y :int))
                 (hb:hb-font-get-scale font x y)
                 (cffi:mem-ref x :int)))
         ;; Scale: font units → pixels, with pixel-height = upem
         (scale (/ (coerce pixel-height 'double-float)
                   (coerce upem 'double-float)))
         ;; Collect per-glyph shapes and positions
         (shape-cache (make-hash-table))
         (glyph-data
           (let ((cursor-x 0))
             (loop for sg in glyphs
                   for gid = (rich-text:shaped-glyph-glyph-id sg)
                   for pen-x = (+ cursor-x (rich-text:shaped-glyph-x-offset sg))
                   for shape = (or (gethash gid shape-cache)
                                   (setf (gethash gid shape-cache)
                                         (rich-text:glyph-to-shape font gid)))
                   do (incf cursor-x (rich-text:shaped-glyph-x-advance sg))
                   when shape
                     collect (list pen-x shape gid))))
         ;; Compute visual bounding box from actual glyph shapes
         (padding 1.0)
         (advance-width (ceiling (* (loop for g in glyphs
                                          sum (rich-text:shaped-glyph-x-advance g))
                                    scale)))
         (bounds (let ((vmin-x 0)
                       (vmax-x advance-width)
                       (vmin-y 0)
                       (vmax-y pixel-height))
                   (dolist (entry glyph-data (list vmin-x vmax-x vmin-y vmax-y))
                     (let* ((pen-x-fu (first entry))
                            (shape (second entry))
                            (pen-x-px (round (* pen-x-fu scale))))
                       (multiple-value-bind (min-x min-y max-x max-y)
                           (harfarasta::shape-bounds shape)
                         (setf vmin-x (min vmin-x (+ pen-x-px (floor (* min-x scale))
                                                     (- (ceiling padding))))
                               vmax-x (max vmax-x (+ pen-x-px (ceiling (* max-x scale))
                                                     (ceiling padding)))
                               vmin-y (min vmin-y (+ (floor (* min-y scale))
                                                     (- (ceiling padding))))
                               vmax-y (max vmax-y (+ (ceiling (* max-y scale))
                                                     (ceiling padding)))))))))
         (x-offset (if (< (first bounds) 0) (- (first bounds)) 0))
         (y-offset (if (< (third bounds) 0) (- (third bounds)) 0))
         (canvas-width (max 1 (+ (second bounds) x-offset)))
         (canvas-height (max 1 (+ (fourth bounds) y-offset))))
    ;; Render each glyph's SDF in the shared coordinate system, then composite
    (let* ((png (make-instance 'zpng:png
                               :width canvas-width
                               :height canvas-height
                               :color-type :truecolor-alpha))
           (image (zpng:data-array png))
           (cr (first color))
           (cg (second color))
           (cb (third color))
           (sdf-cache (make-hash-table :test 'equal)))
      (dolist (entry glyph-data)
        (let* ((pen-x-fu (first entry))   ; font units
               (shape (second entry))
               (gid (third entry))
               (pen-x-px (+ (round (* pen-x-fu scale)) x-offset)))
          ;; Get shape bounds in font units
          (multiple-value-bind (min-x min-y max-x max-y)
              (harfarasta::shape-bounds shape)
            (let* (
                   ;; Glyph bounding box in pixels
                   (gx0 (+ pen-x-px (floor (* min-x scale)) (- (ceiling padding))))
                   (gy0 (+ (floor (* min-y scale)) (- (ceiling padding))))
                   (gx1 (min canvas-width
                             (+ pen-x-px (ceiling (* max-x scale)) (ceiling padding))))
                   (gy1 (min canvas-height
                             (+ (ceiling (* max-y scale)) (ceiling padding))))
                   (gw (max 1 (- gx1 gx0)))
                   (gh (max 1 (- gy1 gy0)))
                   ;; Compute scale + translate for this glyph's SDF
                   ;; pixel_x = (shape_x + pen_x_fu) * scale - gx0
                   ;; → shape_x = (pixel_x + gx0) / scale - pen_x_fu
                   ;; In MSDF: shape_x = (px + 0.5 + translate_x) / sdf_scale
                   ;; So sdf_scale = scale, translate_x = gx0 - pen_x_fu * scale
                   (sdf-scale scale)
                   (sdf-tx (coerce (- gx0 (* pen-x-fu scale)) 'double-float))
                   (sdf-ty (coerce gy0 'double-float))
                   ;; SDF range in font units: ~2 pixels worth of distance
                   (sdf-range (/ 2.0d0 sdf-scale)))
              (when (and (> gw 0) (> gh 0)
                         (< gx0 canvas-width) (< gy0 canvas-height)
                         (> gx1 0) (> gy1 0))
                (let* ((cache-key (list gid gw gh))
                       (sdf (or (gethash cache-key sdf-cache)
                                (setf (gethash cache-key sdf-cache)
                                      (harfarasta::generate-sdf-from-shape
                                       shape gw gh
                                       :range sdf-range :scale sdf-scale
                                       :translate-x sdf-tx :translate-y sdf-ty))))
                       (sdf-data (harfarasta::bitmap-data sdf))
                       ;; Smoothstep width: ~1px of anti-aliasing in SDF space
                       (edge-w (coerce (/ 0.5d0 (* sdf-scale sdf-range)) 'single-float)))
                  ;; Composite SDF-thresholded pixels onto canvas
                  (loop for sy from 0 below gh
                        for cy = (- canvas-height 1 (+ gy0 y-offset sy)) ; flip Y for PNG (top-down)
                        when (and (>= cy 0) (< cy canvas-height))
                          do (loop for sx from 0 below gw
                                   for cx = (+ gx0 sx)
                                   when (and (>= cx 0) (< cx canvas-width))
                                     do (let* ((sd (aref sdf-data (+ sx (* sy gw))))
                                               ;; SDF: 0=inside, 0.5=edge, 1=outside
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

(defun %render-string-obj (file font text size &key depth)
  "Render TEXT using FONT to a Wavefront OBJ file at FILE.
SIZE is the scale factor applied to font-unit coordinates.
DEPTH, when non-NIL, extrudes the mesh along Z in the same units as SIZE."
  (let* ((upem (cffi:with-foreign-objects ((x :int) (y :int))
                 (hb:hb-font-get-scale font x y)
                 (cffi:mem-ref x :int)))
         (scale (/ (coerce size 'single-float)
                   (coerce upem 'single-float)))
         ;; Convert depth from output units to font units so that after
         ;; the OBJ export multiplies by scale, z comes out correct.
         (depth-fu (when depth (/ (coerce depth 'single-float) scale)))
         (meshes (rich-text:text-to-meshes font text :depth depth-fu))
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
               ;; Write vertices
               (loop for i from 0 below (length vertices) by stride
                     for vx = (+ (* (aref vertices i) scale) (* pen-x scale))
                     for vy = (* (aref vertices (1+ i)) scale)
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
                                     (color '(255 255 255)) depth)
  "Render TEXT to FILE in the specified format.
AS is :png or :obj.
Font can be specified by FONT-PATH or by FAMILY/WEIGHT for discovery.
SIZE is the pixel height for PNG or scale factor for OBJ.
COLOR is an (R G B) list (0-255) used for PNG output.
DEPTH, when non-NIL, extrudes OBJ meshes along Z."
  (let ((path (if font-path
                  (pathname font-path)
                  (rich-text:find-font-path :family family :weight weight))))
    (rich-text:with-font (f path)
      (ecase as
        (:png (%render-string-png file f text size color))
        (:obj (%render-string-obj file f text size :depth depth))))))
