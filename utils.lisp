;;;; harfarasta.lisp
;;;; High-level API and utilities

(in-package #:harfarasta)

;;; Edge construction helpers

(defun make-linear-edge (x0 y0 x1 y1)
  "Create a linear edge from (X0,Y0) to (X1,Y1)."
  (make-edge-segment +edge-type-linear+
                     (vec2 (coerce x0 'single-float) (coerce y0 'single-float))
                     (vec2 (coerce x1 'single-float) (coerce y1 'single-float))))

(defun make-quadratic-edge (x0 y0 cx cy x1 y1)
  "Create a quadratic Bezier edge from (X0,Y0) to (X1,Y1) with control point (CX,CY)."
  (make-edge-segment +edge-type-quadratic+
                     (vec2 (coerce x0 'single-float) (coerce y0 'single-float))
                     (vec2 (coerce cx 'single-float) (coerce cy 'single-float))
                     (vec2 (coerce x1 'single-float) (coerce y1 'single-float))))

(defun make-cubic-edge (x0 y0 cx0 cy0 cx1 cy1 x1 y1)
  "Create a cubic Bezier edge from (X0,Y0) to (X1,Y1) with control points."
  (make-edge-segment +edge-type-cubic+
                     (vec2 (coerce x0 'single-float) (coerce y0 'single-float))
                     (vec2 (coerce cx0 'single-float) (coerce cy0 'single-float))
                     (vec2 (coerce cx1 'single-float) (coerce cy1 'single-float))
                     (vec2 (coerce x1 'single-float) (coerce y1 'single-float))))

;;; Path builder API (SVG-like)

(defun path-move-to (builder x y)
  "Start a new contour at (X, Y)."
  (declare (type path-builder builder))
  ;; Close any existing contour
  (when (path-builder-current-edges builder)
    (path-close builder))
  ;; Start new contour
  (let ((p (vec2 (coerce x 'single-float) (coerce y 'single-float))))
    (setf (path-builder-start-point builder) p
          (path-builder-current-point builder) p))
  builder)

(defun path-line-to (builder x y)
  "Add a line from current point to (X, Y)."
  (declare (type path-builder builder))
  (let* ((p0 (path-builder-current-point builder))
         (p1 (vec2 (coerce x 'single-float) (coerce y 'single-float)))
         (edge (make-edge-segment +edge-type-linear+ (v2-copy p0) p1)))
    (push edge (path-builder-current-edges builder))
    (setf (path-builder-current-point builder) p1))
  builder)

(defun path-quadratic-to (builder cx cy x y)
  "Add a quadratic Bezier from current point to (X, Y) with control point (CX, CY)."
  (declare (type path-builder builder))
  (let* ((p0 (path-builder-current-point builder))
         (p1 (vec2 (coerce cx 'single-float) (coerce cy 'single-float)))
         (p2 (vec2 (coerce x 'single-float) (coerce y 'single-float)))
         (edge (make-edge-segment +edge-type-quadratic+ (v2-copy p0) p1 p2)))
    (push edge (path-builder-current-edges builder))
    (setf (path-builder-current-point builder) p2))
  builder)

(defun path-cubic-to (builder cx0 cy0 cx1 cy1 x y)
  "Add a cubic Bezier from current point to (X, Y) with control points."
  (declare (type path-builder builder))
  (let* ((p0 (path-builder-current-point builder))
         (c0 (vec2 (coerce cx0 'single-float) (coerce cy0 'single-float)))
         (c1 (vec2 (coerce cx1 'single-float) (coerce cy1 'single-float)))
         (p1 (vec2 (coerce x 'single-float) (coerce y 'single-float)))
         (edge (make-edge-segment +edge-type-cubic+ (v2-copy p0) c0 c1 p1)))
    (push edge (path-builder-current-edges builder))
    (setf (path-builder-current-point builder) p1))
  builder)

(defun path-close (builder)
  "Close the current contour by connecting back to the start point."
  (declare (type path-builder builder))
  (when (path-builder-current-edges builder)
    (let* ((start (path-builder-start-point builder))
           (current (path-builder-current-point builder)))
      ;; Add closing edge if not already at start
      (unless (v2-equal current start)
        (let ((edge (make-edge-segment +edge-type-linear+
                                       (v2-copy current)
                                       (v2-copy start))))
          (push edge (path-builder-current-edges builder))))
      ;; Create contour
      (let ((contour (%make-contour :edges (nreverse (path-builder-current-edges builder)))))
        (push contour (path-builder-contours builder)))
      ;; Reset state
      (setf (path-builder-current-edges builder) nil
            (path-builder-start-point builder) nil
            (path-builder-current-point builder) nil)))
  builder)

(defun path-to-shape (builder)
  "Convert the path builder to a shape. Closes any open contour first."
  (declare (type path-builder builder))
  ;; Close any open contour
  (when (path-builder-current-edges builder)
    (path-close builder))
  ;; Create shape
  (%make-shape :contours (nreverse (path-builder-contours builder))))

;;; Utility functions

(defun shape-bounds (shape)
  "Calculate the bounding box of a shape.
   Returns (values min-x min-y max-x max-y)."
  (declare (type shape shape))
  (let ((min-x most-positive-single-float)
        (min-y most-positive-single-float)
        (max-x most-negative-single-float)
        (max-y most-negative-single-float))
    (dolist (contour (shape-contours shape))
      (dolist (edge (contour-edges contour))
        (flet ((update-bounds (p)
                 (when p
                   (setf min-x (min min-x (vec2-x p))
                         min-y (min min-y (vec2-y p))
                         max-x (max max-x (vec2-x p))
                         max-y (max max-y (vec2-y p))))))
          (update-bounds (edge-segment-p0 edge))
          (update-bounds (edge-segment-p1 edge))
          (update-bounds (edge-segment-p2 edge))
          (update-bounds (edge-segment-p3 edge)))))
    (values min-x min-y max-x max-y)))

(defun auto-scale-shape (shape width height &key (padding 0.0))
  "Calculate scale and translation to fit shape in bitmap.
   PADDING is border padding in pixels.
   Returns (values scale translate-x translate-y)."
  (declare (type shape shape) (type fixnum width height)
           (type (or fixnum single-float double-float) padding))
  (multiple-value-bind (min-x min-y max-x max-y)
      (shape-bounds shape)
    (let* ((shape-width (- max-x min-x))
           (shape-height (- max-y min-y))
           (pad (coerce padding 'double-float))
           (avail-width (- width (* 2.0d0 pad)))
           (avail-height (- height (* 2.0d0 pad)))
           (scale-x (if (zerop shape-width) 1.0d0 (/ avail-width shape-width)))
           (scale-y (if (zerop shape-height) 1.0d0 (/ avail-height shape-height)))
           (scale (min scale-x scale-y))
           ;; Center the shape
           (translate-x (- pad (* min-x scale)))
           (translate-y (- pad (* min-y scale))))
      (values scale translate-x translate-y))))

(defun bitmap-to-bytes (bitmap)
  "Convert a float bitmap to (unsigned-byte 8) array.
   Values are clamped to [0, 1] and scaled to [0, 255]."
  (declare (type bitmap bitmap))
  (let* ((data (bitmap-data bitmap))
         (len (length data))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len)
      (setf (aref out i)
            (truncate (* (clamp01f (aref data i)) 255.0))))
    out))

(defun bitmap-pixel (bitmap x y)
  "Get pixel value(s) from bitmap at (X, Y).
   Returns single value for 1-channel, vec3 for 3-channel."
  (declare (type bitmap bitmap) (type fixnum x y))
  (let* ((channels (bitmap-channels bitmap))
         (idx (* channels (+ x (* y (bitmap-width bitmap)))))
         (data (bitmap-data bitmap)))
    (if (= channels 1)
        (aref data idx)
        (vec3 (aref data idx)
              (aref data (+ idx 1))
              (aref data (+ idx 2))))))

(defun (setf bitmap-pixel) (value bitmap x y)
  "Set pixel value(s) in bitmap at (X, Y)."
  (declare (type bitmap bitmap) (type fixnum x y))
  (let* ((channels (bitmap-channels bitmap))
         (idx (* channels (+ x (* y (bitmap-width bitmap)))))
         (data (bitmap-data bitmap)))
    (if (= channels 1)
        (setf (aref data idx) (coerce value 'single-float))
        (progn
          (setf (aref data idx) (vec3-x value)
                (aref data (+ idx 1)) (vec3-y value)
                (aref data (+ idx 2)) (vec3-z value))
          value))))

;;; Convenience functions

(defun make-rectangle-shape (x0 y0 x1 y1)
  "Create a shape containing a single rectangular contour."
  (let ((builder (make-path-builder)))
    (path-move-to builder x0 y0)
    (path-line-to builder x1 y0)
    (path-line-to builder x1 y1)
    (path-line-to builder x0 y1)
    (path-close builder)
    (path-to-shape builder)))

(defun make-circle-shape (cx cy radius &optional (segments 32))
  "Create a shape containing a circular contour approximated with Bezier curves."
  (declare (type fixnum segments))
  (let* ((builder (make-path-builder))
         (k (/ (* 4.0 (- (sqrt 2.0) 1.0)) 3.0)) ; Bezier control point factor
         (step (/ (* 2.0 pi) segments)))
    (path-move-to builder (+ cx radius) cy)
    (loop for i from 0 below segments
          for angle = (* i step)
          for next-angle = (* (1+ i) step)
          for x0 = (+ cx (* radius (cos angle)))
          for y0 = (+ cy (* radius (sin angle)))
          for x1 = (+ cx (* radius (cos next-angle)))
          for y1 = (+ cy (* radius (sin next-angle)))
          ;; Control points for circular arc approximation
          for dx0 = (* (- (sin angle)) radius k step 0.5)
          for dy0 = (* (cos angle) radius k step 0.5)
          for dx1 = (* (sin next-angle) radius k step 0.5)
          for dy1 = (* (- (cos next-angle)) radius k step 0.5)
          do (path-cubic-to builder
                            (+ x0 dx0) (+ y0 dy0)
                            (+ x1 dx1) (+ y1 dy1)
                            x1 y1))
    (path-close builder)
    (path-to-shape builder)))

(defun make-triangle-shape (x0 y0 x1 y1 x2 y2)
  "Create a shape containing a single triangular contour."
  (let ((builder (make-path-builder)))
    (path-move-to builder x0 y0)
    (path-line-to builder x1 y1)
    (path-line-to builder x2 y2)
    (path-close builder)
    (path-to-shape builder)))