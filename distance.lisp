;;;; distance.lisp
;;;; Signed distance calculations for Bezier curves
;;;; Based on msdf.h/msdf.c by Viktor Chlumský

(in-package #:harfarasta)

;;; Point on curve functions

(defun linear-point (edge param)
  "Get point on linear edge at parameter T in [0,1]."
  (declare (type edge-segment edge) (type double-float param))
  (let ((p0 (edge-segment-p0 edge))
        (p1 (edge-segment-p1 edge))
        (w (coerce param 'single-float)))
    (v2-mix p0 p1 w)))

(defun quadratic-point (edge param)
  "Get point on quadratic Bezier at parameter T in [0,1]."
  (declare (type edge-segment edge) (type double-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (w (coerce param 'single-float))
         (a (v2-mix p0 p1 w))
         (b (v2-mix p1 p2 w)))
    (v2-mix a b w)))

(defun cubic-point (edge param)
  "Get point on cubic Bezier at parameter T in [0,1]."
  (declare (type edge-segment edge) (type double-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (p3 (edge-segment-p3 edge))
         (w (coerce param 'single-float))
         (p12 (v2-mix p1 p2 w))
         (a (v2-mix p0 p1 w))
         (b (v2-mix a p12 w))
         (c (v2-mix p2 p3 w))
         (d (v2-mix p12 c w)))
    (v2-mix b d w)))

(defun edge-point (edge param)
  "Get point on edge at parameter T in [0,1]."
  (declare (type edge-segment edge) (type double-float param))
  (case (edge-segment-edge-type edge)
    (#.+edge-type-linear+ (linear-point edge param))
    (#.+edge-type-quadratic+ (quadratic-point edge param))
    (#.+edge-type-cubic+ (cubic-point edge param))))

;;; Direction (tangent) functions

(defun linear-direction (edge param)
  "Get direction (tangent) of linear edge."
  (declare (type edge-segment edge) (ignore param))
  (v2- (edge-segment-p1 edge) (edge-segment-p0 edge)))

(defun quadratic-direction (edge param)
  "Get direction of quadratic Bezier at parameter T."
  (declare (type edge-segment edge) (type double-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (a (v2- p1 p0))
         (b (v2- p2 p1))
         (w (coerce param 'single-float)))
    (v2-mix a b w)))

(defun cubic-direction (edge param)
  "Get direction of cubic Bezier at parameter T."
  (declare (type edge-segment edge) (type double-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (p3 (edge-segment-p3 edge))
         (a (v2- p1 p0))
         (b (v2- p2 p1))
         (w (coerce param 'single-float))
         (c (v2-mix a b w))
         (d (v2-mix b (v2- p3 p2) w))
         (t-vec (v2-mix c d w)))
    ;; Handle degenerate case where tangent is zero
    (if (and (zerop (vec2-x t-vec)) (zerop (vec2-y t-vec)))
        (cond ((= param 0.0d0) (v2- p2 p0))
              ((= param 1.0d0) (v2- p3 p1))
              (t t-vec))
        t-vec)))

(defun edge-direction (edge param)
  "Get direction (tangent) of edge at parameter T."
  (declare (type edge-segment edge) (type double-float param))
  (case (edge-segment-edge-type edge)
    (#.+edge-type-linear+ (linear-direction edge param))
    (#.+edge-type-quadratic+ (quadratic-direction edge param))
    (#.+edge-type-cubic+ (cubic-direction edge param))))

;;; Signed distance functions

(defun linear-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to linear edge.
   Returns (values signed-distance param)."
  (declare (type edge-segment edge) (type vec2 origin))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (aq (v2- origin p0))
         (ab (v2- p1 p0))
         (ab-dot-ab (v2-dot ab ab))
         (param (if (zerop ab-dot-ab) 0.0d0
                    (coerce (/ (v2-dot aq ab) ab-dot-ab) 'double-float)))
         (eq-point (if (> param 0.5d0) p1 p0))
         (eq (v2- eq-point origin))
         (endpoint-distance (coerce (v2-length eq) 'double-float)))
    ;; Check if closest point is on the segment
    (when (and (> param 0.0d0) (< param 1.0d0))
      (let* ((ab-ortho (v2-orthonormal ab nil nil))
             (ortho-dist (coerce (v2-dot ab-ortho aq) 'double-float)))
        (when (< (abs ortho-dist) endpoint-distance)
          (return-from linear-signed-distance
            (values (make-signed-distance ortho-dist 0.0d0) param)))))
    ;; Return endpoint distance
    (let* ((ab-norm (v2-normalize ab))
           (eq-norm (v2-normalize eq))
           (cross-val (coerce (v2-cross aq ab) 'double-float))
           (dist (* (coerce (non-zero-sign cross-val) 'double-float) endpoint-distance))
           (d (abs (coerce (v2-dot ab-norm eq-norm) 'double-float))))
      (values (make-signed-distance dist d) param))))

(defun quadratic-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to quadratic Bezier edge.
   Returns (values signed-distance param)."
  (declare (type edge-segment edge) (type vec2 origin))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (qa (v2- p0 origin))
         (ab (v2- p1 p0))
         ;; br = p0 + p2 - 2*p1
         (br (vec2 (+ (vec2-x p0) (vec2-x p2) (- (vec2-x p1)) (- (vec2-x p1)))
                   (+ (vec2-y p0) (vec2-y p2) (- (vec2-y p1)) (- (vec2-y p1)))))
         ;; Coefficients for cubic: a*t^3 + b*t^2 + c*t + d = 0
         (a (coerce (v2-dot br br) 'double-float))
         (b (* 3.0d0 (coerce (v2-dot ab br) 'double-float)))
         (c (+ (* 2.0d0 (coerce (v2-dot ab ab) 'double-float))
               (coerce (v2-dot qa br) 'double-float)))
         (d (coerce (v2-dot qa ab) 'double-float))
         (solutions (solve-cubic a b c d))
         ;; Distance from endpoint p0
         (cross-val (coerce (v2-cross ab qa) 'double-float))
         (min-distance (* (coerce (non-zero-sign cross-val) 'double-float)
                          (coerce (v2-length qa) 'double-float)))
         (param (- (/ (coerce (v2-dot qa ab) 'double-float)
                      (coerce (v2-dot ab ab) 'double-float)))))
    ;; Check distance from endpoint p2
    (let* ((p2-p1 (v2- p2 p1))
           (p2-origin (v2- p2 origin))
           (cross-p2 (coerce (v2-cross p2-p1 p2-origin) 'double-float))
           (dist-p2 (* (coerce (non-zero-sign cross-p2) 'double-float)
                       (coerce (v2-length p2-origin) 'double-float))))
      (when (< (abs dist-p2) (abs min-distance))
        (setf min-distance dist-p2)
        (let* ((origin-p1 (v2- origin p1))
               (p2-p1-dot (v2-dot p2-p1 p2-p1)))
          (setf param (if (zerop p2-p1-dot) 1.0d0
                          (coerce (/ (v2-dot origin-p1 p2-p1) p2-p1-dot) 'double-float))))))
    ;; Check interior solutions
    (dolist (t-val solutions)
      (when (and (> t-val 0.0d0) (< t-val 1.0d0))
        ;; Point on curve: p0 + 2*t*ab + t^2*br
        (let* ((end-point (vec2 (+ (vec2-x p0)
                                   (* 2.0 (coerce t-val 'single-float) (vec2-x ab))
                                   (* (coerce (* t-val t-val) 'single-float) (vec2-x br)))
                                (+ (vec2-y p0)
                                   (* 2.0 (coerce t-val 'single-float) (vec2-y ab))
                                   (* (coerce (* t-val t-val) 'single-float) (vec2-y br)))))
               (p2-p0 (v2- p2 p0))
               (end-origin (v2- end-point origin))
               (cross-end (coerce (v2-cross p2-p0 end-origin) 'double-float))
               (distance (* (coerce (non-zero-sign cross-end) 'double-float)
                            (coerce (v2-length end-origin) 'double-float))))
          (when (<= (abs distance) (abs min-distance))
            (setf min-distance distance
                  param t-val)))))
    ;; Return result
    (if (and (>= param 0.0d0) (<= param 1.0d0))
        (values (make-signed-distance min-distance 0.0d0) param)
        ;; Compute d value for equidistant comparison
        (let* ((ab-norm (v2-normalize ab))
               (qa-norm (v2-normalize qa))
               (p2-p1 (v2- p2 p1))
               (p2-p1-norm (v2-normalize p2-p1))
               (p2-origin (v2- p2 origin))
               (p2-origin-norm (v2-normalize p2-origin)))
          (if (< param 0.5d0)
              (values (make-signed-distance min-distance
                                            (abs (coerce (v2-dot ab-norm qa-norm) 'double-float)))
                      param)
              (values (make-signed-distance min-distance
                                            (abs (coerce (v2-dot p2-p1-norm p2-origin-norm) 'double-float)))
                      param))))))

(defun cubic-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to cubic Bezier edge.
   Returns (values signed-distance param)."
  (declare (type edge-segment edge) (type vec2 origin))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (p3 (edge-segment-p3 edge))
         (qa (v2- p0 origin))
         (ab (v2- p1 p0))
         ;; br = p2 - p1 - ab = p2 - 2*p1 + p0
         (br (vec2 (- (vec2-x p2) (vec2-x p1) (vec2-x ab))
                   (- (vec2-y p2) (vec2-y p1) (vec2-y ab))))
         ;; as = (p3-p2) - (p2-p1) - br
         (as-vec (vec2 (- (- (vec2-x p3) (vec2-x p2))
                          (- (vec2-x p2) (vec2-x p1))
                          (vec2-x br))
                       (- (- (vec2-y p3) (vec2-y p2))
                          (- (vec2-y p2) (vec2-y p1))
                          (vec2-y br))))
         ;; Direction at t=0
         (ep-dir (edge-direction edge 0.0d0))
         (cross-0 (coerce (v2-cross ep-dir qa) 'double-float))
         (min-distance (* (coerce (non-zero-sign cross-0) 'double-float)
                          (coerce (v2-length qa) 'double-float)))
         (ep-dir-dot (v2-dot ep-dir ep-dir))
         (param (if (zerop ep-dir-dot) 0.0d0
                    (- (/ (coerce (v2-dot qa ep-dir) 'double-float)
                          (coerce ep-dir-dot 'double-float))))))
    ;; Check distance from endpoint p3
    (let* ((p3-origin (v2- p3 origin))
           (ep-dir-1 (edge-direction edge 1.0d0))
           (cross-1 (coerce (v2-cross ep-dir-1 p3-origin) 'double-float))
           (dist-1 (* (coerce (non-zero-sign cross-1) 'double-float)
                      (coerce (v2-length p3-origin) 'double-float))))
      (when (< (abs dist-1) (abs min-distance))
        (setf min-distance dist-1)
        (let* ((a-vec (vec2 (+ (vec2-x origin) (vec2-x ep-dir-1) (- (vec2-x p3)))
                            (+ (vec2-y origin) (vec2-y ep-dir-1) (- (vec2-y p3)))))
               (ep-dir-1-dot (v2-dot ep-dir-1 ep-dir-1)))
          (setf param (if (zerop ep-dir-1-dot) 1.0d0
                          (coerce (/ (v2-dot a-vec ep-dir-1) ep-dir-1-dot) 'double-float))))))
    ;; Newton's method search from multiple starting points
    (let ((search-starts 4))
      (loop for i from 0 to search-starts
            for t-init = (/ (coerce i 'double-float) search-starts)
            do (let ((t-val t-init))
                 (loop for step from 0 to search-starts
                       do (let* ((qpt (edge-point edge t-val))
                                 (qpt-origin (v2- qpt origin))
                                 (dir (edge-direction edge t-val))
                                 (cross-qpt (coerce (v2-cross dir qpt-origin) 'double-float))
                                 (distance (* (coerce (non-zero-sign cross-qpt) 'double-float)
                                              (coerce (v2-length qpt-origin) 'double-float))))
                            (when (< (abs distance) (abs min-distance))
                              (setf min-distance distance
                                    param t-val))
                            (when (= step search-starts)
                              (return))
                            ;; Newton step
                            (let* ((tf (coerce t-val 'single-float))
                                   (d1 (vec2 (+ (* 3.0 (vec2-x as-vec) tf tf)
                                                (* 6.0 (vec2-x br) tf)
                                                (* 3.0 (vec2-x ab)))
                                             (+ (* 3.0 (vec2-y as-vec) tf tf)
                                                (* 6.0 (vec2-y br) tf)
                                                (* 3.0 (vec2-y ab)))))
                                   (d2 (vec2 (+ (* 6.0 (vec2-x as-vec) tf)
                                                (* 6.0 (vec2-x br)))
                                             (+ (* 6.0 (vec2-y as-vec) tf)
                                                (* 6.0 (vec2-y br)))))
                                   (denom (+ (v2-dot d1 d1) (v2-dot qpt-origin d2))))
                              (unless (zerop denom)
                                (setf t-val (- t-val (/ (v2-dot qpt-origin d1) denom))))
                              (when (or (< t-val 0.0d0) (> t-val 1.0d0))
                                (return))))))))
    ;; Return result
    (if (and (>= param 0.0d0) (<= param 1.0d0))
        (values (make-signed-distance min-distance 0.0d0) param)
        ;; Compute d value
        (let* ((d0 (v2-normalize (edge-direction edge 0.0d0)))
               (d1 (v2-normalize (edge-direction edge 1.0d0)))
               (qa-norm (v2-normalize qa))
               (p3-origin (v2- p3 origin))
               (p3-origin-norm (v2-normalize p3-origin)))
          (if (< param 0.5d0)
              (values (make-signed-distance min-distance
                                            (abs (coerce (v2-dot d0 qa-norm) 'double-float)))
                      param)
              (values (make-signed-distance min-distance
                                            (abs (coerce (v2-dot d1 p3-origin-norm) 'double-float)))
                      param))))))

(defun edge-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to edge.
   Returns (values signed-distance param)."
  (declare (type edge-segment edge) (type vec2 origin))
  (case (edge-segment-edge-type edge)
    (#.+edge-type-linear+ (linear-signed-distance edge origin))
    (#.+edge-type-quadratic+ (quadratic-signed-distance edge origin))
    (#.+edge-type-cubic+ (cubic-signed-distance edge origin))))

;;; Pseudo-distance correction

(defun distance-to-pseudo-distance (distance origin param edge)
  "Convert true distance to pseudo-distance at endpoints.
   This improves rendering quality at curve endpoints."
  (declare (type signed-distance distance) (type vec2 origin)
           (type double-float param) (type edge-segment edge))
  (cond
    ;; Before start of edge
    ((< param 0.0d0)
     (let* ((dir (v2-normalize (edge-direction edge 0.0d0)))
            (p (edge-point edge 0.0d0))
            (aq (v2- origin p))
            (ts (coerce (v2-dot aq dir) 'double-float)))
       (when (< ts 0.0d0)
         (let ((pseudo-dist (coerce (v2-cross aq dir) 'double-float)))
           (when (<= (abs pseudo-dist) (abs (signed-distance-dist distance)))
             (setf (signed-distance-dist distance) pseudo-dist
                   (signed-distance-d distance) 0.0d0))))))
    ;; After end of edge
    ((> param 1.0d0)
     (let* ((dir (v2-normalize (edge-direction edge 1.0d0)))
            (p (edge-point edge 1.0d0))
            (bq (v2- origin p))
            (ts (coerce (v2-dot bq dir) 'double-float)))
       (when (> ts 0.0d0)
         (let ((pseudo-dist (coerce (v2-cross bq dir) 'double-float)))
           (when (<= (abs pseudo-dist) (abs (signed-distance-dist distance)))
             (setf (signed-distance-dist distance) pseudo-dist
                   (signed-distance-d distance) 0.0d0)))))))
  distance)

;;; Distance comparison

(defun signed-distance< (a b)
  "Compare two signed distances. Returns true if A is closer than B."
  (declare (type signed-distance a b))
  (or (< (abs (signed-distance-dist a)) (abs (signed-distance-dist b)))
      (and (= (abs (signed-distance-dist a)) (abs (signed-distance-dist b)))
           (< (signed-distance-d a) (signed-distance-d b)))))
