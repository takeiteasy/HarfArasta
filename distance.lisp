;;;; distance.lisp
;;;; Signed distance calculations for Bezier curves
;;;; Based on msdf.h/msdf.c by Viktor Chlumský

(in-package #:harfarasta)

;;; Point on curve functions (vec2-returning, used by MSDF and public API)

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

;;; Scalar point-on-curve functions returning (values x y) as single-floats
;;; These avoid vec2 allocation in the hot SDF loop.

(declaim (inline linear-point-xy quadratic-point-xy cubic-point-xy edge-point-xy))

(defun linear-point-xy (edge param)
  "Get point on linear edge at parameter T. Returns (values x y)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type single-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (w param)
         (w1 (- 1.0 w)))
    (values (+ (* w1 (vec2-x p0)) (* w (vec2-x p1)))
            (+ (* w1 (vec2-y p0)) (* w (vec2-y p1))))))

(defun quadratic-point-xy (edge param)
  "Get point on quadratic Bezier at parameter T. Returns (values x y)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type single-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (w param)
         (w1 (- 1.0 w))
         ;; First level: mix(p0,p1,w) and mix(p1,p2,w)
         (ax (+ (* w1 (vec2-x p0)) (* w (vec2-x p1))))
         (ay (+ (* w1 (vec2-y p0)) (* w (vec2-y p1))))
         (bx (+ (* w1 (vec2-x p1)) (* w (vec2-x p2))))
         (by (+ (* w1 (vec2-y p1)) (* w (vec2-y p2)))))
    ;; Second level: mix(a,b,w)
    (values (+ (* w1 ax) (* w bx))
            (+ (* w1 ay) (* w by)))))

(defun cubic-point-xy (edge param)
  "Get point on cubic Bezier at parameter T. Returns (values x y)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type single-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (p3 (edge-segment-p3 edge))
         (w param)
         (w1 (- 1.0 w))
         ;; p12 = mix(p1,p2,w)
         (p12x (+ (* w1 (vec2-x p1)) (* w (vec2-x p2))))
         (p12y (+ (* w1 (vec2-y p1)) (* w (vec2-y p2))))
         ;; a = mix(p0,p1,w)
         (ax (+ (* w1 (vec2-x p0)) (* w (vec2-x p1))))
         (ay (+ (* w1 (vec2-y p0)) (* w (vec2-y p1))))
         ;; b = mix(a,p12,w)
         (bx (+ (* w1 ax) (* w p12x)))
         (by (+ (* w1 ay) (* w p12y)))
         ;; c = mix(p2,p3,w)
         (cx (+ (* w1 (vec2-x p2)) (* w (vec2-x p3))))
         (cy (+ (* w1 (vec2-y p2)) (* w (vec2-y p3))))
         ;; d = mix(p12,c,w)
         (dx (+ (* w1 p12x) (* w cx)))
         (dy (+ (* w1 p12y) (* w cy))))
    ;; result = mix(b,d,w)
    (values (+ (* w1 bx) (* w dx))
            (+ (* w1 by) (* w dy)))))

(defun edge-point-xy (edge param)
  "Get point on edge at parameter T. Returns (values x y) as single-floats."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type single-float param))
  (case (edge-segment-edge-type edge)
    (#.+edge-type-linear+ (linear-point-xy edge param))
    (#.+edge-type-quadratic+ (quadratic-point-xy edge param))
    (#.+edge-type-cubic+ (cubic-point-xy edge param))))

;;; Direction (tangent) functions (vec2-returning, used by MSDF)

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

;;; Scalar direction functions returning (values dx dy) as single-floats

(declaim (inline linear-direction-xy quadratic-direction-xy cubic-direction-xy edge-direction-xy))

(defun linear-direction-xy (edge param)
  "Get direction of linear edge. Returns (values dx dy)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (ignore param))
  (let ((p0 (edge-segment-p0 edge))
        (p1 (edge-segment-p1 edge)))
    (values (- (vec2-x p1) (vec2-x p0))
            (- (vec2-y p1) (vec2-y p0)))))

(defun quadratic-direction-xy (edge param)
  "Get direction of quadratic Bezier at parameter T. Returns (values dx dy)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type single-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (w param)
         (w1 (- 1.0 w))
         ;; a = p1 - p0, b = p2 - p1
         (ax (- (vec2-x p1) (vec2-x p0)))
         (ay (- (vec2-y p1) (vec2-y p0)))
         (bx (- (vec2-x p2) (vec2-x p1)))
         (by (- (vec2-y p2) (vec2-y p1))))
    (values (+ (* w1 ax) (* w bx))
            (+ (* w1 ay) (* w by)))))

(defun cubic-direction-xy (edge param)
  "Get direction of cubic Bezier at parameter T. Returns (values dx dy)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type single-float param))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (p3 (edge-segment-p3 edge))
         (w param)
         (w1 (- 1.0 w))
         ;; a = p1-p0, b = p2-p1
         (ax (- (vec2-x p1) (vec2-x p0)))
         (ay (- (vec2-y p1) (vec2-y p0)))
         (bx (- (vec2-x p2) (vec2-x p1)))
         (by (- (vec2-y p2) (vec2-y p1)))
         ;; c = mix(a,b,w)
         (cx (+ (* w1 ax) (* w bx)))
         (cy (+ (* w1 ay) (* w by)))
         ;; p3-p2
         (p3p2x (- (vec2-x p3) (vec2-x p2)))
         (p3p2y (- (vec2-y p3) (vec2-y p2)))
         ;; d = mix(b, p3-p2, w)
         (dx (+ (* w1 bx) (* w p3p2x)))
         (dy (+ (* w1 by) (* w p3p2y)))
         ;; t-vec = mix(c,d,w)
         (tx (+ (* w1 cx) (* w dx)))
         (ty (+ (* w1 cy) (* w dy))))
    ;; Handle degenerate case where tangent is zero
    (if (and (zerop tx) (zerop ty))
        (cond ((= param 0.0)
               (values (- (vec2-x p2) (vec2-x p0))
                       (- (vec2-y p2) (vec2-y p0))))
              ((= param 1.0)
               (values (- (vec2-x p3) (vec2-x p1))
                       (- (vec2-y p3) (vec2-y p1))))
              (t (values tx ty)))
        (values tx ty))))

(defun edge-direction-xy (edge param)
  "Get direction of edge at parameter T. Returns (values dx dy)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type single-float param))
  (case (edge-segment-edge-type edge)
    (#.+edge-type-linear+ (linear-direction-xy edge param))
    (#.+edge-type-quadratic+ (quadratic-direction-xy edge param))
    (#.+edge-type-cubic+ (cubic-direction-xy edge param))))

;;; Signed distance functions - scalar math, no vec2 allocation
;;; Each returns (values signed-distance param) for compatibility with MSDF code.

(defun linear-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to linear edge.
   Returns (values signed-distance param)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type vec2 origin))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (ox (vec2-x origin)) (oy (vec2-y origin))
         (p0x (vec2-x p0)) (p0y (vec2-y p0))
         (p1x (vec2-x p1)) (p1y (vec2-y p1))
         ;; aq = origin - p0
         (aqx (- ox p0x)) (aqy (- oy p0y))
         ;; ab = p1 - p0
         (abx (- p1x p0x)) (aby (- p1y p0y))
         ;; dot products
         (ab-dot-ab (+ (* abx abx) (* aby aby)))
         (param (if (zerop ab-dot-ab) 0.0d0
                    (coerce (/ (+ (* aqx abx) (* aqy aby)) ab-dot-ab) 'double-float)))
         ;; eq = nearest-endpoint - origin
         (eqx (if (> param 0.5d0) (- p1x ox) (- p0x ox)))
         (eqy (if (> param 0.5d0) (- p1y oy) (- p0y oy)))
         (endpoint-distance (coerce (sqrt (+ (* eqx eqx) (* eqy eqy))) 'double-float)))
    ;; Check if closest point is on the segment
    (when (and (> param 0.0d0) (< param 1.0d0))
      ;; Compute orthonormal to ab
      (let* ((ab-len (sqrt (+ (* abx abx) (* aby aby)))))
        (unless (zerop ab-len)
          (let* ((inv-len (/ 1.0 ab-len))
                 ;; orthonormal (polarity=nil → clockwise): (y/len, -x/len)
                 (ortho-x (* aby inv-len))
                 (ortho-y (* (- abx) inv-len))
                 (ortho-dist (coerce (+ (* ortho-x aqx) (* ortho-y aqy)) 'double-float)))
            (when (< (abs ortho-dist) endpoint-distance)
              (return-from linear-signed-distance
                (values (make-signed-distance ortho-dist 0.0d0) param)))))))
    ;; Return endpoint distance
    (let* ((ab-len (sqrt (+ (* abx abx) (* aby aby))))
           (eq-len (sqrt (+ (* eqx eqx) (* eqy eqy))))
           ;; normalize ab
           (ab-nx (if (< ab-len 1e-10) 0.0 (/ abx ab-len)))
           (ab-ny (if (< ab-len 1e-10) 0.0 (/ aby ab-len)))
           ;; normalize eq
           (eq-nx (if (< eq-len 1e-10) 0.0 (/ eqx eq-len)))
           (eq-ny (if (< eq-len 1e-10) 0.0 (/ eqy eq-len)))
           ;; cross(aq, ab)
           (cross-val (coerce (- (* aqx aby) (* aqy abx)) 'double-float))
           (dist (* (coerce (non-zero-sign cross-val) 'double-float) endpoint-distance))
           ;; dot(ab-norm, eq-norm)
           (d (abs (coerce (+ (* ab-nx eq-nx) (* ab-ny eq-ny)) 'double-float))))
      (values (make-signed-distance dist d) param))))

(defun quadratic-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to quadratic Bezier edge.
   Returns (values signed-distance param)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type vec2 origin))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (ox (vec2-x origin)) (oy (vec2-y origin))
         (p0x (vec2-x p0)) (p0y (vec2-y p0))
         (p1x (vec2-x p1)) (p1y (vec2-y p1))
         (p2x (vec2-x p2)) (p2y (vec2-y p2))
         ;; qa = p0 - origin
         (qax (- p0x ox)) (qay (- p0y oy))
         ;; ab = p1 - p0
         (abx (- p1x p0x)) (aby (- p1y p0y))
         ;; br = p0 + p2 - 2*p1
         (brx (+ p0x p2x (- p1x) (- p1x)))
         (bry (+ p0y p2y (- p1y) (- p1y)))
         ;; Coefficients for cubic: a*t^3 + b*t^2 + c*t + d = 0
         (a (coerce (+ (* brx brx) (* bry bry)) 'double-float))
         (b (* 3.0d0 (coerce (+ (* abx brx) (* aby bry)) 'double-float)))
         (c (+ (* 2.0d0 (coerce (+ (* abx abx) (* aby aby)) 'double-float))
               (coerce (+ (* qax brx) (* qay bry)) 'double-float)))
         (d (coerce (+ (* qax abx) (* qay aby)) 'double-float))
         (solutions (solve-cubic a b c d))
         ;; Distance from endpoint p0
         (cross-val (coerce (- (* abx qay) (* aby qax)) 'double-float))
         (qa-len (coerce (sqrt (+ (* qax qax) (* qay qay))) 'double-float))
         (min-distance (* (coerce (non-zero-sign cross-val) 'double-float) qa-len))
         (ab-dot-ab (+ (* abx abx) (* aby aby)))
         (param (if (zerop ab-dot-ab) 0.0d0
                    (- (/ (coerce (+ (* qax abx) (* qay aby)) 'double-float)
                          (coerce ab-dot-ab 'double-float))))))
    ;; Check distance from endpoint p2
    (let* ((p2p1x (- p2x p1x)) (p2p1y (- p2y p1y))
           (p2ox (- p2x ox)) (p2oy (- p2y oy))
           (cross-p2 (coerce (- (* p2p1x p2oy) (* p2p1y p2ox)) 'double-float))
           (p2o-len (coerce (sqrt (+ (* p2ox p2ox) (* p2oy p2oy))) 'double-float))
           (dist-p2 (* (coerce (non-zero-sign cross-p2) 'double-float) p2o-len)))
      (when (< (abs dist-p2) (abs min-distance))
        (setf min-distance dist-p2)
        (let* ((op1x (- ox p1x)) (op1y (- oy p1y))
               (p2p1-dot (+ (* p2p1x p2p1x) (* p2p1y p2p1y))))
          (setf param (if (zerop p2p1-dot) 1.0d0
                          (coerce (/ (+ (* op1x p2p1x) (* op1y p2p1y)) p2p1-dot) 'double-float))))))
    ;; Check interior solutions
    (dolist (t-val solutions)
      (when (and (> t-val 0.0d0) (< t-val 1.0d0))
        (let* ((tf (coerce t-val 'single-float))
               ;; Point on curve: p0 + 2*t*ab + t^2*br
               (epx (+ p0x (* 2.0 tf abx) (* (* tf tf) brx)))
               (epy (+ p0y (* 2.0 tf aby) (* (* tf tf) bry)))
               ;; end-origin
               (eox (- epx ox)) (eoy (- epy oy))
               ;; Tangent at t: ab + t*br (factor of 2 doesn't affect cross sign)
               (tanx (+ abx (* tf brx)))
               (tany (+ aby (* tf bry)))
               ;; cross(tangent, end-origin)
               (cross-end (coerce (- (* tanx eoy) (* tany eox)) 'double-float))
               (eo-len (coerce (sqrt (+ (* eox eox) (* eoy eoy))) 'double-float))
               (distance (* (coerce (non-zero-sign cross-end) 'double-float) eo-len)))
          (when (<= (abs distance) (abs min-distance))
            (setf min-distance distance
                  param t-val)))))
    ;; Return result
    (if (and (>= param 0.0d0) (<= param 1.0d0))
        (values (make-signed-distance min-distance 0.0d0) param)
        ;; Compute d value for equidistant comparison
        (let* (;; normalize ab
               (ab-len (sqrt (+ (* abx abx) (* aby aby))))
               (ab-nx (if (< ab-len 1e-10) 0.0 (/ abx ab-len)))
               (ab-ny (if (< ab-len 1e-10) 0.0 (/ aby ab-len)))
               ;; normalize qa
               (qa-len-f (sqrt (+ (* qax qax) (* qay qay))))
               (qa-nx (if (< qa-len-f 1e-10) 0.0 (/ qax qa-len-f)))
               (qa-ny (if (< qa-len-f 1e-10) 0.0 (/ qay qa-len-f)))
               ;; p2-p1 normalized
               (p2p1x (- p2x p1x)) (p2p1y (- p2y p1y))
               (p2p1-len (sqrt (+ (* p2p1x p2p1x) (* p2p1y p2p1y))))
               (p2p1-nx (if (< p2p1-len 1e-10) 0.0 (/ p2p1x p2p1-len)))
               (p2p1-ny (if (< p2p1-len 1e-10) 0.0 (/ p2p1y p2p1-len)))
               ;; p2-origin normalized
               (p2ox (- p2x ox)) (p2oy (- p2y oy))
               (p2o-len (sqrt (+ (* p2ox p2ox) (* p2oy p2oy))))
               (p2o-nx (if (< p2o-len 1e-10) 0.0 (/ p2ox p2o-len)))
               (p2o-ny (if (< p2o-len 1e-10) 0.0 (/ p2oy p2o-len))))
          (if (< param 0.5d0)
              (values (make-signed-distance min-distance
                                            (abs (coerce (+ (* ab-nx qa-nx) (* ab-ny qa-ny)) 'double-float)))
                      param)
              (values (make-signed-distance min-distance
                                            (abs (coerce (+ (* p2p1-nx p2o-nx) (* p2p1-ny p2o-ny)) 'double-float)))
                      param))))))

(defun cubic-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to cubic Bezier edge.
   Returns (values signed-distance param)."
  (declare (optimize (speed 3) (safety 0))
           (type edge-segment edge) (type vec2 origin))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (p3 (edge-segment-p3 edge))
         (ox (vec2-x origin)) (oy (vec2-y origin))
         (p0x (vec2-x p0)) (p0y (vec2-y p0))
         (p1x (vec2-x p1)) (p1y (vec2-y p1))
         (p2x (vec2-x p2)) (p2y (vec2-y p2))
         (p3x (vec2-x p3)) (p3y (vec2-y p3))
         ;; qa = p0 - origin
         (qax (- p0x ox)) (qay (- p0y oy))
         ;; ab = p1 - p0
         (abx (- p1x p0x)) (aby (- p1y p0y))
         ;; br = p2 - p1 - ab = p2 - 2*p1 + p0
         (brx (- p2x p1x abx)) (bry (- p2y p1y aby))
         ;; as = (p3-p2) - (p2-p1) - br
         (asx (- (- p3x p2x) (- p2x p1x) brx))
         (asy (- (- p3y p2y) (- p2y p1y) bry))
         ;; qa-len
         (qa-len (sqrt (+ (* qax qax) (* qay qay)))))
    ;; Get direction at t=0 using scalar version
    (multiple-value-bind (ep-dir-x ep-dir-y) (edge-direction-xy edge 0.0)
    (let* ((cross-0 (coerce (- (* ep-dir-x qay) (* ep-dir-y qax)) 'double-float))
           (min-distance (* (coerce (non-zero-sign cross-0) 'double-float)
                            (coerce qa-len 'double-float)))
           (ep-dir-dot (+ (* ep-dir-x ep-dir-x) (* ep-dir-y ep-dir-y)))
           (param (if (zerop ep-dir-dot) 0.0d0
                      (- (/ (coerce (+ (* qax ep-dir-x) (* qay ep-dir-y)) 'double-float)
                            (coerce ep-dir-dot 'double-float))))))
      ;; Check distance from endpoint p3
      (let ((p3ox (- p3x ox)) (p3oy (- p3y oy)))
        (multiple-value-bind (ed1x ed1y) (edge-direction-xy edge 1.0)
          (let* ((cross-1 (coerce (- (* ed1x p3oy) (* ed1y p3ox)) 'double-float))
                 (p3o-len (coerce (sqrt (+ (* p3ox p3ox) (* p3oy p3oy))) 'double-float))
                 (dist-1 (* (coerce (non-zero-sign cross-1) 'double-float) p3o-len)))
            (when (< (abs dist-1) (abs min-distance))
              (setf min-distance dist-1)
              ;; a-vec = origin + ep-dir-1 - p3
              (let* ((avx (+ ox ed1x (- p3x)))
                     (avy (+ oy ed1y (- p3y)))
                     (ed1-dot (+ (* ed1x ed1x) (* ed1y ed1y))))
                (setf param (if (zerop ed1-dot) 1.0d0
                                (coerce (/ (+ (* avx ed1x) (* avy ed1y)) ed1-dot) 'double-float))))))))
      ;; Newton's method search from multiple starting points
      (let ((search-starts 4))
        (loop for i from 0 to search-starts
              for t-init = (/ (coerce i 'double-float) search-starts)
              do (let ((t-val t-init))
                   (loop for step from 0 to search-starts
                         do (let* ((tf (coerce t-val 'single-float)))
                              ;; Get point and direction on curve (scalar)
                              (multiple-value-bind (qptx qpty) (edge-point-xy edge tf)
                                (let* ((qox (- qptx ox)) (qoy (- qpty oy)))
                                  (multiple-value-bind (dirx diry) (edge-direction-xy edge tf)
                                    (let* ((cross-qpt (coerce (- (* dirx qoy) (* diry qox)) 'double-float))
                                           (qo-len (coerce (sqrt (+ (* qox qox) (* qoy qoy))) 'double-float))
                                           (distance (* (coerce (non-zero-sign cross-qpt) 'double-float) qo-len)))
                                      (when (< (abs distance) (abs min-distance))
                                        (setf min-distance distance
                                              param t-val))
                                      (when (= step search-starts)
                                        (return))
                                      ;; Newton step - d1 and d2 as scalar pairs
                                      (let* ((d1x (+ (* 3.0 asx tf tf)
                                                     (* 6.0 brx tf)
                                                     (* 3.0 abx)))
                                             (d1y (+ (* 3.0 asy tf tf)
                                                     (* 6.0 bry tf)
                                                     (* 3.0 aby)))
                                             (d2x (+ (* 6.0 asx tf)
                                                     (* 6.0 brx)))
                                             (d2y (+ (* 6.0 asy tf)
                                                     (* 6.0 bry)))
                                             ;; denom = dot(d1,d1) + dot(qpt-origin, d2)
                                             (denom (+ (+ (* d1x d1x) (* d1y d1y))
                                                       (+ (* qox d2x) (* qoy d2y)))))
                                        (unless (zerop denom)
                                          (setf t-val (- t-val (/ (+ (* qox d1x) (* qoy d1y)) denom))))
                                        (when (or (< t-val 0.0d0) (> t-val 1.0d0))
                                          (return)))))))))))))
      ;; Return result
      (if (and (>= param 0.0d0) (<= param 1.0d0))
          (values (make-signed-distance min-distance 0.0d0) param)
          ;; Compute d value
          (multiple-value-bind (d0x d0y) (edge-direction-xy edge 0.0)
            (multiple-value-bind (d1x d1y) (edge-direction-xy edge 1.0)
              ;; normalize d0
              (let* ((d0-len (sqrt (+ (* d0x d0x) (* d0y d0y))))
                     (d0-nx (if (< d0-len 1e-10) 0.0 (/ d0x d0-len)))
                     (d0-ny (if (< d0-len 1e-10) 0.0 (/ d0y d0-len)))
                     ;; normalize d1
                     (d1-len (sqrt (+ (* d1x d1x) (* d1y d1y))))
                     (d1-nx (if (< d1-len 1e-10) 0.0 (/ d1x d1-len)))
                     (d1-ny (if (< d1-len 1e-10) 0.0 (/ d1y d1-len)))
                     ;; normalize qa
                     (qa-len-f (sqrt (+ (* qax qax) (* qay qay))))
                     (qa-nx (if (< qa-len-f 1e-10) 0.0 (/ qax qa-len-f)))
                     (qa-ny (if (< qa-len-f 1e-10) 0.0 (/ qay qa-len-f)))
                     ;; normalize p3-origin
                     (p3ox (- p3x ox)) (p3oy (- p3y oy))
                     (p3o-len (sqrt (+ (* p3ox p3ox) (* p3oy p3oy))))
                     (p3o-nx (if (< p3o-len 1e-10) 0.0 (/ p3ox p3o-len)))
                     (p3o-ny (if (< p3o-len 1e-10) 0.0 (/ p3oy p3o-len))))
                (if (< param 0.5d0)
                    (values (make-signed-distance min-distance
                                                  (abs (coerce (+ (* d0-nx qa-nx) (* d0-ny qa-ny)) 'double-float)))
                            param)
                    (values (make-signed-distance min-distance
                                                  (abs (coerce (+ (* d1-nx p3o-nx) (* d1-ny p3o-ny)) 'double-float)))
                            param)))))))))

(defun edge-signed-distance (edge origin)
  "Calculate signed distance from ORIGIN to edge.
   Returns (values signed-distance param)."
  (declare (type edge-segment edge) (type vec2 origin))
  (case (edge-segment-edge-type edge)
    (#.+edge-type-linear+ (linear-signed-distance edge origin))
    (#.+edge-type-quadratic+ (quadratic-signed-distance edge origin))
    (#.+edge-type-cubic+ (cubic-signed-distance edge origin))))

;;; Pseudo-distance correction - scalar math version

(defun distance-to-pseudo-distance (distance origin param edge)
  "Convert true distance to pseudo-distance at endpoints.
   This improves rendering quality at curve endpoints."
  (declare (optimize (speed 3) (safety 0))
           (type signed-distance distance) (type vec2 origin)
           (type double-float param) (type edge-segment edge))
  (let ((ox (vec2-x origin)) (oy (vec2-y origin)))
    (cond
      ;; Before start of edge
      ((< param 0.0d0)
       (multiple-value-bind (dirx diry) (edge-direction-xy edge 0.0)
         (multiple-value-bind (px py) (edge-point-xy edge 0.0)
           ;; normalize dir
           (let* ((dir-len (sqrt (+ (* dirx dirx) (* diry diry)))))
             (unless (< dir-len 1e-10)
               (let* ((inv-len (/ 1.0 dir-len))
                      (ndx (* dirx inv-len)) (ndy (* diry inv-len))
                      ;; aq = origin - p
                      (aqx (- ox px)) (aqy (- oy py))
                      (ts (coerce (+ (* aqx ndx) (* aqy ndy)) 'double-float)))
                 (when (< ts 0.0d0)
                   (let ((pseudo-dist (coerce (- (* aqx ndy) (* aqy ndx)) 'double-float)))
                     (when (<= (abs pseudo-dist) (abs (signed-distance-dist distance)))
                       (setf (signed-distance-dist distance) pseudo-dist
                             (signed-distance-d distance) 0.0d0))))))))))
      ;; After end of edge
      ((> param 1.0d0)
       (multiple-value-bind (dirx diry) (edge-direction-xy edge 1.0)
         (multiple-value-bind (px py) (edge-point-xy edge 1.0)
           ;; normalize dir
           (let* ((dir-len (sqrt (+ (* dirx dirx) (* diry diry)))))
             (unless (< dir-len 1e-10)
               (let* ((inv-len (/ 1.0 dir-len))
                      (ndx (* dirx inv-len)) (ndy (* diry inv-len))
                      ;; bq = origin - p
                      (bqx (- ox px)) (bqy (- oy py))
                      (ts (coerce (+ (* bqx ndx) (* bqy ndy)) 'double-float)))
                 (when (> ts 0.0d0)
                   (let ((pseudo-dist (coerce (- (* bqx ndy) (* bqy ndx)) 'double-float)))
                     (when (<= (abs pseudo-dist) (abs (signed-distance-dist distance)))
                       (setf (signed-distance-dist distance) pseudo-dist
                             (signed-distance-d distance) 0.0d0))))))))))))
  distance)

;;; Distance comparison

(declaim (inline signed-distance<))

(defun signed-distance< (a b)
  "Compare two signed distances. Returns true if A is closer than B."
  (declare (type signed-distance a b))
  (let ((abs-a (abs (signed-distance-dist a)))
        (abs-b (abs (signed-distance-dist b))))
    (or (< abs-a abs-b)
        (and (= abs-a abs-b)
             (< (signed-distance-d a) (signed-distance-d b))))))
