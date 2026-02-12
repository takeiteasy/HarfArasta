;;;; math.lisp
;;;; Vector operations and mathematical utilities

(in-package #:harfarasta)

;;; Constants

(defconstant +pi+ (coerce pi 'double-float))
(defconstant +epsilon+ 1d-14)
(defconstant +infinity+ 1d24)
(defconstant +negative-infinity+ -1d24)

;;; Utility functions

(declaim (inline clamp clampf clamp01 clamp01f))

(defun clamp (value min-val max-val)
  "Clamp VALUE to the range [MIN-VAL, MAX-VAL]."
  (declare (type double-float value min-val max-val))
  (if (< value min-val)
      min-val
      (if (> value max-val)
          max-val
          value)))

(defun clampf (value min-val max-val)
  "Clamp single-float VALUE to the range [MIN-VAL, MAX-VAL]."
  (declare (type single-float value min-val max-val))
  (if (< value min-val)
      min-val
      (if (> value max-val)
          max-val
          value)))

(defun clamp01 (value)
  "Clamp VALUE to [0, 1]."
  (declare (type double-float value))
  (clamp value 0.0d0 1.0d0))

(defun clamp01f (value)
  "Clamp single-float VALUE to [0, 1]."
  (declare (type single-float value))
  (clampf value 0.0 1.0))

(declaim (inline mix mixf))

(defun mix (a b weight)
  "Linear interpolation between A and B by WEIGHT."
  (declare (type double-float a b weight))
  (+ (* (- 1.0d0 weight) a) (* weight b)))

(defun mixf (a b weight)
  "Linear interpolation between single-floats A and B by WEIGHT."
  (declare (type single-float a b weight))
  (+ (* (- 1.0 weight) a) (* weight b)))

(declaim (inline sign non-zero-sign))

(defun sign (n)
  "Return -1, 0, or 1 based on sign of N."
  (declare (type double-float n))
  (cond ((< n 0.0d0) -1)
        ((> n 0.0d0) 1)
        (t 0)))

(defun non-zero-sign (n)
  "Return -1 or 1 based on sign of N (never 0)."
  (declare (type double-float n))
  (if (> n 0.0d0) 1 -1))

(declaim (inline median))

(defun median (a b c)
  "Return the median of three values."
  (declare (type double-float a b c))
  (max (min a b) (min (max a b) c)))

;;; Vec2 operations

(declaim (inline v2+ v2- v2* v2-scale v2-dot v2-cross v2-length v2-length-squared))

(defun v2+ (a b)
  "Add two vec2s."
  (declare (type vec2 a b))
  (vec2 (+ (vec2-x a) (vec2-x b))
        (+ (vec2-y a) (vec2-y b))))

(defun v2- (a b)
  "Subtract vec2 B from A."
  (declare (type vec2 a b))
  (vec2 (- (vec2-x a) (vec2-x b))
        (- (vec2-y a) (vec2-y b))))

(defun v2* (a b)
  "Component-wise multiply two vec2s."
  (declare (type vec2 a b))
  (vec2 (* (vec2-x a) (vec2-x b))
        (* (vec2-y a) (vec2-y b))))

(defun v2-scale (v s)
  "Scale vec2 V by scalar S."
  (declare (type vec2 v) (type single-float s))
  (vec2 (* (vec2-x v) s)
        (* (vec2-y v) s)))

(defun v2-dot (a b)
  "Dot product of two vec2s."
  (declare (type vec2 a b))
  (+ (* (vec2-x a) (vec2-x b))
     (* (vec2-y a) (vec2-y b))))

(defun v2-cross (a b)
  "2D cross product (returns scalar)."
  (declare (type vec2 a b))
  (- (* (vec2-x a) (vec2-y b))
     (* (vec2-y a) (vec2-x b))))

(defun v2-length-squared (v)
  "Squared length of vec2."
  (declare (type vec2 v))
  (+ (* (vec2-x v) (vec2-x v))
     (* (vec2-y v) (vec2-y v))))

(defun v2-length (v)
  "Length of vec2."
  (declare (type vec2 v))
  (sqrt (v2-length-squared v)))

(defun v2-normalize (v)
  "Return normalized vec2 (unit length)."
  (declare (type vec2 v))
  (let ((len (v2-length v)))
    (if (< len 1e-10)
        (vec2 0.0 0.0)
        (let ((inv-len (/ 1.0 len)))
          (vec2 (* (vec2-x v) inv-len)
                (* (vec2-y v) inv-len))))))

(defun v2-mix (a b weight)
  "Linear interpolation between vec2s A and B."
  (declare (type vec2 a b) (type single-float weight))
  (let ((w1 (- 1.0 weight)))
    (vec2 (+ (* w1 (vec2-x a)) (* weight (vec2-x b)))
          (+ (* w1 (vec2-y a)) (* weight (vec2-y b))))))

(defun v2-copy (v)
  "Create a copy of vec2 V."
  (declare (type vec2 v))
  (vec2 (vec2-x v) (vec2-y v)))

(defun v2-equal (a b)
  "Check if two vec2s are equal."
  (declare (type vec2 a b))
  (and (= (vec2-x a) (vec2-x b))
       (= (vec2-y a) (vec2-y b))))

(defun v2-orthonormal (v polarity &optional allow-zero)
  "Get orthonormal vector to V. POLARITY true = counter-clockwise."
  (declare (type vec2 v))
  (let ((len (v2-length v)))
    (if (zerop len)
        (if polarity
            (vec2 0.0 (if allow-zero 0.0 1.0))
            (vec2 0.0 (if allow-zero 0.0 -1.0)))
        (let ((inv-len (/ 1.0 len)))
          (if polarity
              (vec2 (* (- (vec2-y v)) inv-len)
                    (* (vec2-x v) inv-len))
              (vec2 (* (vec2-y v) inv-len)
                    (* (- (vec2-x v)) inv-len)))))))

;;; Vec3 operations

(defun v3-median (v)
  "Return the median of vec3 components."
  (declare (type vec3 v))
  (median (coerce (vec3-x v) 'double-float)
          (coerce (vec3-y v) 'double-float)
          (coerce (vec3-z v) 'double-float)))

;;; Polynomial solvers

(defun solve-quadratic (a b c)
  "Solve ax^2 + bx + c = 0. Returns list of real roots."
  (declare (type double-float a b c))
  (cond
    ;; Degenerate: linear equation
    ((< (abs a) +epsilon+)
     (if (< (abs b) +epsilon+)
         (if (zerop c)
             :infinite ; infinite solutions
             nil)      ; no solutions
         (list (/ (- c) b))))
    ;; Quadratic
    (t
     (let ((discriminant (- (* b b) (* 4.0d0 a c))))
       (cond
         ((> discriminant 0.0d0)
          (let ((sqrt-d (sqrt discriminant))
                (denom (* 2.0d0 a)))
            (list (/ (+ (- b) sqrt-d) denom)
                  (/ (- (- b) sqrt-d) denom))))
         ((zerop discriminant)
          (list (/ (- b) (* 2.0d0 a))))
         (t nil))))))

(defun solve-cubic-normed (a b c)
  "Solve x^3 + ax^2 + bx + c = 0. Returns list of real roots."
  (declare (type double-float a b c))
  (let* ((a2 (* a a))
         (q (/ (- a2 (* 3.0d0 b)) 9.0d0))
         (r (/ (+ (* a (- (* 2.0d0 a2) (* 9.0d0 b))) (* 27.0d0 c)) 54.0d0))
         (r2 (* r r))
         (q3 (* q q q)))
    (if (< r2 q3)
        ;; Three real roots
        (let* ((t-val (/ r (sqrt q3)))
               (t-val (clamp t-val -1.0d0 1.0d0))
               (theta (acos t-val))
               (a/3 (/ a 3.0d0))
               (sqrt-q (* -2.0d0 (sqrt q))))
          (list (- (* sqrt-q (cos (/ theta 3.0d0))) a/3)
                (- (* sqrt-q (cos (/ (+ theta (* 2.0d0 +pi+)) 3.0d0))) a/3)
                (- (* sqrt-q (cos (/ (- theta (* 2.0d0 +pi+)) 3.0d0))) a/3)))
        ;; One or two real roots
        (let* ((aa (- (expt (+ (abs r) (sqrt (- r2 q3))) (/ 1.0d0 3.0d0))))
               (aa (if (< r 0.0d0) (- aa) aa))
               (bb (if (zerop aa) 0.0d0 (/ q aa)))
               (a/3 (/ a 3.0d0)))
          (if (< (abs (* 0.5d0 (sqrt 3.0d0) (- aa bb))) +epsilon+)
              ;; Two real roots (one repeated)
              (list (- (+ aa bb) a/3)
                    (- (* -0.5d0 (+ aa bb)) a/3))
              ;; One real root
              (list (- (+ aa bb) a/3)))))))

(defun solve-cubic (a b c d)
  "Solve ax^3 + bx^2 + cx + d = 0. Returns list of real roots."
  (declare (type double-float a b c d))
  (if (< (abs a) +epsilon+)
      (solve-quadratic b c d)
      (solve-cubic-normed (/ b a) (/ c a) (/ d a))))

;;; Shoelace formula for winding calculation

(defun shoelace (a b)
  "Calculate shoelace term for two points."
  (declare (type vec2 a b))
  (* (- (vec2-x b) (vec2-x a))
     (+ (vec2-y a) (vec2-y b))))
