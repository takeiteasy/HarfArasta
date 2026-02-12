;;;; edge-coloring.lisp
;;;; Edge coloring algorithm for MSDF generation
;;;; Based on msdf.h/msdf.c by Viktor Chlumský

(in-package #:harfarasta)

;;; Corner detection

(defun corner-p (a b threshold)
  "Check if two direction vectors form a corner.
   Returns true if the angle between them is sharp enough."
  (declare (type vec2 a b) (type double-float threshold))
  (or (<= (v2-dot a b) 0.0)
      (> (abs (v2-cross a b)) threshold)))

;;; Color switching

(defun switch-color (color seed banned)
  "Switch to a new color avoiding BANNED colors.
   Returns (values new-color new-seed)."
  (declare (type fixnum color banned) (type (unsigned-byte 64) seed))
  (let ((combined (logand color banned)))
    (cond
      ;; If combined is a single primary color, switch to complement
      ((or (= combined +edge-color-red+)
           (= combined +edge-color-green+)
           (= combined +edge-color-blue+))
       (values (logxor combined +edge-color-white+) seed))
      ;; If starting from black or white, pick a starting color
      ((or (= color +edge-color-black+) (= color +edge-color-white+))
       (let ((start-colors (vector +edge-color-cyan+ +edge-color-magenta+ +edge-color-yellow+)))
         (values (aref start-colors (mod seed 3))
                 (floor seed 3))))
      ;; Otherwise rotate color
      (t
       (let* ((shift (1+ (logand seed 1)))
              (shifted (ash color shift))
              (new-color (logand (logior shifted (ash shifted -3)) +edge-color-white+)))
         (values new-color (ash seed -1)))))))

;;; Edge splitting

(defun split-linear-edge (edge)
  "Split a linear edge into three parts."
  (declare (type edge-segment edge))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (color (edge-segment-color edge))
         (t1 (v2-mix p0 p1 (/ 1.0 3.0)))
         (t2 (v2-mix p0 p1 (/ 2.0 3.0))))
    (list (make-edge-segment +edge-type-linear+ (v2-copy p0) (v2-copy t1))
          (make-edge-segment +edge-type-linear+ (v2-copy t1) (v2-copy t2))
          (make-edge-segment +edge-type-linear+ (v2-copy t2) (v2-copy p1)))))

(defun split-quadratic-edge (edge)
  "Split a quadratic edge into three parts using de Casteljau subdivision."
  (declare (type edge-segment edge))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         ;; Points at t=1/3
         (q0-1 (v2-mix p0 p1 (/ 1.0 3.0)))
         (q1-1 (quadratic-point edge (/ 1.0d0 3.0d0)))
         ;; Points at t=2/3
         (q0-2 (quadratic-point edge (/ 1.0d0 3.0d0)))
         (q2-2 (quadratic-point edge (/ 2.0d0 3.0d0)))
         ;; Middle control point for segment 2
         (a (v2-mix p0 p1 (/ 5.0 9.0)))
         (b (v2-mix p1 p2 (/ 4.0 9.0)))
         (mid-ctrl (v2-mix a b 0.5))
         ;; Control point for segment 3
         (q1-3 (v2-mix p1 p2 (/ 2.0 3.0))))
    (list (make-edge-segment +edge-type-quadratic+
                             (v2-copy p0)
                             (v2-copy q0-1)
                             (v2-copy q1-1))
          (make-edge-segment +edge-type-quadratic+
                             (v2-copy q0-2)
                             (v2-copy mid-ctrl)
                             (v2-copy q2-2))
          (make-edge-segment +edge-type-quadratic+
                             (v2-copy q2-2)
                             (v2-copy q1-3)
                             (v2-copy p2)))))

(defun split-cubic-edge (edge)
  "Split a cubic edge into three parts using de Casteljau subdivision."
  (declare (type edge-segment edge))
  (let* ((p0 (edge-segment-p0 edge))
         (p1 (edge-segment-p1 edge))
         (p2 (edge-segment-p2 edge))
         (p3 (edge-segment-p3 edge))
         ;; Segment 1: t in [0, 1/3]
         (s1-p0 p0)
         (s1-p1 (if (v2-equal p0 p1) p0 (v2-mix p0 p1 (/ 1.0 3.0))))
         (a1 (v2-mix p0 p1 (/ 1.0 3.0)))
         (b1 (v2-mix p1 p2 (/ 1.0 3.0)))
         (s1-p2 (v2-mix a1 b1 (/ 1.0 3.0)))
         (s1-p3 (cubic-point edge (/ 1.0d0 3.0d0)))
         ;; Segment 2: t in [1/3, 2/3]
         (s2-p0 (cubic-point edge (/ 1.0d0 3.0d0)))
         ;; Control points for segment 2 (complex de Casteljau)
         (c1 (v2-mix a1 b1 (/ 1.0 3.0)))
         (a2 (v2-mix p1 p2 (/ 1.0 3.0)))
         (b2 (v2-mix p2 p3 (/ 1.0 3.0)))
         (d1 (v2-mix a2 b2 (/ 1.0 3.0)))
         (s2-p1 (v2-mix c1 d1 (/ 2.0 3.0)))
         (c2 (v2-mix (v2-mix p0 p1 (/ 2.0 3.0))
                     (v2-mix p1 p2 (/ 2.0 3.0))
                     (/ 2.0 3.0)))
         (d2 (v2-mix (v2-mix p1 p2 (/ 2.0 3.0))
                     (v2-mix p2 p3 (/ 2.0 3.0))
                     (/ 2.0 3.0)))
         (s2-p2 (v2-mix c2 d2 (/ 1.0 3.0)))
         (s2-p3 (cubic-point edge (/ 2.0d0 3.0d0)))
         ;; Segment 3: t in [2/3, 1]
         (s3-p0 (cubic-point edge (/ 2.0d0 3.0d0)))
         (a3 (v2-mix p1 p2 (/ 2.0 3.0)))
         (b3 (v2-mix p2 p3 (/ 2.0 3.0)))
         (s3-p1 (v2-mix a3 b3 (/ 2.0 3.0)))
         (s3-p2 (if (v2-equal p2 p3) p3 (v2-mix p2 p3 (/ 2.0 3.0))))
         (s3-p3 p3))
    (list (make-edge-segment +edge-type-cubic+
                             (v2-copy s1-p0) (v2-copy s1-p1)
                             (v2-copy s1-p2) (v2-copy s1-p3))
          (make-edge-segment +edge-type-cubic+
                             (v2-copy s2-p0) (v2-copy s2-p1)
                             (v2-copy s2-p2) (v2-copy s2-p3))
          (make-edge-segment +edge-type-cubic+
                             (v2-copy s3-p0) (v2-copy s3-p1)
                             (v2-copy s3-p2) (v2-copy s3-p3)))))

(defun split-edge (edge)
  "Split an edge into three parts."
  (declare (type edge-segment edge))
  (let ((parts (case (edge-segment-edge-type edge)
                 (#.+edge-type-linear+ (split-linear-edge edge))
                 (#.+edge-type-quadratic+ (split-quadratic-edge edge))
                 (#.+edge-type-cubic+ (split-cubic-edge edge)))))
    ;; Copy edge type to split parts
    (dolist (part parts)
      (setf (edge-segment-edge-type part) (edge-segment-edge-type edge)
            (edge-segment-color part) (edge-segment-color edge)))
    parts))

;;; Winding number calculation

(defun calculate-contour-winding (contour)
  "Calculate the winding number of a contour using the shoelace formula.
   Returns 1 for counter-clockwise, -1 for clockwise, 0 for degenerate."
  (declare (type contour contour))
  (let ((edges (contour-edges contour))
        (total 0.0d0))
    (cond
      ((null edges) 0)
      ((= (length edges) 1)
       ;; Single edge - sample at three points
       (let* ((edge (first edges))
              (a (edge-point edge 0.0d0))
              (b (edge-point edge (/ 1.0d0 3.0d0)))
              (c (edge-point edge (/ 2.0d0 3.0d0))))
         (incf total (shoelace a b))
         (incf total (shoelace b c))
         (incf total (shoelace c a))))
      ((= (length edges) 2)
       ;; Two edges - sample at four points
       (let* ((e0 (first edges))
              (e1 (second edges))
              (a (edge-point e0 0.0d0))
              (b (edge-point e0 0.5d0))
              (c (edge-point e1 0.0d0))
              (d (edge-point e1 0.5d0)))
         (incf total (shoelace a b))
         (incf total (shoelace b c))
         (incf total (shoelace c d))
         (incf total (shoelace d a))))
      (t
       ;; Multiple edges - use start points
       (let ((prev (edge-point (car (last edges)) 0.0d0)))
         (dolist (edge edges)
           (let ((cur (edge-point edge 0.0d0)))
             (incf total (shoelace prev cur))
             (setf prev cur))))))
    ;; Return sign of total
    (cond ((< total 0.0d0) -1)
          ((> total 0.0d0) 1)
          (t 0))))

;;; Edge coloring algorithm

(defun color-contour-edges (contour seed)
  "Color edges in a contour for MSDF generation.
   Returns the new seed value."
  (declare (type contour contour) (type (unsigned-byte 64) seed))
  (let* ((edges (contour-edges contour))
         (edge-count (length edges))
         (angle-threshold 3.0d0)
         (cross-threshold (sin angle-threshold))
         (corners nil))

    (when (zerop edge-count)
      (return-from color-contour-edges seed))

    ;; Find corners
    (let ((prev-dir (edge-direction (car (last edges)) 1.0d0)))
      (loop for i from 0
            for edge in edges
            for dir = (edge-direction edge 0.0d0)
            for dir-norm = (v2-normalize dir)
            for prev-norm = (v2-normalize prev-dir)
            when (corner-p prev-norm dir-norm cross-threshold)
              do (push i corners)
            do (setf prev-dir (edge-direction edge 1.0d0))))
    (setf corners (nreverse corners))

    (cond
      ;; No corners - all edges get white
      ((null corners)
       (dolist (edge edges)
         (setf (edge-segment-color edge) +edge-color-white+)))

      ;; Single corner - special handling
      ((= (length corners) 1)
       (let ((colors (make-array 3 :initial-element +edge-color-white+)))
         (multiple-value-bind (c1 s1) (switch-color +edge-color-white+ seed +edge-color-black+)
           (setf (aref colors 0) c1
                 seed s1))
         (setf (aref colors 2) (aref colors 0))
         (multiple-value-bind (c2 s2) (switch-color (aref colors 2) seed +edge-color-black+)
           (setf (aref colors 2) c2
                 seed s2))

         (if (>= edge-count 3)
             ;; Distribute colors among edges
             (let ((corner (first corners))
                   (m edge-count))
               (loop for j from 0 below m
                     for index = (mod (+ corner j) m)
                     for edge = (nth index edges)
                     ;; Color assignment based on position
                     for color-index = (max 0 (min 2 (truncate (+ 3 (* 2.875 (/ j (1- m))) -1.4375 0.5) 1)))
                     do (setf (edge-segment-color edge) (aref colors color-index))))
             ;; For 1-2 edges, split them
             (let* ((corner (first corners))
                    (edge0 (nth 0 edges))
                    (split0 (split-edge edge0)))
               (cond
                 ((>= edge-count 2)
                  ;; Split both edges
                  (let* ((edge1 (nth 1 edges))
                         (split1 (split-edge edge1)))
                    (if (zerop corner)
                        ;; Corner at start
                        (progn
                          (setf (edge-segment-color (nth 0 split0)) (aref colors 0)
                                (edge-segment-color (nth 1 split0)) (aref colors 0)
                                (edge-segment-color (nth 2 split0)) (aref colors 1)
                                (edge-segment-color (nth 0 split1)) (aref colors 1)
                                (edge-segment-color (nth 1 split1)) (aref colors 2)
                                (edge-segment-color (nth 2 split1)) (aref colors 2))
                          (setf (contour-edges contour)
                                (append split0 split1)))
                        ;; Corner at second edge
                        (progn
                          (setf (edge-segment-color (nth 0 split1)) (aref colors 0)
                                (edge-segment-color (nth 1 split1)) (aref colors 0)
                                (edge-segment-color (nth 2 split1)) (aref colors 1)
                                (edge-segment-color (nth 0 split0)) (aref colors 1)
                                (edge-segment-color (nth 1 split0)) (aref colors 2)
                                (edge-segment-color (nth 2 split0)) (aref colors 2))
                          (setf (contour-edges contour)
                                (append split1 split0))))))
                 (t
                  ;; Single edge - split into 3
                  (setf (edge-segment-color (nth 0 split0)) (aref colors 0)
                        (edge-segment-color (nth 1 split0)) (aref colors 1)
                        (edge-segment-color (nth 2 split0)) (aref colors 2))
                  (setf (contour-edges contour) split0)))))))

      ;; Multiple corners - standard coloring
      (t
       (let* ((start (first corners))
              (m edge-count)
              (spline 0)
              (color +edge-color-white+)
              (initial-color nil))
         (multiple-value-bind (c s) (switch-color color seed +edge-color-black+)
           (setf color c
                 seed s
                 initial-color c))

         (loop for j from 0 below m
               for index = (mod (+ start j) m)
               for edge = (nth index edges)
               do ;; Check if this is a corner (except the first one)
                  (when (and (< (1+ spline) (length corners))
                             (= (nth (1+ spline) corners) index))
                    (incf spline)
                    ;; Switch color, banning initial color if this is the last spline
                    (let ((banned (if (= spline (1- (length corners)))
                                      initial-color
                                      +edge-color-black+)))
                      (multiple-value-bind (c s) (switch-color color seed banned)
                        (setf color c
                              seed s))))
                  (setf (edge-segment-color edge) color)))))
    seed))

;;; Shape normalization

(defun normalize-contour (contour)
  "Ensure contour has at least 3 edges by splitting if needed."
  (declare (type contour contour))
  (when (= (length (contour-edges contour)) 1)
    (let ((split (split-edge (first (contour-edges contour)))))
      (setf (contour-edges contour) split))))

(defun normalize-shape (shape)
  "Normalize all contours in a shape."
  (declare (type shape shape))
  (dolist (contour (shape-contours shape))
    (normalize-contour contour)))

;;; Main edge coloring function

(defun color-shape-edges (shape &optional (seed 0))
  "Color all edges in a shape for MSDF generation."
  (declare (type shape shape) (type (unsigned-byte 64) seed))
  (dolist (contour (shape-contours shape))
    (setf seed (color-contour-edges contour seed))
    (setf (contour-winding contour) (calculate-contour-winding contour)))
  seed)
