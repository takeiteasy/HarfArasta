;;;; earcut.lisp — Ear-clipping triangulation with hole bridging and z-order hashing
;;;; Based on the mapbox/earcut algorithm.

(in-package #:harfarasta)

;;; ============================================================================
;;; Earcut Node — doubly-linked circular list
;;; ============================================================================

(defstruct earcut-node
  "A node in the earcut doubly-linked circular list."
  (i 0 :type fixnum)              ; vertex index in flat coords array
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (prev nil :type (or null earcut-node))
  (next nil :type (or null earcut-node))
  (z 0 :type fixnum)              ; z-order curve value
  (prev-z nil :type (or null earcut-node))
  (next-z nil :type (or null earcut-node))
  (steiner nil :type boolean))

;;; ============================================================================
;;; Building the linked list
;;; ============================================================================

(defun %earcut-insert-node (i x y last-node)
  "Create a node and insert it after LAST-NODE. Returns the new node."
  (let ((node (make-earcut-node :i i :x x :y y)))
    (if (null last-node)
        (progn
          (setf (earcut-node-prev node) node
                (earcut-node-next node) node))
        (progn
          (setf (earcut-node-next node) (earcut-node-next last-node)
                (earcut-node-prev node) last-node)
          (setf (earcut-node-prev (earcut-node-next last-node)) node)
          (setf (earcut-node-next last-node) node)))
    node))

(defun %earcut-remove-node (p)
  "Remove node P from the linked list."
  (setf (earcut-node-next (earcut-node-prev p)) (earcut-node-next p))
  (setf (earcut-node-prev (earcut-node-next p)) (earcut-node-prev p))
  (when (earcut-node-prev-z p)
    (setf (earcut-node-next-z (earcut-node-prev-z p)) (earcut-node-next-z p)))
  (when (earcut-node-next-z p)
    (setf (earcut-node-prev-z (earcut-node-next-z p)) (earcut-node-prev-z p))))

(defun %earcut-linked-list (coords start end ccw-p)
  "Build a circular doubly-linked list from coords[start..end).
COORDS is a flat single-float array of x,y pairs.
START and END are vertex indices (not byte offsets).
CCW-P: when T, ensure the resulting ring is CCW (positive area);
when NIL, ensure it is CW (negative area).
Returns the first node of the list (last-node's next)."
  (let* ((last-node nil)
         (sa (> (%earcut-signed-area coords start end) 0.0d0))
         ;; If ccw-p matches data winding, keep order; otherwise reverse
         (forward (eq ccw-p sa)))
    (if forward
        (loop for i from start below end
              do (setf last-node
                       (%earcut-insert-node i
                                            (aref coords (* i 2))
                                            (aref coords (1+ (* i 2)))
                                            last-node)))
        (loop for i from (1- end) downto start
              do (setf last-node
                       (%earcut-insert-node i
                                            (aref coords (* i 2))
                                            (aref coords (1+ (* i 2)))
                                            last-node))))
    (when (and last-node
               (= (earcut-node-x last-node)
                  (earcut-node-x (earcut-node-next last-node)))
               (= (earcut-node-y last-node)
                  (earcut-node-y (earcut-node-next last-node))))
      (%earcut-remove-node last-node)
      (setf last-node (earcut-node-next last-node)))
    (when last-node
      (earcut-node-next last-node))))

;;; ============================================================================
;;; Signed area and winding
;;; ============================================================================

(defun %earcut-signed-area (coords start end)
  "Signed area of ring coords[start..end). Positive = CCW."
  (let ((sum 0.0d0)
        (n (- end start)))
    (declare (type double-float sum))
    (loop for i from start below end
          for j = (+ start (mod (1+ (- i start)) n))
          do (incf sum (* (- (float (aref coords (* i 2)) 1.0d0)
                             (float (aref coords (* j 2)) 1.0d0))
                          (+ (float (aref coords (1+ (* i 2))) 1.0d0)
                             (float (aref coords (1+ (* j 2))) 1.0d0)))))
    sum))

;;; ============================================================================
;;; Filter duplicate/collinear points
;;; ============================================================================

(defun %earcut-equals-p (a b)
  "Check if two nodes have the same coordinates."
  (and (= (earcut-node-x a) (earcut-node-x b))
       (= (earcut-node-y a) (earcut-node-y b))))

(declaim (inline %earcut-triangle-area))
(defun %earcut-triangle-area (p q r)
  "Signed area of triangle formed by nodes P, Q, R."
  (declare (type earcut-node p q r))
  (- (* (- (earcut-node-y q) (earcut-node-y p))
        (- (earcut-node-x r) (earcut-node-x q)))
     (* (- (earcut-node-x q) (earcut-node-x p))
        (- (earcut-node-y r) (earcut-node-y q)))))

(defun %earcut-filter-points (start &optional (end start))
  "Remove duplicate and collinear adjacent points. Returns the new start node or NIL."
  (let ((p start)
        (again t))
    (loop while again do
      (setf again nil)
      (if (or (not (earcut-node-steiner p))
              t)  ; always check
          (if (and (not (%earcut-equals-p p (earcut-node-next p)))
                   (not (zerop (%earcut-triangle-area
                                p (earcut-node-prev p) (earcut-node-next p)))))
              (setf p (earcut-node-next p))
              (progn
                (%earcut-remove-node p)
                (setf p (earcut-node-next p)
                      end (earcut-node-next p))
                (when (eq p (earcut-node-next p))
                  (return-from %earcut-filter-points nil))
                (setf again t)))
          (setf p (earcut-node-next p)))
      (when (eq p end)
        (setf again nil)))
    p))

;;; ============================================================================
;;; Z-order hashing for spatial acceleration
;;; ============================================================================

(defun %earcut-z-order (x y min-x min-y inv-size)
  "Calculate Morton code (z-order) for point (X, Y)."
  (declare (type single-float x y min-x min-y inv-size))
  (let ((lx (truncate (* (- x min-x) inv-size)))
        (ly (truncate (* (- y min-y) inv-size))))
    ;; Clamp to 15 bits
    (setf lx (logand lx #x7FFF)
          ly (logand ly #x7FFF))
    ;; Interleave bits
    (setf lx (logand (logior lx (ash lx 8)) #x00FF00FF)
          lx (logand (logior lx (ash lx 4)) #x0F0F0F0F)
          lx (logand (logior lx (ash lx 2)) #x33333333)
          lx (logand (logior lx (ash lx 1)) #x55555555))
    (setf ly (logand (logior ly (ash ly 8)) #x00FF00FF)
          ly (logand (logior ly (ash ly 4)) #x0F0F0F0F)
          ly (logand (logior ly (ash ly 2)) #x33333333)
          ly (logand (logior ly (ash ly 1)) #x55555555))
    (logior lx (ash ly 1))))

(defun %earcut-index-curve (start min-x min-y inv-size)
  "Assign z-order values to each node and build a z-order linked list."
  (let ((p start))
    (loop do
      (when (zerop (earcut-node-z p))
        (setf (earcut-node-z p)
              (%earcut-z-order (earcut-node-x p) (earcut-node-y p)
                               min-x min-y inv-size)))
      (setf (earcut-node-prev-z p) (earcut-node-prev p)
            (earcut-node-next-z p) (earcut-node-next p))
      (setf p (earcut-node-next p))
      (when (eq p start) (return)))
    ;; Sort by z-order using simple insertion sort on the linked list
    (setf (earcut-node-prev-z (earcut-node-prev-z start)) nil)
    (setf (earcut-node-next-z (earcut-node-prev start)) nil)
    ;; Use merge sort on the z-order list
    (%earcut-sort-linked start)))

(defun %earcut-sort-linked (list)
  "Bottom-up merge sort for z-order linked list. Returns the new head."
  (let ((in-size 1)
        (head list)
        (num-merges 0))
    (loop do
      (setf num-merges 0)
      (let ((p head)
            (tail nil))
        (setf head nil)
        (loop while p do
          (incf num-merges)
          (let ((q p)
                (p-size 0)
                (q-size in-size))
            (dotimes (i in-size)
              (incf p-size)
              (setf q (earcut-node-next-z q))
              (unless q (return)))
            (loop while (or (> p-size 0) (and (> q-size 0) q)) do
              (let ((e nil))
                (cond
                  ((zerop p-size)
                   (setf e q
                         q (earcut-node-next-z q))
                   (decf q-size))
                  ((or (zerop q-size) (null q))
                   (setf e p
                         p (earcut-node-next-z p))
                   (decf p-size))
                  ((<= (earcut-node-z p) (earcut-node-z q))
                   (setf e p
                         p (earcut-node-next-z p))
                   (decf p-size))
                  (t
                   (setf e q
                         q (earcut-node-next-z q))
                   (decf q-size)))
                (if tail
                    (setf (earcut-node-next-z tail) e)
                    (setf head e))
                (setf (earcut-node-prev-z e) tail
                      tail e)))
            (setf p q)))
        (when tail
          (setf (earcut-node-next-z tail) nil)))
      (setf in-size (* in-size 2))
      (when (<= num-merges 1)
        (return head)))))

;;; ============================================================================
;;; Point-in-triangle test
;;; ============================================================================

(declaim (inline %earcut-point-in-triangle-p))
(defun %earcut-point-in-triangle-p (ax ay bx by cx cy px py)
  "Return T if (px,py) is inside triangle (ax,ay)-(bx,by)-(cx,cy)."
  (declare (type single-float ax ay bx by cx cy px py))
  (and (>= (* (- cx px) (- ay py) ) (* (- ax px) (- cy py)))
       (>= (* (- ax px) (- by py)) (* (- bx px) (- ay py)))
       (>= (* (- bx px) (- cy py)) (* (- cx px) (- by py)))))

;;; ============================================================================
;;; Ear detection
;;; ============================================================================

(defun %earcut-is-ear-p (ear)
  "Check if the triangle at EAR is a valid ear (convex + no points inside)."
  (declare (type earcut-node ear))
  (let* ((a (earcut-node-prev ear))
         (b ear)
         (c (earcut-node-next ear))
         (ax (earcut-node-x a)) (ay (earcut-node-y a))
         (bx (earcut-node-x b)) (by (earcut-node-y b))
         (cx (earcut-node-x c)) (cy (earcut-node-y c)))
    ;; Ear candidate requires negative signed area (convex in CCW outer ring)
    (when (>= (%earcut-triangle-area a b c) 0.0)
      (return-from %earcut-is-ear-p nil))
    ;; Check no other point inside this triangle
    (let ((p (earcut-node-next c)))
      (loop while (not (eq p a)) do
        (when (and (%earcut-point-in-triangle-p ax ay bx by cx cy
                                                 (earcut-node-x p) (earcut-node-y p))
                   (>= (%earcut-triangle-area (earcut-node-prev p) p (earcut-node-next p)) 0.0))
          (return-from %earcut-is-ear-p nil))
        (setf p (earcut-node-next p))))
    t))

(defun %earcut-is-ear-hashed-p (ear min-x min-y inv-size)
  "Optimized ear check using z-order to limit search range."
  (declare (type earcut-node ear)
           (type single-float min-x min-y inv-size))
  (let* ((a (earcut-node-prev ear))
         (b ear)
         (c (earcut-node-next ear))
         (ax (earcut-node-x a)) (ay (earcut-node-y a))
         (bx (earcut-node-x b)) (by (earcut-node-y b))
         (cx (earcut-node-x c)) (cy (earcut-node-y c)))
    (when (>= (%earcut-triangle-area a b c) 0.0)
      (return-from %earcut-is-ear-hashed-p nil))
    ;; Bounding box of the triangle
    (let* ((x0 (min ax bx cx))
           (y0 (min ay by cy))
           (x1 (max ax bx cx))
           (y1 (max ay by cy))
           (min-z (%earcut-z-order x0 y0 min-x min-y inv-size))
           (max-z (%earcut-z-order x1 y1 min-x min-y inv-size))
           (p (earcut-node-prev-z ear))
           (n (earcut-node-next-z ear)))
      ;; Search backward and forward in z-order
      (loop while (and p (>= (earcut-node-z p) min-z)) do
        (when (and (not (eq p a))
                   (not (eq p c))
                   (%earcut-point-in-triangle-p ax ay bx by cx cy
                                                 (earcut-node-x p) (earcut-node-y p))
                   (>= (%earcut-triangle-area (earcut-node-prev p) p (earcut-node-next p)) 0.0))
          (return-from %earcut-is-ear-hashed-p nil))
        (setf p (earcut-node-prev-z p)))
      (loop while (and n (<= (earcut-node-z n) max-z)) do
        (when (and (not (eq n a))
                   (not (eq n c))
                   (%earcut-point-in-triangle-p ax ay bx by cx cy
                                                 (earcut-node-x n) (earcut-node-y n))
                   (>= (%earcut-triangle-area (earcut-node-prev n) n (earcut-node-next n)) 0.0))
          (return-from %earcut-is-ear-hashed-p nil))
        (setf n (earcut-node-next-z n)))
      t)))

;;; ============================================================================
;;; Hole elimination
;;; ============================================================================

(defun %earcut-get-leftmost (start)
  "Find the leftmost node in the ring starting at START."
  (let ((p start)
        (leftmost start))
    (loop do
      (when (or (< (earcut-node-x p) (earcut-node-x leftmost))
                (and (= (earcut-node-x p) (earcut-node-x leftmost))
                     (< (earcut-node-y p) (earcut-node-y leftmost))))
        (setf leftmost p))
      (setf p (earcut-node-next p))
      (when (eq p start) (return)))
    leftmost))

(defun %earcut-find-hole-bridge (hole outer-node)
  "Find a bridge vertex on the outer ring visible from the hole's leftmost point.
Casts a horizontal ray leftward from HOLE and finds the nearest outer edge intersection,
then selects the best bridge vertex (closest with smallest reflex angle)."
  (declare (type earcut-node hole outer-node))
  (let* ((hx (earcut-node-x hole))
         (hy (earcut-node-y hole))
         (p outer-node)
         (qx most-negative-single-float)
         (m nil))
    ;; Cast ray leftward from hole point; find nearest intersection with outer ring
    (loop do
      (let ((py (earcut-node-y p))
            (ny (earcut-node-y (earcut-node-next p))))
        (when (and (<= hy (max py ny))
                   (>= hy (min py ny))
                   (/= py ny))
          (let ((x (+ (earcut-node-x p)
                      (/ (* (- hy py)
                            (- (earcut-node-x (earcut-node-next p))
                               (earcut-node-x p)))
                         (- ny py)))))
            (when (and (<= x hx) (> x qx))
              (setf qx x)
              ;; Pick the vertex with smaller x (closer to hole)
              (if (< (earcut-node-x p) (earcut-node-x (earcut-node-next p)))
                  (setf m p)
                  (setf m (earcut-node-next p)))
              ;; Hole touches outer segment exactly — early return
              (when (= x hx)
                (return-from %earcut-find-hole-bridge m))))))
      (setf p (earcut-node-next p))
      (when (eq p outer-node) (return)))
    (unless m
      (return-from %earcut-find-hole-bridge nil))
    ;; Look for outer ring vertices inside the triangle (hole, intersection, m)
    ;; that would be a better (closer, less reflex) bridge point
    (let ((stop m)
          (mx (earcut-node-x m))
          (my (earcut-node-y m))
          (tan-min most-positive-single-float)
          (candidate m))
      (setf p m)
      (loop do
        (when (and (>= hx (earcut-node-x p))
                   (>= (earcut-node-x p) mx)
                   (/= hx (earcut-node-x p))
                   (%earcut-point-in-triangle-p
                    (if (< hy my) hx qx) hy mx my
                    (if (>= hy my) hx qx) hy
                    (earcut-node-x p) (earcut-node-y p)))
          (let ((tan-val (abs (/ (- hy (earcut-node-y p))
                                 (- hx (earcut-node-x p))))))
            (when (and (%earcut-locally-inside-p p hole)
                       (or (< tan-val tan-min)
                           (and (= tan-val tan-min)
                                (or (> (earcut-node-x p) (earcut-node-x candidate))
                                    (and (= (earcut-node-x p) (earcut-node-x candidate))
                                         (%earcut-sector-contains-sector-p candidate p))))))
              (setf candidate p
                    tan-min tan-val))))
        (setf p (earcut-node-next p))
        (when (eq p stop) (return)))
      candidate)))

(defun %earcut-split-polygon (a b)
  "Create a bridge between nodes A and B, creating new nodes.
Returns the new node on A's side."
  (let ((a2 (%earcut-insert-node (earcut-node-i a) (earcut-node-x a) (earcut-node-y a) nil))
        (b2 (%earcut-insert-node (earcut-node-i b) (earcut-node-x b) (earcut-node-y b) nil)))
    ;; a -> ... -> b  becomes  a -> b2 -> ... -> a  and  b -> a2 -> ... -> b
    (let ((an (earcut-node-next a))
          (bp (earcut-node-prev b)))
      (setf (earcut-node-next a) b
            (earcut-node-prev b) a)
      (setf (earcut-node-next a2) an
            (earcut-node-prev an) a2)
      (setf (earcut-node-next bp) b2
            (earcut-node-prev b2) bp)
      (setf (earcut-node-next b2) a2
            (earcut-node-prev a2) b2))
    b2))

(defun %earcut-eliminate-hole (hole outer-node)
  "Bridge a hole ring into the outer ring. Returns the updated outer node."
  (let* ((bridge (%earcut-find-hole-bridge hole outer-node)))
    (if bridge
        (let ((bridge-reverse (%earcut-split-polygon bridge hole)))
          (%earcut-filter-points bridge-reverse (earcut-node-next bridge-reverse))
          (%earcut-filter-points bridge (earcut-node-next bridge)))
        outer-node)))

(defun %earcut-eliminate-holes (coords hole-indices outer-node)
  "Eliminate all holes by bridging them into the outer polygon.
HOLE-INDICES is a list of vertex indices where each hole starts.
Returns the updated outer node."
  (let ((queue nil)
        (n-holes (length hole-indices)))
    ;; Build linked lists for each hole and collect their leftmost nodes
    (loop for k from 0 below n-holes
          for start-idx = (nth k hole-indices)
          for end-idx = (if (< k (1- n-holes))
                            (nth (1+ k) hole-indices)
                            (/ (length coords) 2))
          do (let ((list (%earcut-linked-list coords start-idx end-idx nil)))
               (when list
                 (when (eq list (earcut-node-next list))
                   (setf (earcut-node-steiner list) t))
                 (push (%earcut-get-leftmost list) queue))))
    ;; Sort holes by x coordinate (leftmost first)
    (setf queue (sort queue (lambda (a b)
                              (< (earcut-node-x a) (earcut-node-x b)))))
    ;; Bridge each hole
    (dolist (hole queue)
      (setf outer-node (%earcut-eliminate-hole hole outer-node)))
    outer-node))

;;; ============================================================================
;;; Main triangulation loop
;;; ============================================================================

(defun %earcut-cure-local-intersections (start triangles)
  "Fix local self-intersections after the main loop."
  (let ((p start))
    (loop do
      (let* ((a (earcut-node-prev p))
             (b (earcut-node-next (earcut-node-next p))))
        (when (and (not (%earcut-equals-p a b))
                   (%earcut-intersects-p a p (earcut-node-next p) b)
                   (%earcut-locally-inside-p a b)
                   (%earcut-locally-inside-p b a))
          ;; Output triangle
          (vector-push-extend (earcut-node-i a) triangles)
          (vector-push-extend (earcut-node-i p) triangles)
          (vector-push-extend (earcut-node-i b) triangles)
          ;; Remove two nodes
          (%earcut-remove-node p)
          (%earcut-remove-node (earcut-node-next p))
          (setf p b
                start b)))
      (setf p (earcut-node-next p))
      (when (eq p start)
        (return start)))))

(defun %earcut-on-segment-p (p q r)
  "Check if point Q lies on segment PR."
  (and (<= (earcut-node-x q) (max (earcut-node-x p) (earcut-node-x r)))
       (>= (earcut-node-x q) (min (earcut-node-x p) (earcut-node-x r)))
       (<= (earcut-node-y q) (max (earcut-node-y p) (earcut-node-y r)))
       (>= (earcut-node-y q) (min (earcut-node-y p) (earcut-node-y r)))))

(defun %earcut-sign (x)
  (cond ((> x 0.0) 1)
        ((< x 0.0) -1)
        (t 0)))

(defun %earcut-intersects-p (p1 q1 p2 q2)
  "Check if segments p1-q1 and p2-q2 intersect."
  (let ((o1 (%earcut-sign (%earcut-triangle-area p1 q1 p2)))
        (o2 (%earcut-sign (%earcut-triangle-area p1 q1 q2)))
        (o3 (%earcut-sign (%earcut-triangle-area p2 q2 p1)))
        (o4 (%earcut-sign (%earcut-triangle-area p2 q2 q1))))
    (cond
      ((and (/= o1 o2) (/= o3 o4)) t)
      ((and (zerop o1) (%earcut-on-segment-p p1 p2 q1)) t)
      ((and (zerop o2) (%earcut-on-segment-p p1 q2 q1)) t)
      ((and (zerop o3) (%earcut-on-segment-p p2 p1 q2)) t)
      ((and (zerop o4) (%earcut-on-segment-p p2 q1 q2)) t)
      (t nil))))

(defun %earcut-locally-inside-p (a b)
  "Check if B is locally inside the polygon at node A."
  ;; Matches mapbox: area(prev,a,next) < 0 means convex vertex in CW ring
  (if (< (%earcut-triangle-area (earcut-node-prev a) a (earcut-node-next a)) 0.0)
      (and (>= (%earcut-triangle-area a b (earcut-node-next a)) 0.0)
           (>= (%earcut-triangle-area a (earcut-node-prev a) b) 0.0))
      (or (< (%earcut-triangle-area a b (earcut-node-prev a)) 0.0)
          (< (%earcut-triangle-area a (earcut-node-next a) b) 0.0))))

(defun %earcut-sector-contains-sector-p (m p)
  "Whether sector in node m contains sector in node p in the same coordinates."
  (and (< (%earcut-triangle-area (earcut-node-prev m) m (earcut-node-prev p)) 0.0)
       (< (%earcut-triangle-area (earcut-node-next p) m (earcut-node-next m)) 0.0)))

(defun %earcut-split-earcut (start triangles min-x min-y inv-size)
  "Try to split remaining polygon and triangulate each half."
  (let ((a start))
    (loop do
      (let ((b (earcut-node-next (earcut-node-next a))))
        (loop while (not (eq b (earcut-node-prev a))) do
          (when (and (not (eq (earcut-node-i a) (earcut-node-i b)))
                     (%earcut-is-valid-diagonal-p a b))
            (let ((c (%earcut-split-polygon a b)))
              (setf a (%earcut-filter-points a (earcut-node-next a))
                    c (%earcut-filter-points c (earcut-node-next c)))
              (%earcut-triangulate-linked a triangles min-x min-y inv-size 0)
              (%earcut-triangulate-linked c triangles min-x min-y inv-size 0)
              (return-from %earcut-split-earcut)))
          (setf b (earcut-node-next b))))
      (setf a (earcut-node-next a))
      (when (eq a start)
        (return)))))

(defun %earcut-is-valid-diagonal-p (a b)
  "Check if diagonal a-b is valid (doesn't intersect other edges, locally inside)."
  (and (%earcut-locally-inside-p a b)
       (%earcut-locally-inside-p b a)
       (%earcut-middle-inside-p a b)
       ;; Check that diagonal doesn't intersect any polygon edge
       (not (%earcut-intersects-polygon-p a b))))

(defun %earcut-middle-inside-p (a b)
  "Check if the midpoint of a-b is inside the polygon."
  (let* ((p a)
         (inside nil)
         (px (/ (+ (earcut-node-x a) (earcut-node-x b)) 2.0))
         (py (/ (+ (earcut-node-y a) (earcut-node-y b)) 2.0)))
    (loop do
      (when (and (not (eq (> (earcut-node-y p) py)
                          (> (earcut-node-y (earcut-node-next p)) py)))
                 (<= px (+ (earcut-node-x (earcut-node-next p))
                           (/ (* (- py (earcut-node-y (earcut-node-next p)))
                                 (- (earcut-node-x p) (earcut-node-x (earcut-node-next p))))
                              (- (earcut-node-y p) (earcut-node-y (earcut-node-next p)))))))
        (setf inside (not inside)))
      (setf p (earcut-node-next p))
      (when (eq p a) (return)))
    inside))

(defun %earcut-intersects-polygon-p (a b)
  "Check if any edge in the polygon intersects segment a-b."
  (let ((p a))
    (loop do
      (when (and (not (eq (earcut-node-i p) (earcut-node-i a)))
                 (not (eq (earcut-node-i (earcut-node-next p)) (earcut-node-i a)))
                 (not (eq (earcut-node-i p) (earcut-node-i b)))
                 (not (eq (earcut-node-i (earcut-node-next p)) (earcut-node-i b)))
                 (%earcut-intersects-p p (earcut-node-next p) a b))
        (return-from %earcut-intersects-polygon-p t))
      (setf p (earcut-node-next p))
      (when (eq p a)
        (return nil)))))

(defun %earcut-triangulate-linked (ear triangles min-x min-y inv-size pass)
  "Main ear-clipping loop. PASS: 0=strict, 1=allow-collinear, 2=split."
  (when (null ear) (return-from %earcut-triangulate-linked))
  (let ((use-z (and (/= inv-size 0.0) (zerop pass)))
        (stop ear))
    (when (and use-z (zerop (earcut-node-z ear)))
      (%earcut-index-curve ear min-x min-y inv-size))
    (let ((prev nil)
          (next nil))
      (loop while (not (eq (earcut-node-prev ear) (earcut-node-next ear))) do
        (setf prev (earcut-node-prev ear)
              next (earcut-node-next ear))
        (if (if use-z
                (%earcut-is-ear-hashed-p ear min-x min-y inv-size)
                (%earcut-is-ear-p ear))
            (progn
              ;; Output triangle indices
              (vector-push-extend (earcut-node-i prev) triangles)
              (vector-push-extend (earcut-node-i ear) triangles)
              (vector-push-extend (earcut-node-i next) triangles)
              (%earcut-remove-node ear)
              ;; Skip next vertex
              (setf ear (earcut-node-next next)
                    stop (earcut-node-next next)))
            (progn
              (setf ear (earcut-node-next ear))
              (when (eq ear stop)
                ;; No ear found in this pass, try next pass
                (cond
                  ((zerop pass)
                   (setf ear (%earcut-filter-points ear))
                   (when ear
                     (%earcut-triangulate-linked ear triangles min-x min-y inv-size 1)))
                  ((= pass 1)
                   (setf ear (%earcut-cure-local-intersections ear triangles))
                   (when (and ear (not (eq (earcut-node-prev ear) (earcut-node-next ear))))
                     (%earcut-triangulate-linked ear triangles min-x min-y inv-size 2)))
                  ((= pass 2)
                   (%earcut-split-earcut ear triangles min-x min-y inv-size)))
                (return-from %earcut-triangulate-linked))))))))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun earcut (coords &optional hole-indices)
  "Triangulate a polygon with holes using ear-clipping.
COORDS: flat (simple-array single-float (*)) of x,y pairs.
HOLE-INDICES: list of vertex indices where each hole ring starts.
Returns (simple-array (unsigned-byte 32) (*)) of triangle vertex indices."
  (declare (type (simple-array single-float (*)) coords))
  (let ((n (/ (length coords) 2)))
    (when (zerop n)
      (return-from earcut (make-array 0 :element-type '(unsigned-byte 32))))
    (let* ((outer-end (if hole-indices (first hole-indices) n))
           (outer-node (%earcut-linked-list coords 0 outer-end t)))
      (unless outer-node
        (return-from earcut (make-array 0 :element-type '(unsigned-byte 32))))
      ;; Eliminate holes
      (when hole-indices
        (setf outer-node (%earcut-eliminate-holes coords hole-indices outer-node)))
      ;; Compute bounding box for z-order optimization
      (let ((min-x most-positive-single-float)
            (min-y most-positive-single-float)
            (max-x most-negative-single-float)
            (max-y most-negative-single-float)
            (inv-size 0.0))
        (when (> n 80)
          (let ((p outer-node))
            (loop do
              (let ((x (earcut-node-x p))
                    (y (earcut-node-y p)))
                (when (< x min-x) (setf min-x x))
                (when (< y min-y) (setf min-y y))
                (when (> x max-x) (setf max-x x))
                (when (> y max-y) (setf max-y y)))
              (setf p (earcut-node-next p))
              (when (eq p outer-node) (return))))
          (let ((range (max (- max-x min-x) (- max-y min-y))))
            (when (> range 0.0)
              (setf inv-size (/ 32767.0 range)))))
        ;; Triangulate
        (let ((triangles (make-array 0 :element-type '(unsigned-byte 32)
                                       :adjustable t :fill-pointer 0)))
          (%earcut-triangulate-linked outer-node triangles min-x min-y inv-size 0)
          ;; Return a simple array
          (let ((result (make-array (length triangles) :element-type '(unsigned-byte 32))))
            (loop for i from 0 below (length triangles)
                  do (setf (aref result i) (aref triangles i)))
            result))))))
