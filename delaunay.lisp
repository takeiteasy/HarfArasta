;;;; delaunay.lisp

(in-package #:harfarasta)

;;; ============================================================================
;;; Data Structures
;;; ============================================================================

(defstruct vertex
  "A vertex in the triangulation."
  (pos (make-vec2 0.0 0.0) :type vec2)
  (edges nil :type list))  ; List of quad-edges originating from this vertex

(defstruct quad-edge
  "One of four quad-edges comprising an edge."
  (org nil :type (or null vertex))
  (onext nil :type (or null quad-edge))
  (idx 0 :type (integer 0 3))
  (visited nil :type boolean)
  (edge nil :type (or null edge)))  ; Back-pointer to parent edge

(defstruct edge
  "An edge containing four quad-edges."
  (e (make-array 4) :type (simple-vector 4))  ; Array of 4 quad-edges
  (ids nil :type list))  ; List of constraint IDs on this edge

(defstruct triangle
  "A triangle with three edges and three vertices."
  (edges (make-array 3) :type (simple-vector 3))
  (x (make-array 3 :element-type 'single-float) :type (simple-array single-float (3)))
  (y (make-array 3 :element-type 'single-float) :type (simple-array single-float (3))))

(defstruct triangulation-context
  "The triangulation context."
  (vertices nil :type list)
  (edges nil :type list)
  (super-triangle-vertices nil :type list))  ; Track super-triangle vertices for filtering

(defstruct locate-result
  "Result of point location."
  (exact-vertex-p nil :type boolean)
  (on-edge-p nil :type boolean)
  (vertex nil :type (or null vertex))
  (edge nil :type (or null quad-edge)))

;;; ============================================================================
;;; Quad-Edge Algebra
;;; ============================================================================

(declaim (inline qe-rot qe-inv-rot qe-sym qe-dst qe-onext qe-oprev
                 qe-lnext qe-lprev qe-dnext qe-dprev qe-rprev))

(defun qe-rot (e)
  "Rotate quad-edge 90 degrees counterclockwise."
  (declare (type quad-edge e))
  (let* ((idx (quad-edge-idx e))
         (edge (quad-edge-edge e))
         (new-idx (svref #(1 2 3 0) idx)))
    (svref (edge-e edge) new-idx)))

(defun qe-inv-rot (e)
  "Rotate quad-edge 90 degrees clockwise."
  (declare (type quad-edge e))
  (let* ((idx (quad-edge-idx e))
         (edge (quad-edge-edge e))
         (new-idx (svref #(3 0 1 2) idx)))
    (svref (edge-e edge) new-idx)))

(defun qe-sym (e)
  "Return the symmetric (opposite direction) quad-edge."
  (declare (type quad-edge e))
  (let* ((idx (quad-edge-idx e))
         (edge (quad-edge-edge e))
         (new-idx (svref #(2 3 0 1) idx)))
    (svref (edge-e edge) new-idx)))

(defun qe-dst (e)
  "Return the destination vertex of the quad-edge."
  (declare (type quad-edge e))
  (quad-edge-org (qe-sym e)))

(defun qe-onext (e)
  "Return the next quad-edge with the same origin, counterclockwise."
  (declare (type quad-edge e))
  (quad-edge-onext e))

(defun qe-oprev (e)
  "Return the previous quad-edge with the same origin, clockwise."
  (declare (type quad-edge e))
  (qe-rot (qe-onext (qe-rot e))))

(defun qe-lnext (e)
  "Return the next edge around the left face."
  (declare (type quad-edge e))
  (qe-rot (qe-onext (qe-inv-rot e))))

(defun qe-lprev (e)
  "Return the previous edge around the left face."
  (declare (type quad-edge e))
  (qe-sym (qe-onext e)))

(defun qe-dnext (e)
  "Return the next edge around the destination vertex."
  (declare (type quad-edge e))
  (qe-sym (qe-onext (qe-sym e))))

(defun qe-dprev (e)
  "Return the previous edge around the destination vertex."
  (declare (type quad-edge e))
  (qe-inv-rot (qe-onext (qe-inv-rot e))))

(defun qe-rprev (e)
  "Return the previous edge around the right face."
  (declare (type quad-edge e))
  (qe-onext (qe-sym e)))

;;; ============================================================================
;;; Geometric Predicates
;;; ============================================================================

(declaim (inline orientation right-of-p left-of-p on-line-p
                 in-circumcircle-p convex-p in-triangle-p))

(defun orientation (a b c)
  "Return twice the signed area of triangle ABC.
   Positive if CCW, negative if CW, zero if collinear.
   Computed in double-float for precision."
  (declare (type vec2 a b c))
  (let ((x1 (- (float (vec2-x b) 1.0d0) (float (vec2-x a) 1.0d0)))
        (y1 (- (float (vec2-y b) 1.0d0) (float (vec2-y a) 1.0d0)))
        (x2 (- (float (vec2-x c) 1.0d0) (float (vec2-x a) 1.0d0)))
        (y2 (- (float (vec2-y c) 1.0d0) (float (vec2-y a) 1.0d0))))
    (- (* x1 y2) (* x2 y1))))

(defun right-of-p (p e)
  "Return T if point P is to the right of edge E."
  (declare (type vec2 p) (type quad-edge e))
  (< (orientation (vertex-pos (quad-edge-org e))
                  (vertex-pos (qe-dst e))
                  p)
     0.0))

(defun left-of-p (p e)
  "Return T if point P is to the left of edge E."
  (declare (type vec2 p) (type quad-edge e))
  (> (orientation (vertex-pos (quad-edge-org e))
                  (vertex-pos (qe-dst e))
                  p)
     0.0))

(defun on-line-p (p org dst)
  "Return T if point P is on the line from ORG to DST."
  (declare (type vec2 p org dst))
  (= (orientation org dst p) 0.0))

(defun in-circumcircle-p (p a b c)
  "Return T if point P is inside the circumcircle of triangle ABC.
   Computed in double-float for precision."
  (declare (type vec2 p a b c))
  (let* ((ax (float (vec2-x a) 1.0d0)) (ay (float (vec2-y a) 1.0d0))
         (bx (float (vec2-x b) 1.0d0)) (by (float (vec2-y b) 1.0d0))
         (cx (float (vec2-x c) 1.0d0)) (cy (float (vec2-y c) 1.0d0))
         (px (float (vec2-x p) 1.0d0)) (py (float (vec2-y p) 1.0d0))
         (ax- (- ax px)) (ay- (- ay py))
         (bx- (- bx px)) (by- (- by py))
         (cx- (- cx px)) (cy- (- cy py)))
    (> (+ (* (+ (* ax- ax-) (* ay- ay-))
             (- (* bx- cy-) (* cx- by-)))
          (- (* (+ (* bx- bx-) (* by- by-))
                (- (* ax- cy-) (* cx- ay-))))
          (* (+ (* cx- cx-) (* cy- cy-))
             (- (* ax- by-) (* bx- ay-))))
       0)))

(defun convex-p (a b c d)
  "Return T if quadrilateral ABCD is convex (including degenerate cases).
   Computed in double-float for precision."
  (declare (type vec2 a b c d))
  (let* ((x1 (- (float (vec2-x b) 1.0d0) (float (vec2-x a) 1.0d0)))
         (y1 (- (float (vec2-y b) 1.0d0) (float (vec2-y a) 1.0d0)))
         (x2 (- (float (vec2-x c) 1.0d0) (float (vec2-x b) 1.0d0)))
         (y2 (- (float (vec2-y c) 1.0d0) (float (vec2-y b) 1.0d0)))
         (x3 (- (float (vec2-x d) 1.0d0) (float (vec2-x c) 1.0d0)))
         (y3 (- (float (vec2-y d) 1.0d0) (float (vec2-y c) 1.0d0)))
         (x4 (- (float (vec2-x a) 1.0d0) (float (vec2-x d) 1.0d0)))
         (y4 (- (float (vec2-y a) 1.0d0) (float (vec2-y d) 1.0d0)))
         (c1 (- (* x1 y2) (* x2 y1)))
         (c2 (- (* x2 y3) (* x3 y2)))
         (c3 (- (* x3 y4) (* x4 y3)))
         (c4 (- (* x4 y1) (* x1 y4))))
    (and (>= (* c1 c2) 0.0d0)
         (>= (* c2 c3) 0.0d0)
         (>= (* c3 c4) 0.0d0))))

(defun in-triangle-p (p a b c)
  "Return T if point P is inside or on the edge of triangle ABC."
  (declare (type vec2 p a b c))
  (let* ((ab-x (- (vec2-x b) (vec2-x a)))
         (ab-y (- (vec2-y b) (vec2-y a)))
         (bc-x (- (vec2-x c) (vec2-x b)))
         (bc-y (- (vec2-y c) (vec2-y b)))
         (ca-x (- (vec2-x a) (vec2-x c)))
         (ca-y (- (vec2-y a) (vec2-y c)))
         (ap-x (- (vec2-x p) (vec2-x a)))
         (ap-y (- (vec2-y p) (vec2-y a)))
         (bp-x (- (vec2-x p) (vec2-x b)))
         (bp-y (- (vec2-y p) (vec2-y b)))
         (cp-x (- (vec2-x p) (vec2-x c)))
         (cp-y (- (vec2-y p) (vec2-y c)))
         (oa (- (* ab-x ap-y) (* ab-y ap-x)))
         (ob (- (* bc-x bp-y) (* bc-y bp-x)))
         (oc (- (* ca-x cp-y) (* ca-y cp-x))))
    (and (>= oa 0.0) (>= ob 0.0) (>= oc 0.0))))

;;; ============================================================================
;;; Edge and Vertex Operations
;;; ============================================================================

(defun edge-constrained-p (edge)
  "Return T if the edge has constraint IDs."
  (declare (type edge edge))
  (not (null (edge-ids edge))))

(defun splice (a b)
  "Fundamental quad-edge operation that swaps edge rings."
  (declare (type quad-edge a b))
  (let* ((alpha (qe-rot (qe-onext a)))
         (beta (qe-rot (qe-onext b)))
         (b-onext (qe-onext b))
         (a-onext (qe-onext a))
         (alpha-onext (qe-onext alpha))
         (beta-onext (qe-onext beta)))
    (setf (quad-edge-onext b) a-onext)
    (setf (quad-edge-onext a) b-onext)
    (setf (quad-edge-onext alpha) beta-onext)
    (setf (quad-edge-onext beta) alpha-onext)))

(defun swap-edge (e)
  "Swap edge E to connect the opposite diagonal of the quadrilateral."
  (declare (type quad-edge e))
  ;; Remove original reference from vertices
  (setf (vertex-edges (quad-edge-org e))
        (delete e (vertex-edges (quad-edge-org e)) :test #'eq))
  (setf (vertex-edges (qe-dst e))
        (delete (qe-sym e) (vertex-edges (qe-dst e)) :test #'eq))

  (let ((a (qe-oprev e))
        (b (qe-oprev (qe-sym e))))
    (splice e a)
    (splice (qe-sym e) b)
    (splice e (qe-lnext a))
    (splice (qe-sym e) (qe-lnext b))

    (setf (quad-edge-org e) (qe-dst a))
    (setf (quad-edge-org (qe-sym e)) (qe-dst b))

    ;; Add new references
    (push e (vertex-edges (quad-edge-org e)))
    (push (qe-sym e) (vertex-edges (qe-dst e)))))

(defun create-edge (ctx org dst)
  "Create a new edge between ORG and DST vertices."
  (declare (type triangulation-context ctx)
           (type vertex org dst))
  (let ((edge (make-edge)))
    ;; Initialize the four quad-edges
    (dotimes (i 4)
      (setf (svref (edge-e edge) i)
            (make-quad-edge :idx i :edge edge)))

    ;; Set up the primary edge (0 and 2)
    (let ((e0 (svref (edge-e edge) 0))
          (e1 (svref (edge-e edge) 1))
          (e2 (svref (edge-e edge) 2))
          (e3 (svref (edge-e edge) 3)))
      (setf (quad-edge-onext e0) e0)
      (setf (quad-edge-onext e2) e2)
      (setf (quad-edge-org e0) org)
      (setf (quad-edge-org e2) dst)

      ;; Dual edges
      (setf (quad-edge-onext e1) e3)
      (setf (quad-edge-onext e3) e1)

      ;; Add reference to endpoint vertices
      (push e0 (vertex-edges org))
      (push e2 (vertex-edges dst))

      ;; Add to context
      (push edge (triangulation-context-edges ctx))

      e0)))

(defun destroy-edge (ctx e)
  "Remove edge E from the triangulation."
  (declare (type triangulation-context ctx)
           (type quad-edge e))
  ;; Remove reference from endpoint vertices
  (setf (vertex-edges (quad-edge-org e))
        (delete e (vertex-edges (quad-edge-org e)) :test #'eq))
  (setf (vertex-edges (qe-dst e))
        (delete (qe-sym e) (vertex-edges (qe-dst e)) :test #'eq))

  (splice e (qe-oprev e))
  (splice (qe-sym e) (qe-oprev (qe-sym e)))

  (let ((edge (quad-edge-edge e)))
    (setf (triangulation-context-edges ctx)
          (delete edge (triangulation-context-edges ctx) :test #'eq))))

(defun connect (ctx a b)
  "Connect the destination of A to the origin of B."
  (declare (type triangulation-context ctx)
           (type quad-edge a b))
  (let ((e (create-edge ctx (qe-dst a) (quad-edge-org b))))
    (splice e (qe-lnext a))
    (splice (qe-sym e) b)
    e))

(defun create-vertex (ctx pos)
  "Create a new vertex at position POS."
  (declare (type triangulation-context ctx)
           (type vec2 pos))
  (let ((vertex (make-vertex :pos pos)))
    (push vertex (triangulation-context-vertices ctx))
    vertex))

;;; ============================================================================
;;; Core Algorithms
;;; ============================================================================

(defun flip-until-done (stack)
  "Flip edges until the stack is empty, restoring Delaunay property."
  (let ((visited nil))
    (loop while stack do
      (let ((e (pop stack)))
        (unless (quad-edge-visited e)
          (setf (quad-edge-visited e) t)
          (push e visited)

          (unless (edge-constrained-p (quad-edge-edge e))
            (when (in-circumcircle-p (vertex-pos (quad-edge-org (qe-dprev e)))
                                     (vertex-pos (qe-dst e))
                                     (vertex-pos (quad-edge-org e))
                                     (vertex-pos (quad-edge-org (qe-dnext e))))
              (let ((e1 (qe-lnext e))
                    (e2 (qe-dnext e)))
                (unless (quad-edge-visited e1) (push e1 stack))
                (unless (quad-edge-visited e2) (push e2 stack))
                (swap-edge e)))))))

    ;; Cleanup visited flags
    (dolist (e visited)
      (setf (quad-edge-visited e) nil))))

(defun vertex-ccw-compare (v1 v2)
  "Compare two vertices by angle for CCW sorting."
  (let* ((angle-a (atan (cdr v1) (car v1)))
         (angle-b (atan (cdr v2) (car v2))))
    (if (/= angle-a angle-b)
        (< angle-a angle-b)
        (let ((dist-a (+ (* (car v1) (car v1)) (* (cdr v1) (cdr v1))))
              (dist-b (+ (* (car v2) (car v2)) (* (cdr v2) (cdr v2)))))
          (< dist-a dist-b)))))

(defun ear-triangulate (ctx verts)
  "Triangulate a simple polygon using ear clipping."
  (declare (type triangulation-context ctx)
           (type list verts))
  (let ((new-edges nil)
        (num (length verts))
        (nodes (coerce verts 'vector)))

    ;; Build circular list indices
    (let ((next-indices (make-array num)))
      (dotimes (i num)
        (setf (aref next-indices i) (mod (1+ i) num)))

      (let ((first-idx 0))
        (loop while (> num 3) do
          (let ((found-ear nil))
            ;; Search for an ear
            (let ((left-idx first-idx))
              (dotimes (ii num)
                (let* ((tip-idx (aref next-indices left-idx))
                       (right-idx (aref next-indices tip-idx))
                       (p0 (vertex-pos (car (aref nodes left-idx))))
                       (p1 (vertex-pos (car (aref nodes tip-idx))))
                       (p2 (vertex-pos (car (aref nodes right-idx))))
                       (e0-x (- (vec2-x p1) (vec2-x p0)))
                       (e0-y (- (vec2-y p1) (vec2-y p0)))
                       (e1-x (- (vec2-x p2) (vec2-x p0)))
                       (e1-y (- (vec2-y p2) (vec2-y p0)))
                       (orient (- (* e0-x e1-y) (* e0-y e1-x))))
                  (when (> orient 0.0)
                    (let ((is-ear t))
                      ;; Check no other vertex inside this ear
                      (loop with node-idx = (aref next-indices right-idx)
                            until (= node-idx left-idx)
                            do (let ((v (vertex-pos (car (aref nodes node-idx)))))
                                 (when (in-triangle-p v p0 p1 p2)
                                   (setf is-ear nil)
                                   (return)))
                               (setf node-idx (aref next-indices node-idx)))
                      (when is-ear
                        ;; Found an ear
                        (let* ((vert-tip (car (aref nodes tip-idx)))
                               (vert-lft (car (aref nodes left-idx)))
                               (vert-rgt (car (aref nodes right-idx)))
                               (right-edge-in-face nil)
                               (left-edge-in-face nil))
                          ;; Find edges
                          (dolist (e (vertex-edges vert-tip))
                            (when (eq (qe-dst e) vert-lft)
                              (setf left-edge-in-face (qe-sym e)))
                            (when (eq (qe-dst e) vert-rgt)
                              (setf right-edge-in-face e)))

                          (when (and left-edge-in-face right-edge-in-face)
                            (let* ((edge1 (connect ctx right-edge-in-face left-edge-in-face))
                                   (edge2 (qe-sym edge1)))
                              (push edge1 new-edges)
                              (push edge2 new-edges))

                            ;; Clip the ear
                            (when (= first-idx tip-idx)
                              (setf first-idx (aref next-indices first-idx)))
                            (setf (aref next-indices left-idx) right-idx)
                            (decf num)
                            (setf found-ear t)
                            (return))))))
                  (setf left-idx (aref next-indices left-idx)))))

            (unless found-ear
              (error "Could not find an ear in polygon triangulation"))))))

    (flip-until-done new-edges)))

(defun destroy-vertex (ctx vert)
  "Remove a vertex from the triangulation and retriangulate the hole."
  (declare (type triangulation-context ctx)
           (type vertex vert))
  (let* ((edges-to-destroy (copy-list (vertex-edges vert)))
         (num-edges (length edges-to-destroy))
         (outline nil))

    ;; Collect outline vertices with their angles
    (dolist (e edges-to-destroy)
      (let* ((dst (qe-dst e))
             (dx (- (vec2-x (vertex-pos dst)) (vec2-x (vertex-pos vert))))
             (dy (- (vec2-y (vertex-pos dst)) (vec2-y (vertex-pos vert)))))
        (push (list dst dx dy) outline)
        (destroy-edge ctx e)))

    ;; Sort outline CCW
    (setf outline (sort outline
                        (lambda (a b)
                          (vertex-ccw-compare (cons (second a) (third a))
                                              (cons (second b) (third b))))))

    ;; Convert to format expected by ear-triangulate
    (let ((verts (mapcar (lambda (item)
                           (cons (first item) (cons (second item) (third item))))
                         outline)))
      (when (> num-edges 2)
        (ear-triangulate ctx verts)))

    ;; Remove vertex from context
    (setf (triangulation-context-vertices ctx)
          (delete vert (triangulation-context-vertices ctx) :test #'eq))))

;;; ============================================================================
;;; Point Location
;;; ============================================================================

(defun locate-triangle-point (ctx x y)
  "Locate the triangle containing point (X, Y)."
  (declare (type triangulation-context ctx)
           (type single-float x y))
  (let ((result (make-locate-result))
        (target (make-vec2 x y))
        (begin-edge (svref (edge-e (first (triangulation-context-edges ctx))) 0)))

    (loop with e1 = begin-edge do
      (let* ((e2 (qe-lnext e1))
             (e3 (qe-lnext e2))
             (v0 (quad-edge-org e1))
             (v1 (quad-edge-org e2))
             (v2 (quad-edge-org e3))
             (p0 (vertex-pos v0))
             (p1 (vertex-pos v1))
             (p2 (vertex-pos v2)))

        ;; Check if target matches a vertex
        (when (and (= (vec2-x p0) x) (= (vec2-y p0) y))
          (setf (locate-result-exact-vertex-p result) t
                (locate-result-vertex result) v0
                (locate-result-edge result) (first (vertex-edges v0)))
          (return result))
        (when (and (= (vec2-x p1) x) (= (vec2-y p1) y))
          (setf (locate-result-exact-vertex-p result) t
                (locate-result-vertex result) v1
                (locate-result-edge result) (first (vertex-edges v1)))
          (return result))
        (when (and (= (vec2-x p2) x) (= (vec2-y p2) y))
          (setf (locate-result-exact-vertex-p result) t
                (locate-result-vertex result) v2
                (locate-result-edge result) (first (vertex-edges v2)))
          (return result))

        ;; Check if inside triangle
        (when (and (left-of-p target e1)
                   (left-of-p target e2)
                   (left-of-p target e3))
          (setf (locate-result-edge result) e1)
          (return result))

        ;; Find next edge to pass through
        (let ((next nil))
          (dolist (e (list e1 e2 e3))
            (let ((j-dst (qe-dst (nth (mod (1+ (position e (list e1 e2 e3))) 3) (list e1 e2 e3)))))
              (when (and (left-of-p (vertex-pos j-dst) e)
                         (right-of-p target e))
                (setf next (qe-sym e))
                (return))))

          ;; If not found, check if on an edge
          (unless next
            (dolist (e (list e1 e2 e3))
              (when (on-line-p target (vertex-pos (quad-edge-org e))
                               (vertex-pos (qe-dst e)))
                (setf (locate-result-on-edge-p result) t
                      (locate-result-edge result) e)
                (return-from locate-triangle-point result))))

          (setf e1 next))))))

(defun insert-point-internal (ctx pos)
  "Insert a point at POS and return the vertex."
  (declare (type triangulation-context ctx)
           (type vec2 pos))
  (let ((locate (locate-triangle-point ctx (vec2-x pos) (vec2-y pos))))
    (when (locate-result-exact-vertex-p locate)
      (return-from insert-point-internal (locate-result-vertex locate)))

    (let ((new-vertex (create-vertex ctx pos))
          (start-side (locate-result-edge locate)))

      ;; If on edge, remove that edge first
      (when (locate-result-on-edge-p locate)
        (setf start-side (qe-oprev start-side))
        (destroy-edge ctx (qe-onext start-side)))

      ;; Subdivide the region
      (let ((diagonal (create-edge ctx (quad-edge-org start-side) new-vertex)))
        (splice start-side diagonal)
        (let ((start diagonal)
              (side start-side))
          (loop
            (setf diagonal (connect ctx side (qe-sym diagonal)))
            (setf side (qe-oprev diagonal))
            (when (eq (qe-lnext side) start)
              (return))))

        ;; Flip edges to restore Delaunay property
        (let ((check-stack nil)
              (side start-side))
          (loop
            (push side check-stack)
            (setf side (qe-oprev (qe-lnext side)))
            (when (eq side start-side)
              (return)))

          (flip-until-done check-stack)))

      new-vertex)))

;;; ============================================================================
;;; Segment Insertion
;;; ============================================================================

(defun insert-segment (id vert1 vert2)
  "Insert a constrained segment between VERT1 and VERT2 with ID."
  (let ((p (vertex-pos vert1))
        (q (vertex-pos vert2))
        (intersectings nil))  ; Used as a queue

    ;; Gather intersecting edges
    (let ((pivot vert1)
          (r (first (vertex-edges vert1))))
      (let ((l (qe-onext r)))
        (loop while (not (eq pivot vert2)) do
          (if (< (orientation (vertex-pos (qe-dst r)) p q) 0.0)
              ;; Right
              (loop while (< (orientation (vertex-pos (qe-dst l)) p q) 0.0) do
                (setf r l)
                (setf l (qe-onext l)))
              ;; Left or on-line
              (loop do
                (setf l r)
                (setf r (qe-oprev r))
                while (>= (orientation (vertex-pos (qe-dst r)) p q) 0.0)))

          (cond
            ;; L is on-line
            ((= (orientation (vertex-pos (qe-dst l)) p q) 0.0)
             (setf l (qe-sym (qe-dnext l)))
             (setf r (qe-lprev r))
             (setf pivot (quad-edge-org l)))

            ;; Normal case
            (t
             (loop
               (setf intersectings (nconc intersectings (list (qe-lnext r))))
               (let ((s (quad-edge-org (qe-dnext (qe-dnext l)))))
                 (cond
                   ((< (orientation (vertex-pos s) p q) 0.0)
                    (setf l (qe-dnext l))
                    (setf r (qe-oprev l)))
                   ((> (orientation (vertex-pos s) p q) 0.0)
                    (setf r (qe-dprev r))
                    (setf l (qe-onext r)))
                   (t  ; On-line
                    (setf r (qe-dnext (qe-dnext l)))
                    (setf l (qe-onext r))
                    (setf pivot s)
                    (return))))))))))

    ;; Flip intersecting edges
    (let ((flip-stack nil))
      (loop with made-progress = t
            while (and intersectings made-progress)
            do
        (setf made-progress nil)
        (let ((remaining nil))
          (loop while intersectings do
            (let ((e (pop intersectings)))
              (let ((a (vertex-pos (quad-edge-org e)))
                    (b (vertex-pos (qe-dst (qe-oprev e))))
                    (c (vertex-pos (qe-dst e)))
                    (d (vertex-pos (quad-edge-org (qe-lprev e)))))

                (when (edge-constrained-p (quad-edge-edge e))
                  (error "Intersecting constraints not allowed"))

                (if (convex-p a b c d)
                    (progn
                      (setf made-progress t)
                      (swap-edge e)
                      (if (< (* (orientation (vertex-pos (quad-edge-org e)) p q)
                                (orientation (vertex-pos (qe-dst e)) p q))
                             0.0d0)
                          (push e remaining)
                          (progn
                            (push e flip-stack)
                            (push (qe-sym e) flip-stack))))
                    (push e remaining)))))
          (setf intersectings (nreverse remaining))))

      ;; Assign ID to edges on the constraint
      (loop with cur = vert1
            while (not (eq cur vert2)) do
        (dolist (e (vertex-edges cur))
          (let ((nxt (qe-dst e)))
            (when (= (orientation (vertex-pos nxt) p q) 0.0)
              (let* ((x1 (- (vec2-x q) (vec2-x (vertex-pos cur))))
                     (y1 (- (vec2-y q) (vec2-y (vertex-pos cur))))
                     (x2 (- (vec2-x (vertex-pos nxt)) (vec2-x (vertex-pos cur))))
                     (y2 (- (vec2-y (vertex-pos nxt)) (vec2-y (vertex-pos cur)))))
                (when (> (+ (* x1 x2) (* y1 y2)) 0.0)
                  (push id (edge-ids (quad-edge-edge e)))
                  (setf cur nxt)
                  (return)))))))

      (flip-until-done flip-stack))))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun make-context (&key super-triangle bounds (float-type 'single-float))
  "Create a new triangulation context.
   Either provide :SUPER-TRIANGLE as ((x1 y1) (x2 y2) (x3 y3))
   or :BOUNDS as ((min-x min-y) (max-x max-y)) to auto-compute."
  (declare (ignore float-type))  ; Currently only single-float supported
  (let ((ctx (make-triangulation-context)))
    (multiple-value-bind (x1 y1 x2 y2 x3 y3)
        (if super-triangle
            (values (coerce (first (first super-triangle)) 'single-float)
                    (coerce (second (first super-triangle)) 'single-float)
                    (coerce (first (second super-triangle)) 'single-float)
                    (coerce (second (second super-triangle)) 'single-float)
                    (coerce (first (third super-triangle)) 'single-float)
                    (coerce (second (third super-triangle)) 'single-float))
            (let* ((min-x (coerce (first (first bounds)) 'single-float))
                   (min-y (coerce (second (first bounds)) 'single-float))
                   (max-x (coerce (first (second bounds)) 'single-float))
                   (max-y (coerce (second (second bounds)) 'single-float))
                   (dx (- max-x min-x))
                   (dy (- max-y min-y))
                   (cx (/ (+ min-x max-x) 2.0))
                   (cy (/ (+ min-y max-y) 2.0))
                   (size (max dx dy))
                   (margin (* size 10.0)))  ; Large margin to ensure all points fit
              (values (- cx margin)
                      (- cy margin)
                      (+ cx margin)
                      (- cy margin)
                      cx
                      (+ cy (* margin 2.0)))))

      ;; Create super-triangle vertices
      (let* ((va (create-vertex ctx (make-vec2 x1 y1)))
             (vb (create-vertex ctx (make-vec2 x2 y2)))
             (vc (create-vertex ctx (make-vec2 x3 y3))))

        ;; Track super-triangle vertices
        (setf (triangulation-context-super-triangle-vertices ctx) (list va vb vc))

        ;; Create edges
        (let ((ab (create-edge ctx va vb))
              (bc (create-edge ctx vb vc))
              (ca (create-edge ctx vc va)))

          ;; Connect them
          (splice (qe-sym ab) bc)
          (splice (qe-sym bc) ca)
          (splice (qe-sym ca) ab))))

    ctx))

(defun insert-point (ctx x y)
  "Insert a point at (X, Y) into the triangulation."
  (declare (type triangulation-context ctx))
  (let ((pos (make-vec2 (coerce x 'single-float)
                        (coerce y 'single-float))))
    (insert-point-internal ctx pos)))

(defun insert-constraint (ctx x1 y1 x2 y2 &key id)
  "Insert a constrained edge from (X1, Y1) to (X2, Y2).
   ID is required for later removal."
  (declare (type triangulation-context ctx))
  (let ((p1 (make-vec2 (coerce x1 'single-float)
                       (coerce y1 'single-float)))
        (p2 (make-vec2 (coerce x2 'single-float)
                       (coerce y2 'single-float))))
    (let ((vtx1 (insert-point-internal ctx p1))
          (vtx2 (insert-point-internal ctx p2)))
      (insert-segment id vtx1 vtx2))))

(defun insert (ctx x1 y1 x2 y2 &key constrained id)
  "Unified insert function.
   If CONSTRAINED is T, insert as a constraint edge with ID.
   Otherwise, just insert both endpoints as points."
  (declare (type triangulation-context ctx))
  (if constrained
      (insert-constraint ctx x1 y1 x2 y2 :id id)
      (progn
        (insert-point ctx x1 y1)
        (insert-point ctx x2 y2))))

(defun remove-constraint (ctx id)
  "Remove the constraint with ID from the triangulation."
  (declare (type triangulation-context ctx))
  (let ((vertices-to-check nil))
    ;; Find edges with this ID and collect affected vertices
    (dolist (edge (triangulation-context-edges ctx))
      (when (member id (edge-ids edge))
        (setf (edge-ids edge) (delete id (edge-ids edge)))
        (let ((v0 (quad-edge-org (svref (edge-e edge) 0)))
              (v2 (quad-edge-org (svref (edge-e edge) 2))))
          (pushnew v0 vertices-to-check :test #'eq)
          (pushnew v2 vertices-to-check :test #'eq))))

    ;; Check if vertices should be removed
    (dolist (v vertices-to-check)
      (unless (member v (triangulation-context-super-triangle-vertices ctx))
        (let ((should-destroy t))
          (dolist (e (vertex-edges v))
            (when (edge-constrained-p (quad-edge-edge e))
              (setf should-destroy nil)
              (return)))
          (when should-destroy
            (destroy-vertex ctx v)))))))

(defun vertex-count (ctx)
  "Return the number of vertices in the triangulation."
  (declare (type triangulation-context ctx))
  (length (triangulation-context-vertices ctx)))

(defun edge-count (ctx)
  "Return the number of edges in the triangulation."
  (declare (type triangulation-context ctx))
  (length (triangulation-context-edges ctx)))

(defun triangle-count (ctx)
  "Return the number of triangles in the triangulation.
   Uses Euler's formula: F = 2 - V + E"
  (declare (type triangulation-context ctx))
  (- 2 (- (vertex-count ctx) (edge-count ctx))))

(defun get-triangles (ctx &key exclude-super-triangle as-points)
  "Return all triangles in the triangulation.
   If EXCLUDE-SUPER-TRIANGLE is T, filter out triangles touching the super-triangle.
   If AS-POINTS is T, return coordinate lists instead of triangle objects."
  (declare (type triangulation-context ctx))
  (let ((visited nil)
        (triangles nil)
        (super-verts (triangulation-context-super-triangle-vertices ctx)))

    ;; DFS to find all triangles
    (let ((stack (list (svref (edge-e (first (triangulation-context-edges ctx))) 0))))
      (loop while stack do
        (let* ((e1 (pop stack))
               (e2 (qe-lnext e1))
               (e3 (qe-lnext e2)))

          (unless (member e1 visited :test #'eq)
            (push e1 visited)
            (push e2 visited)
            (push e3 visited)

            (let ((v0 (quad-edge-org e1))
                  (v1 (quad-edge-org e2))
                  (v2 (quad-edge-org e3)))

              ;; Check if should include this triangle
              (unless (and exclude-super-triangle
                           (or (member v0 super-verts :test #'eq)
                               (member v1 super-verts :test #'eq)
                               (member v2 super-verts :test #'eq)))
                (if as-points
                    (push (list (list (vec2-x (vertex-pos v0))
                                      (vec2-y (vertex-pos v0)))
                                (list (vec2-x (vertex-pos v1))
                                      (vec2-y (vertex-pos v1)))
                                (list (vec2-x (vertex-pos v2))
                                      (vec2-y (vertex-pos v2))))
                          triangles)
                    (let ((tri (make-triangle)))
                      (setf (svref (triangle-edges tri) 0) e1)
                      (setf (svref (triangle-edges tri) 1) e2)
                      (setf (svref (triangle-edges tri) 2) e3)
                      (setf (aref (triangle-x tri) 0) (vec2-x (vertex-pos v0)))
                      (setf (aref (triangle-y tri) 0) (vec2-y (vertex-pos v0)))
                      (setf (aref (triangle-x tri) 1) (vec2-x (vertex-pos v1)))
                      (setf (aref (triangle-y tri) 1) (vec2-y (vertex-pos v1)))
                      (setf (aref (triangle-x tri) 2) (vec2-x (vertex-pos v2)))
                      (setf (aref (triangle-y tri) 2) (vec2-y (vertex-pos v2)))
                      (push tri triangles)))))

            ;; Add neighbors to stack
            (push (qe-sym e1) stack)
            (push (qe-sym e2) stack)
            (push (qe-sym e3) stack)))))

    (nreverse triangles)))

(defun locate-point (ctx x y)
  "Locate the triangle containing point (X, Y).
   Returns a triangle object."
  (declare (type triangulation-context ctx))
  (let* ((loc (locate-triangle-point ctx (coerce x 'single-float) (coerce y 'single-float)))
         (e1 (locate-result-edge loc)))
    (when e1
      (let* ((e2 (qe-lnext e1))
             (e3 (qe-lnext e2))
             (tri (make-triangle)))
        (setf (svref (triangle-edges tri) 0) e1)
        (setf (svref (triangle-edges tri) 1) e2)
        (setf (svref (triangle-edges tri) 2) e3)
        (setf (aref (triangle-x tri) 0) (vec2-x (vertex-pos (quad-edge-org e1))))
        (setf (aref (triangle-y tri) 0) (vec2-y (vertex-pos (quad-edge-org e1))))
        (setf (aref (triangle-x tri) 1) (vec2-x (vertex-pos (quad-edge-org e2))))
        (setf (aref (triangle-y tri) 1) (vec2-y (vertex-pos (quad-edge-org e2))))
        (setf (aref (triangle-x tri) 2) (vec2-x (vertex-pos (quad-edge-org e3))))
        (setf (aref (triangle-y tri) 2) (vec2-y (vertex-pos (quad-edge-org e3))))
        tri))))

(defun adjacent-triangles (triangle)
  "Return the three triangles adjacent to TRIANGLE."
  (declare (type triangle triangle))
  (let ((result nil))
    (dotimes (i 3)
      (let* ((e1 (qe-sym (svref (triangle-edges triangle) i)))
             (e2 (qe-lnext e1))
             (e3 (qe-lnext e2))
             (tri (make-triangle)))
        (setf (svref (triangle-edges tri) 0) e1)
        (setf (svref (triangle-edges tri) 1) e2)
        (setf (svref (triangle-edges tri) 2) e3)
        (setf (aref (triangle-x tri) 0) (vec2-x (vertex-pos (quad-edge-org e1))))
        (setf (aref (triangle-y tri) 0) (vec2-y (vertex-pos (quad-edge-org e1))))
        (setf (aref (triangle-x tri) 1) (vec2-x (vertex-pos (quad-edge-org e2))))
        (setf (aref (triangle-y tri) 1) (vec2-y (vertex-pos (quad-edge-org e2))))
        (setf (aref (triangle-x tri) 2) (vec2-x (vertex-pos (quad-edge-org e3))))
        (setf (aref (triangle-y tri) 2) (vec2-y (vertex-pos (quad-edge-org e3))))
        (push tri result)))
    (nreverse result)))

(defun constrained-p (edge)
  "Return T if EDGE is constrained."
  (typecase edge
    (edge (edge-constrained-p edge))
    (quad-edge (edge-constrained-p (quad-edge-edge edge)))
    (t nil)))

(defun constraint-ids (edge)
  "Return the list of constraint IDs on EDGE."
  (typecase edge
    (edge (edge-ids edge))
    (quad-edge (edge-ids (quad-edge-edge edge)))
    (t nil)))
