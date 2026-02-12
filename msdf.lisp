;;;; msdf.lisp
;;;; Multi-channel signed distance field generation
;;;; Based on msdf.h/msdf.c by Viktor Chlumský

(in-package #:harfarasta)

;;; Constants

(defconstant +msdf-edge-threshold+ 0.02d0)

;;; Pixel clash detection

(defun pixel-clash-p (a b threshold)
  "Detect if two adjacent pixels have a rendering clash.
   A and B are vec3 RGB values, THRESHOLD is the clash detection threshold."
  (declare (type vec3 a b) (type double-float threshold))
  (let* ((ax (vec3-x a)) (ay (vec3-y a)) (az (vec3-z a))
         (bx (vec3-x b)) (by (vec3-y b)) (bz (vec3-z b))
         ;; Boolean flags for > 0.5
         (ax>  (> ax 0.5)) (ay>  (> ay 0.5)) (az>  (> az 0.5))
         (bx>  (> bx 0.5)) (by>  (> by 0.5)) (bz>  (> bz 0.5))
         ;; Boolean flags for < 0.5
         (ax<  (< ax 0.5)) (ay<  (< ay 0.5)) (az<  (< az 0.5))
         (bx<  (< bx 0.5)) (by<  (< by 0.5)) (bz<  (< bz 0.5))
         ;; Count channels > 0.5 to determine inside/outside
         (a-in (>= (+ (if ax> 1 0) (if ay> 1 0) (if az> 1 0)) 2))
         (b-in (>= (+ (if bx> 1 0) (if by> 1 0) (if bz> 1 0)) 2)))
    ;; Must have same inside/outside classification
    (unless (eq a-in b-in)
      (return-from pixel-clash-p nil))
    ;; Must not be all-in or all-out
    (when (or (and ax> ay> az>)
              (and ax< ay< az<)
              (and bx> by> bz>)
              (and bx< by< bz<))
      (return-from pixel-clash-p nil))
    ;; Check for clash pattern - channels must differ in their > and < relationships
    (let (aa ab ba bb ac bc)
      (cond
        ;; Red channel differs
        ((and (not (eq ax> bx>)) (not (eq ax< bx<)))
         (setf aa ax ba bx)
         (cond
           ;; Green channel also differs
           ((and (not (eq ay> by>)) (not (eq ay< by<)))
            (setf ab ay bb by ac az bc bz))
           ;; Blue channel differs
           ((and (not (eq az> bz>)) (not (eq az< bz<)))
            (setf ab az bb bz ac ay bc by))
           (t (return-from pixel-clash-p nil))))
        ;; Green and blue channels differ
        ((and (not (eq ay> by>)) (not (eq ay< by<))
              (not (eq az> bz>)) (not (eq az< bz<)))
         (setf aa ay ba by ab az bb bz ac ax bc bx))
        (t (return-from pixel-clash-p nil)))
      ;; Check if this is a true clash
      (and (>= (abs (- aa ba)) threshold)
           (>= (abs (- ab bb)) threshold)
           (>= (abs (- ac 0.5)) (abs (- bc 0.5)))))))

;;; MSDF generation

(defun generate-msdf (shape width height
                      &key (range 4.0d0) (scale 1.0d0)
                           (translate-x 0.0d0) (translate-y 0.0d0)
                           output)
  "Generate a multi-channel signed distance field from a vector shape.

   SHAPE is the input shape with colored edges.
   WIDTH and HEIGHT are output dimensions in pixels.
   RANGE is the distance field range in shape units.
   SCALE is the shape-to-pixel scale factor.
   TRANSLATE-X and TRANSLATE-Y offset the shape.
   OUTPUT is an optional 3-channel bitmap to write to.

   Returns a bitmap with R, G, B channels containing signed distances."
  (declare (type shape shape)
           (type fixnum width height)
           (type double-float range scale translate-x translate-y))

  ;; Prepare shape
  (normalize-shape shape)
  (color-shape-edges shape)

  (let* ((out (or output (make-bitmap width height 3)))
         (out-data (bitmap-data out))
         (contours (shape-contours shape))
         (contour-count (length contours))
         (inv-range (/ 1.0d0 range))
         ;; Pre-allocate per-contour distance storage
         (contour-sd (make-array contour-count)))

    ;; Initialize contour-sd array
    (dotimes (i contour-count)
      (setf (aref contour-sd i) (make-multi-distance)))

    ;; Process each pixel
    (loop for y from 0 below height
          do (loop for x from 0 below width
                   ;; Convert pixel to shape coordinates
                   for px = (coerce (/ (+ x 0.5d0 translate-x) scale) 'single-float)
                   for py = (coerce (/ (+ y 0.5d0 translate-y) scale) 'single-float)
                   for p = (vec2 px py)
                   do
                      ;; Initialize per-pixel tracking
                      (let ((sr (make-edge-point))
                            (sg (make-edge-point))
                            (sb (make-edge-point))
                            (d (abs +negative-infinity+))
                            (neg-dist (- +negative-infinity+))
                            (pos-dist +infinity+)
                            (winding 0))

                        ;; Initialize edge points
                        (setf (edge-point-min-distance sr) (make-signed-distance +negative-infinity+ 1.0d0)
                              (edge-point-min-distance sg) (make-signed-distance +negative-infinity+ 1.0d0)
                              (edge-point-min-distance sb) (make-signed-distance +negative-infinity+ 1.0d0))

                        ;; Process each contour
                        (loop for j from 0 below contour-count
                              for contour = (nth j contours)
                              for contour-winding = (contour-winding contour)
                              do
                                 ;; Per-contour tracking
                                 (let ((r (make-edge-point))
                                       (g (make-edge-point))
                                       (b (make-edge-point)))
                                   (setf (edge-point-min-distance r) (make-signed-distance +negative-infinity+ 1.0d0)
                                         (edge-point-min-distance g) (make-signed-distance +negative-infinity+ 1.0d0)
                                         (edge-point-min-distance b) (make-signed-distance +negative-infinity+ 1.0d0))

                                   ;; Process each edge
                                   (dolist (edge (contour-edges contour))
                                     (multiple-value-bind (dist param)
                                         (edge-signed-distance edge p)
                                       (let ((color (edge-segment-color edge)))
                                         ;; Update per-channel nearest edge
                                         (when (and (plusp (logand color +edge-color-red+))
                                                    (signed-distance< dist (edge-point-min-distance r)))
                                           (setf (edge-point-min-distance r) dist
                                                 (edge-point-near-edge r) edge
                                                 (edge-point-near-param r) param))
                                         (when (and (plusp (logand color +edge-color-green+))
                                                    (signed-distance< dist (edge-point-min-distance g)))
                                           (setf (edge-point-min-distance g) dist
                                                 (edge-point-near-edge g) edge
                                                 (edge-point-near-param g) param))
                                         (when (and (plusp (logand color +edge-color-blue+))
                                                    (signed-distance< dist (edge-point-min-distance b)))
                                           (setf (edge-point-min-distance b) dist
                                                 (edge-point-near-edge b) edge
                                                 (edge-point-near-param b) param)))))

                                   ;; Update global nearest
                                   (when (signed-distance< (edge-point-min-distance r)
                                                           (edge-point-min-distance sr))
                                     (setf (edge-point-min-distance sr) (edge-point-min-distance r)
                                           (edge-point-near-edge sr) (edge-point-near-edge r)
                                           (edge-point-near-param sr) (edge-point-near-param r)))
                                   (when (signed-distance< (edge-point-min-distance g)
                                                           (edge-point-min-distance sg))
                                     (setf (edge-point-min-distance sg) (edge-point-min-distance g)
                                           (edge-point-near-edge sg) (edge-point-near-edge g)
                                           (edge-point-near-param sg) (edge-point-near-param g)))
                                   (when (signed-distance< (edge-point-min-distance b)
                                                           (edge-point-min-distance sb))
                                     (setf (edge-point-min-distance sb) (edge-point-min-distance b)
                                           (edge-point-near-edge sb) (edge-point-near-edge b)
                                           (edge-point-near-param sb) (edge-point-near-param b)))

                                   ;; Compute median distance for this contour
                                   (let ((med-min-dist (abs (median
                                                             (signed-distance-dist (edge-point-min-distance r))
                                                             (signed-distance-dist (edge-point-min-distance g))
                                                             (signed-distance-dist (edge-point-min-distance b))))))
                                     (when (< med-min-dist d)
                                       (setf d med-min-dist
                                             winding (- contour-winding))))

                                   ;; Apply pseudo-distance correction
                                   (when (edge-point-near-edge r)
                                     (distance-to-pseudo-distance (edge-point-min-distance r) p
                                                                  (edge-point-near-param r)
                                                                  (edge-point-near-edge r)))
                                   (when (edge-point-near-edge g)
                                     (distance-to-pseudo-distance (edge-point-min-distance g) p
                                                                  (edge-point-near-param g)
                                                                  (edge-point-near-edge g)))
                                   (when (edge-point-near-edge b)
                                     (distance-to-pseudo-distance (edge-point-min-distance b) p
                                                                  (edge-point-near-param b)
                                                                  (edge-point-near-edge b)))

                                   ;; Store contour distances
                                   (let ((csd (aref contour-sd j))
                                         (med-dist (median (signed-distance-dist (edge-point-min-distance r))
                                                           (signed-distance-dist (edge-point-min-distance g))
                                                           (signed-distance-dist (edge-point-min-distance b)))))
                                     (setf (multi-distance-r csd) (signed-distance-dist (edge-point-min-distance r))
                                           (multi-distance-g csd) (signed-distance-dist (edge-point-min-distance g))
                                           (multi-distance-b csd) (signed-distance-dist (edge-point-min-distance b))
                                           (multi-distance-med csd) med-dist)

                                     ;; Track positive/negative distances
                                     (when (and (> contour-winding 0)
                                                (>= med-dist 0.0d0)
                                                (< (abs med-dist) (abs pos-dist)))
                                       (setf pos-dist med-dist))
                                     (when (and (< contour-winding 0)
                                                (<= med-dist 0.0d0)
                                                (< (abs med-dist) (abs neg-dist)))
                                       (setf neg-dist med-dist)))))

                        ;; Apply pseudo-distance to global nearest
                        (when (edge-point-near-edge sr)
                          (distance-to-pseudo-distance (edge-point-min-distance sr) p
                                                       (edge-point-near-param sr)
                                                       (edge-point-near-edge sr)))
                        (when (edge-point-near-edge sg)
                          (distance-to-pseudo-distance (edge-point-min-distance sg) p
                                                       (edge-point-near-param sg)
                                                       (edge-point-near-edge sg)))
                        (when (edge-point-near-edge sb)
                          (distance-to-pseudo-distance (edge-point-min-distance sb) p
                                                       (edge-point-near-param sb)
                                                       (edge-point-near-edge sb)))

                        ;; Select final distance based on winding rules
                        (let ((msd (make-multi-distance)))
                          (setf (multi-distance-r msd) +infinity+
                                (multi-distance-g msd) +infinity+
                                (multi-distance-b msd) +infinity+
                                (multi-distance-med msd) +infinity+)

                          (cond
                            ;; Positive winding dominates
                            ((and (>= pos-dist 0.0d0) (<= (abs pos-dist) (abs neg-dist)))
                             (setf (multi-distance-med msd) +negative-infinity+
                                   winding 1)
                             (loop for i from 0 below contour-count
                                   for contour = (nth i contours)
                                   for csd = (aref contour-sd i)
                                   when (and (> (contour-winding contour) 0)
                                             (> (multi-distance-med csd) (multi-distance-med msd))
                                             (< (abs (multi-distance-med csd)) (abs neg-dist)))
                                     do (setf (multi-distance-r msd) (multi-distance-r csd)
                                              (multi-distance-g msd) (multi-distance-g csd)
                                              (multi-distance-b msd) (multi-distance-b csd)
                                              (multi-distance-med msd) (multi-distance-med csd))))

                            ;; Negative winding dominates
                            ((and (<= neg-dist 0.0d0) (<= (abs neg-dist) (abs pos-dist)))
                             (setf (multi-distance-med msd) +infinity+
                                   winding -1)
                             (loop for i from 0 below contour-count
                                   for contour = (nth i contours)
                                   for csd = (aref contour-sd i)
                                   when (and (< (contour-winding contour) 0)
                                             (< (multi-distance-med csd) (multi-distance-med msd))
                                             (< (abs (multi-distance-med csd)) (abs pos-dist)))
                                     do (setf (multi-distance-r msd) (multi-distance-r csd)
                                              (multi-distance-g msd) (multi-distance-g csd)
                                              (multi-distance-b msd) (multi-distance-b csd)
                                              (multi-distance-med msd) (multi-distance-med csd)))))

                          ;; Check for opposite winding contours
                          (loop for i from 0 below contour-count
                                for contour = (nth i contours)
                                for csd = (aref contour-sd i)
                                when (and (/= (contour-winding contour) winding)
                                          (< (abs (multi-distance-med csd)) (abs (multi-distance-med msd))))
                                  do (setf (multi-distance-r msd) (multi-distance-r csd)
                                           (multi-distance-g msd) (multi-distance-g csd)
                                           (multi-distance-b msd) (multi-distance-b csd)
                                           (multi-distance-med msd) (multi-distance-med csd)))

                          ;; Use global nearest if it matches the median
                          (when (= (median (signed-distance-dist (edge-point-min-distance sr))
                                           (signed-distance-dist (edge-point-min-distance sg))
                                           (signed-distance-dist (edge-point-min-distance sb)))
                                   (multi-distance-med msd))
                            (setf (multi-distance-r msd) (signed-distance-dist (edge-point-min-distance sr))
                                  (multi-distance-g msd) (signed-distance-dist (edge-point-min-distance sg))
                                  (multi-distance-b msd) (signed-distance-dist (edge-point-min-distance sb))))

                          ;; Fallback to global nearest if no distance found
                          (when (>= (abs (multi-distance-med msd)) (abs +infinity+))
                            (setf (multi-distance-r msd) (signed-distance-dist (edge-point-min-distance sr))
                                  (multi-distance-g msd) (signed-distance-dist (edge-point-min-distance sg))
                                  (multi-distance-b msd) (signed-distance-dist (edge-point-min-distance sb))))

                          ;; Write to output with clamping
                          ;; Convention: higher values = inside, lower = outside
                          ;; So we use: value = 0.5 - distance/range (negated distance)
                          (let ((index (* 3 (+ x (* y width)))))
                            (setf (aref out-data index)
                                  (clampf (coerce (- 0.5d0 (* (multi-distance-r msd) inv-range)) 'single-float) 0.0 1.0)
                                  (aref out-data (+ index 1))
                                  (clampf (coerce (- 0.5d0 (* (multi-distance-g msd) inv-range)) 'single-float) 0.0 1.0)
                                  (aref out-data (+ index 2))
                                  (clampf (coerce (- 0.5d0 (* (multi-distance-b msd) inv-range)) 'single-float) 0.0 1.0)))))))

    ;; Error correction - fix pixel clashes
    (let ((clashes nil)
          (tx (/ +msdf-edge-threshold+ (* scale range)))
          (ty (/ +msdf-edge-threshold+ (* scale range))))
      ;; Find clashes
      (loop for y from 0 below height
            do (loop for x from 0 below width
                     for idx = (* 3 (+ x (* y width)))
                     for pixel = (vec3 (aref out-data idx)
                                       (aref out-data (+ idx 1))
                                       (aref out-data (+ idx 2)))
                     do ;; Check neighbors
                        (when (or (and (> x 0)
                                       (let* ((nidx (* 3 (+ (1- x) (* y width))))
                                              (neighbor (vec3 (aref out-data nidx)
                                                              (aref out-data (+ nidx 1))
                                                              (aref out-data (+ nidx 2)))))
                                         (pixel-clash-p pixel neighbor tx)))
                                  (and (< x (1- width))
                                       (let* ((nidx (* 3 (+ (1+ x) (* y width))))
                                              (neighbor (vec3 (aref out-data nidx)
                                                              (aref out-data (+ nidx 1))
                                                              (aref out-data (+ nidx 2)))))
                                         (pixel-clash-p pixel neighbor tx)))
                                  (and (> y 0)
                                       (let* ((nidx (* 3 (+ x (* (1- y) width))))
                                              (neighbor (vec3 (aref out-data nidx)
                                                              (aref out-data (+ nidx 1))
                                                              (aref out-data (+ nidx 2)))))
                                         (pixel-clash-p pixel neighbor ty)))
                                  (and (< y (1- height))
                                       (let* ((nidx (* 3 (+ x (* (1+ y) width))))
                                              (neighbor (vec3 (aref out-data nidx)
                                                              (aref out-data (+ nidx 1))
                                                              (aref out-data (+ nidx 2)))))
                                         (pixel-clash-p pixel neighbor ty))))
                          (push (cons x y) clashes))))

      ;; Fix clashes by setting all channels to median
      (dolist (clash clashes)
        (let* ((x (car clash))
               (y (cdr clash))
               (idx (* 3 (+ x (* y width))))
               (med (coerce (median (coerce (aref out-data idx) 'double-float)
                                    (coerce (aref out-data (+ idx 1)) 'double-float)
                                    (coerce (aref out-data (+ idx 2)) 'double-float))
                            'single-float)))
          (setf (aref out-data idx) med
                (aref out-data (+ idx 1)) med
                (aref out-data (+ idx 2)) med))))

    out))

;;; Single-channel SDF from vector shape

(defun generate-sdf-from-shape (shape width height
                                &key (range 4.0d0) (scale 1.0d0)
                                     (translate-x 0.0d0) (translate-y 0.0d0)
                                     output)
  "Generate a single-channel signed distance field from a vector shape.

   Same parameters as GENERATE-MSDF but returns a 1-channel bitmap."
  (declare (type shape shape)
           (type fixnum width height)
           (type double-float range scale translate-x translate-y))

  ;; Use MSDF generation and take median
  (let* ((msdf (generate-msdf shape width height
                              :range range :scale scale
                              :translate-x translate-x :translate-y translate-y))
         (msdf-data (bitmap-data msdf))
         (out (or output (make-bitmap width height 1)))
         (out-data (bitmap-data out)))

    (loop for y from 0 below height
          do (loop for x from 0 below width
                   for msdf-idx = (* 3 (+ x (* y width)))
                   for out-idx = (+ x (* y width))
                   do (setf (aref out-data out-idx)
                            (coerce (median (coerce (aref msdf-data msdf-idx) 'double-float)
                                            (coerce (aref msdf-data (+ msdf-idx 1)) 'double-float)
                                            (coerce (aref msdf-data (+ msdf-idx 2)) 'double-float))
                                    'single-float))))
    out))
