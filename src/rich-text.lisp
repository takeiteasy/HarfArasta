;;;; src/rich-text.lisp
;;;; Phase 2: Glyph outline extraction
;;;; Phase 3: Single glyph rendering (SDF/MSDF/mesh)

(in-package #:cl-rich-text)

(defun make-path-builder-draw-sink (builder)
  "Create a draw-sink that forwards HarfBuzz draw events to a trivial-sdf path-builder."
  (cl-rich-text/harfbuzz:make-draw-sink
   :move-to (lambda (x y)
              (trivial-sdf:path-move-to builder x y))
   :line-to (lambda (x y)
              (trivial-sdf:path-line-to builder x y))
   :quadratic-to (lambda (cx cy x y)
                   (trivial-sdf:path-quadratic-to builder cx cy x y))
   :cubic-to (lambda (cx0 cy0 cx1 cy1 x y)
               (trivial-sdf:path-cubic-to builder cx0 cy0 cx1 cy1 x y))
   :close-path (lambda ()
                 (trivial-sdf:path-close builder))))

(defun glyph-to-shape (font glyph-id)
  "Extract glyph outline from FONT as a trivial-sdf:shape, or NIL for blank glyphs.
FONT is an hb_font_t pointer. GLYPH-ID is the glyph codepoint (uint32)."
  (let* ((builder (trivial-sdf:make-path-builder))
         (sink (make-path-builder-draw-sink builder))
         (sink-id (cl-rich-text/harfbuzz:register-draw-data sink))
         (dfuncs (cl-rich-text/harfbuzz:make-hb-draw-funcs)))
    (unwind-protect
         (progn
           (cl-rich-text/harfbuzz:hb-font-draw-glyph
            font glyph-id dfuncs (cffi:make-pointer sink-id))
           (let ((shape (trivial-sdf:path-to-shape builder)))
             (if (null (trivial-sdf:shape-contours shape))
                 nil
                 shape)))
      (cl-rich-text/harfbuzz:hb-draw-funcs-destroy dfuncs)
      (cl-rich-text/harfbuzz:unregister-draw-data sink-id))))

(defmacro %with-font-from-path ((font-var path index) &body body)
  "Internal: Load a font from PATH with face INDEX and bind to FONT-VAR.
Manages blob, face, and font lifecycle. Sets scale to face upem."
  (let ((blob-var (gensym "BLOB"))
        (face-var (gensym "FACE"))
        (upem-var (gensym "UPEM")))
    `(let* ((,blob-var (cl-rich-text/harfbuzz:hb-blob-create-from-file
                        (namestring ,path)))
            (,face-var (cl-rich-text/harfbuzz:hb-face-create ,blob-var ,index))
            (,font-var (cl-rich-text/harfbuzz:hb-font-create ,face-var))
            (,upem-var (cl-rich-text/harfbuzz:hb-face-get-upem ,face-var)))
       (cl-rich-text/harfbuzz:hb-font-set-scale ,font-var ,upem-var ,upem-var)
       (unwind-protect
            (progn ,@body)
         (cl-rich-text/harfbuzz:hb-font-destroy ,font-var)
         (cl-rich-text/harfbuzz:hb-face-destroy ,face-var)
         (cl-rich-text/harfbuzz:hb-blob-destroy ,blob-var)))))

(defmacro with-font ((font-var first-arg &rest args) &body body)
  "Load a font and bind it to FONT-VAR for the duration of BODY.
Manages blob, face, and font lifecycle. Sets scale to face upem.

Two calling conventions:
  (with-font (f \"/path/to/font.ttf\") ...)           ; path mode
  (with-font (f \"/path/to/font.ttf\" :index 1) ...)  ; path mode with index
  (with-font (f :family \"Helvetica\") ...)            ; discovery mode
  (with-font (f :family \"Roboto\" :weight :bold) ...) ; discovery mode"
  (if (keywordp first-arg)
      ;; Discovery mode: all args are a font-discovery plist
      (let ((path-var (gensym "PATH")))
        `(let ((,path-var (find-font-path ,first-arg ,@args)))
           (%with-font-from-path (,font-var ,path-var 0) ,@body)))
      ;; Path mode: first-arg is the path, remaining args are &key index
      (let ((index (or (getf args :index) 0)))
        `(%with-font-from-path (,font-var ,first-arg ,index) ,@body))))

;;;; ——— Phase 4.5: Font discovery ———

(defun find-font-path (&rest args &key family weight slant spacing stretch)
  "Find a system font file path matching the given criteria.
Returns a pathname. Signals an error if no matching font is found."
  (declare (ignore family weight slant spacing stretch))
  (org.shirakumo.font-discovery:init)
  (let ((font-obj (apply #'org.shirakumo.font-discovery:find-font args)))
    (unless font-obj
      (error "No font found matching ~{~S ~S~^, ~}" args))
    (org.shirakumo.font-discovery:file font-obj)))

;;;; ——— Phase 3: Single glyph rendering ———

;;; SDF/MSDF rendering

(defun shape-to-sdf (shape width height &key (range 4.0d0) (padding 2.0))
  "Render SHAPE to a 1-channel SDF bitmap of WIDTH x HEIGHT pixels.
RANGE is the distance field range in shape units. PADDING is border padding in pixels."
  (multiple-value-bind (scale tx ty)
      (trivial-sdf:auto-scale-shape shape width height :padding padding)
    (trivial-sdf:generate-sdf-from-shape shape width height
                                          :range range :scale scale
                                          :translate-x tx :translate-y ty)))

(defun shape-to-msdf (shape width height &key (range 4.0d0) (padding 2.0))
  "Render SHAPE to a 3-channel MSDF bitmap of WIDTH x HEIGHT pixels.
RANGE is the distance field range in shape units. PADDING is border padding in pixels."
  (multiple-value-bind (scale tx ty)
      (trivial-sdf:auto-scale-shape shape width height :padding padding)
    (trivial-sdf:generate-msdf shape width height
                                :range range :scale scale
                                :translate-x tx :translate-y ty)))

;;; Mesh generation internals

(declaim (inline %cross-2d))
(defun %cross-2d (ax ay bx by)
  "2D cross product of vectors (AX,AY) and (BX,BY)."
  (- (* ax by) (* ay bx)))

(defun %linearize-contour (contour segments-per-edge)
  "Sample edges of CONTOUR into line-segment points.
Returns a list of (x . y) cons pairs forming a closed polygon."
  (let ((points nil))
    (dolist (edge (trivial-sdf:contour-edges contour))
      (let ((n (if (= (trivial-sdf:edge-segment-edge-type edge)
                      trivial-sdf:+edge-type-linear+)
                   1
                   segments-per-edge)))
        (dotimes (i n)
          (let* ((t-param (/ (coerce i 'double-float) (coerce n 'double-float)))
                 (pt (trivial-sdf:edge-point edge t-param)))
            (push (cons (trivial-sdf:vec2-x pt) (trivial-sdf:vec2-y pt))
                  points)))))
    (nreverse points)))

(defun %point-in-shape-p (x y contour-polygons)
  "Test if point (X,Y) is inside the shape using non-zero winding number rule.
CONTOUR-POLYGONS is a list of point-lists from %linearize-contour."
  (let ((winding 0))
    (dolist (polygon contour-polygons)
      (let ((n (length polygon)))
        (when (> n 0)
          (loop for i from 0 below n
                for p1 = (nth i polygon)
                for p2 = (nth (mod (1+ i) n) polygon)
                for x1 single-float = (car p1)
                for y1 single-float = (cdr p1)
                for x2 single-float = (car p2)
                for y2 single-float = (cdr p2)
                do (cond
                     ;; Upward crossing
                     ((<= y1 y)
                      (when (> y2 y)
                        (when (> (%cross-2d (- x2 x1) (- y2 y1)
                                            (- x x1) (- y y1))
                                 0.0)
                          (incf winding))))
                     ;; Downward crossing
                     (t
                      (when (<= y2 y)
                        (when (< (%cross-2d (- x2 x1) (- y2 y1)
                                            (- x x1) (- y y1))
                                 0.0)
                          (decf winding)))))))))
    (not (zerop winding))))

(defun shape-to-mesh (shape &key (segments-per-edge 8))
  "Triangulate SHAPE into an indexed mesh via constrained Delaunay triangulation.
Returns (VALUES vertices indices) where VERTICES is a (simple-array single-float (*))
of interleaved x,y pairs and INDICES is a (simple-array (unsigned-byte 32) (*))
of triangle index triples. SEGMENTS-PER-EDGE controls curve sampling resolution."
  (let* ((contours (trivial-sdf:shape-contours shape))
         (contour-polygons (mapcar (lambda (c)
                                     (%linearize-contour c segments-per-edge))
                                   contours))
         ;; Compute bounding box
         (min-x most-positive-single-float)
         (min-y most-positive-single-float)
         (max-x most-negative-single-float)
         (max-y most-negative-single-float))
    ;; Find bounds from all linearized points
    (dolist (polygon contour-polygons)
      (dolist (pt polygon)
        (let ((px (car pt))
              (py (cdr pt)))
          (when (< px min-x) (setf min-x px))
          (when (< py min-y) (setf min-y py))
          (when (> px max-x) (setf max-x px))
          (when (> py max-y) (setf max-y py)))))
    ;; Add margin
    (let* ((margin (* 0.1 (max (- max-x min-x) (- max-y min-y) 1.0)))
           (ctx (tdt:make-context :bounds (list (list (- min-x margin) (- min-y margin))
                                                (list (+ max-x margin) (+ max-y margin)))))
           (constraint-id 0))
      ;; Insert contour edges as constraints
      (dolist (polygon contour-polygons)
        (let ((n (length polygon)))
          (loop for i from 0 below n
                for p1 = (nth i polygon)
                for p2 = (nth (mod (1+ i) n) polygon)
                do (tdt:insert-constraint ctx
                                          (car p1) (cdr p1)
                                          (car p2) (cdr p2)
                                          :id (incf constraint-id)))))
      ;; Get triangles and filter interior ones
      (let* ((triangles (tdt:get-triangles ctx
                                            :exclude-super-triangle t
                                            :as-points t))
             (interior-tris
               (remove-if-not
                (lambda (tri)
                  (let* ((p0 (first tri))
                         (p1 (second tri))
                         (p2 (third tri))
                         (cx (/ (+ (first p0) (first p1) (first p2)) 3.0))
                         (cy (/ (+ (second p0) (second p1) (second p2)) 3.0)))
                    (%point-in-shape-p cx cy contour-polygons)))
                triangles))
             ;; Build indexed mesh
             (vert-table (make-hash-table :test #'equal))
             (vert-list nil)
             (vert-count 0)
             (index-list nil))
        (dolist (tri interior-tris)
          (dolist (pt tri)
            (let ((key (cons (coerce (first pt) 'single-float)
                             (coerce (second pt) 'single-float))))
              (unless (gethash key vert-table)
                (setf (gethash key vert-table) vert-count)
                (push key vert-list)
                (incf vert-count))
              (push (gethash key vert-table) index-list))))
        ;; Build output arrays
        (let ((vertices (make-array (* 2 vert-count) :element-type 'single-float))
              (indices (make-array (length index-list) :element-type '(unsigned-byte 32))))
          (loop for vt in (nreverse vert-list)
                for i from 0 by 2
                do (setf (aref vertices i) (car vt)
                         (aref vertices (1+ i)) (cdr vt)))
          (loop for idx in (nreverse index-list)
                for i from 0
                do (setf (aref indices i) idx))
          (values vertices indices))))))

;;; Glyph-level convenience functions

(defun glyph-to-sdf (font glyph-id width height &key (range 4.0d0) (padding 2.0))
  "Render glyph GLYPH-ID from FONT as a 1-channel SDF bitmap, or NIL for blank glyphs."
  (let ((shape (glyph-to-shape font glyph-id)))
    (when shape
      (shape-to-sdf shape width height :range range :padding padding))))

(defun glyph-to-msdf (font glyph-id width height &key (range 4.0d0) (padding 2.0))
  "Render glyph GLYPH-ID from FONT as a 3-channel MSDF bitmap, or NIL for blank glyphs."
  (let ((shape (glyph-to-shape font glyph-id)))
    (when shape
      (shape-to-msdf shape width height :range range :padding padding))))

(defun glyph-to-mesh (font glyph-id &key (segments-per-edge 8))
  "Triangulate glyph GLYPH-ID from FONT into an indexed mesh, or NIL for blank glyphs.
Returns (VALUES vertices indices) — see SHAPE-TO-MESH for details."
  (let ((shape (glyph-to-shape font glyph-id)))
    (when shape
      (shape-to-mesh shape :segments-per-edge segments-per-edge))))

;;;; ——— Phase 4: String shaping + rendering ———

(defstruct shaped-glyph
  "Per-glyph shaping output from HarfBuzz."
  (glyph-id 0 :type fixnum)
  (cluster 0 :type fixnum)
  (x-advance 0 :type fixnum)
  (y-advance 0 :type fixnum)
  (x-offset 0 :type fixnum)
  (y-offset 0 :type fixnum))

(defun shape-text (font text &key direction script language)
  "Shape TEXT using FONT via HarfBuzz. Returns a list of shaped-glyph structs.
DIRECTION is a keyword (:ltr :rtl :ttb :btt) or NIL for auto-detection.
SCRIPT is a 4-char tag string (e.g. \"Latn\") or NIL.
LANGUAGE is a BCP-47 string (e.g. \"en\") or NIL.
Unset properties are guessed by hb_buffer_guess_segment_properties."
  (let ((buf (cl-rich-text/harfbuzz:hb-buffer-create)))
    (unwind-protect
         (progn
           (cl-rich-text/harfbuzz:hb-buffer-add-utf8 buf text -1 0 -1)
           (when direction
             (cl-rich-text/harfbuzz:hb-buffer-set-direction buf direction))
           (when script
             (cl-rich-text/harfbuzz:hb-buffer-set-script
              buf (cl-rich-text/harfbuzz:hb-script-from-tag script)))
           (when language
             (cl-rich-text/harfbuzz:hb-buffer-set-language
              buf (cl-rich-text/harfbuzz:hb-language-from-string language -1)))
           (cl-rich-text/harfbuzz:hb-buffer-guess-segment-properties buf)
           (cl-rich-text/harfbuzz:hb-shape font buf (cffi:null-pointer) 0)
           (cffi:with-foreign-object (len :uint)
             (let ((infos (cl-rich-text/harfbuzz:hb-buffer-get-glyph-infos buf len))
                   (count (cffi:mem-ref len :uint)))
               (let ((positions (cl-rich-text/harfbuzz:hb-buffer-get-glyph-positions buf len)))
                 (loop for i from 0 below count
                       for info = (cffi:mem-aptr infos
                                                 '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t) i)
                       for pos = (cffi:mem-aptr positions
                                                '(:struct cl-rich-text/harfbuzz:hb-glyph-position-t) i)
                       collect (make-shaped-glyph
                                :glyph-id (cffi:foreign-slot-value
                                           info '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t)
                                           'cl-rich-text/harfbuzz::codepoint)
                                :cluster (cffi:foreign-slot-value
                                          info '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t)
                                          'cl-rich-text/harfbuzz::cluster)
                                :x-advance (cffi:foreign-slot-value
                                            pos '(:struct cl-rich-text/harfbuzz:hb-glyph-position-t)
                                            'cl-rich-text/harfbuzz::x-advance)
                                :y-advance (cffi:foreign-slot-value
                                            pos '(:struct cl-rich-text/harfbuzz:hb-glyph-position-t)
                                            'cl-rich-text/harfbuzz::y-advance)
                                :x-offset (cffi:foreign-slot-value
                                           pos '(:struct cl-rich-text/harfbuzz:hb-glyph-position-t)
                                           'cl-rich-text/harfbuzz::x-offset)
                                :y-offset (cffi:foreign-slot-value
                                           pos '(:struct cl-rich-text/harfbuzz:hb-glyph-position-t)
                                           'cl-rich-text/harfbuzz::y-offset)))))))
      (cl-rich-text/harfbuzz:hb-buffer-destroy buf))))

(defun %map-shaped-glyphs (shaped-glyphs render-fn)
  "Walk SHAPED-GLYPHS accumulating cursor position, calling RENDER-FN for each glyph.
RENDER-FN receives (font-not-needed glyph-id) and should return render data or NIL.
Returns a list of (pen-x pen-y . render-data) for non-NIL results."
  (let ((cursor-x 0)
        (cursor-y 0)
        (results nil))
    (dolist (sg shaped-glyphs)
      (let ((pen-x (+ cursor-x (shaped-glyph-x-offset sg)))
            (pen-y (+ cursor-y (shaped-glyph-y-offset sg))))
        (let ((data (funcall render-fn (shaped-glyph-glyph-id sg))))
          (when data
            (push (list* pen-x pen-y data) results))))
      (incf cursor-x (shaped-glyph-x-advance sg))
      (incf cursor-y (shaped-glyph-y-advance sg)))
    (nreverse results)))

(defun text-to-meshes (font text &key direction script language (segments-per-edge 8))
  "Shape TEXT and triangulate each visible glyph into a positioned mesh.
Returns a list of (x y vertices indices). See SHAPE-TO-MESH for array formats."
  (let ((glyphs (shape-text font text :direction direction
                                       :script script :language language)))
    (%map-shaped-glyphs
     glyphs
     (lambda (glyph-id)
       (let ((shape (glyph-to-shape font glyph-id)))
         (when shape
           (multiple-value-bind (vertices indices)
               (shape-to-mesh shape :segments-per-edge segments-per-edge)
             (list vertices indices))))))))

(defun text-to-sdfs (font text glyph-width glyph-height
                     &key direction script language (range 4.0d0) (padding 2.0))
  "Shape TEXT and render each visible glyph as a positioned SDF bitmap.
Returns a list of (x y bitmap). See SHAPE-TO-SDF for bitmap format."
  (let ((glyphs (shape-text font text :direction direction
                                       :script script :language language)))
    (%map-shaped-glyphs
     glyphs
     (lambda (glyph-id)
       (let ((shape (glyph-to-shape font glyph-id)))
         (when shape
           (list (shape-to-sdf shape glyph-width glyph-height
                               :range range :padding padding))))))))

(defun text-to-msdfs (font text glyph-width glyph-height
                      &key direction script language (range 4.0d0) (padding 2.0))
  "Shape TEXT and render each visible glyph as a positioned MSDF bitmap.
Returns a list of (x y bitmap). See SHAPE-TO-MSDF for bitmap format."
  (let ((glyphs (shape-text font text :direction direction
                                       :script script :language language)))
    (%map-shaped-glyphs
     glyphs
     (lambda (glyph-id)
       (let ((shape (glyph-to-shape font glyph-id)))
         (when shape
           (list (shape-to-msdf shape glyph-width glyph-height
                                :range range :padding padding))))))))

;;;; ——— Phase 4.5: Bitmap rendering ———

(declaim (inline %smoothstep))
(defun %smoothstep (edge0 edge1 x)
  "Hermite smoothstep interpolation. Returns 0.0 for x<=edge0, 1.0 for x>=edge1,
and smooth transition in between."
  (let ((t-val (max 0.0 (min 1.0 (/ (- x edge0) (- edge1 edge0))))))
    (* t-val t-val (- 3.0 (* 2.0 t-val)))))

(defun shape-to-bitmap (shape width height &key (range 4.0d0) (padding 2.0) (edge-width nil))
  "Render SHAPE to a 1-channel anti-aliased grayscale bitmap of WIDTH x HEIGHT pixels.
Generates an SDF internally then applies smoothstep thresholding.
RANGE and PADDING are passed to the SDF generator.
EDGE-WIDTH controls anti-aliasing sharpness; NIL auto-computes ~1px of AA."
  (let* ((sdf (shape-to-sdf shape width height :range range :padding padding))
         (sdf-data (trivial-sdf:bitmap-data sdf))
         (bmp (trivial-sdf:make-bitmap width height 1))
         (bmp-data (trivial-sdf:bitmap-data bmp))
         (w (if edge-width
                (coerce edge-width 'single-float)
                ;; Auto: ~1 pixel of AA in SDF-normalized space
                (/ 1.0 (max 1.0 (coerce (min width height) 'single-float))))))
    (declare (type single-float w))
    ;; SDF encoding: 0.0=inside, 0.5=edge, 1.0=outside
    ;; Bitmap output: 1.0=inside, 0.0=outside (standard coverage)
    (loop for i from 0 below (length sdf-data)
          for sd single-float = (aref sdf-data i)
          do (setf (aref bmp-data i)
                   (coerce (- 1.0 (%smoothstep (- 0.5 w) (+ 0.5 w) sd)) 'single-float)))
    bmp))

(defun glyph-to-bitmap (font glyph-id width height &key (range 4.0d0) (padding 2.0) (edge-width nil))
  "Render glyph GLYPH-ID from FONT as a 1-channel anti-aliased bitmap, or NIL for blank glyphs."
  (let ((shape (glyph-to-shape font glyph-id)))
    (when shape
      (shape-to-bitmap shape width height :range range :padding padding :edge-width edge-width))))

(defun text-to-bitmaps (font text glyph-width glyph-height
                        &key direction script language (range 4.0d0) (padding 2.0) (edge-width nil))
  "Shape TEXT and render each visible glyph as a positioned anti-aliased bitmap.
Returns a list of (x y bitmap). See SHAPE-TO-BITMAP for bitmap format."
  (let ((glyphs (shape-text font text :direction direction
                                       :script script :language language)))
    (%map-shaped-glyphs
     glyphs
     (lambda (glyph-id)
       (let ((shape (glyph-to-shape font glyph-id)))
         (when shape
           (list (shape-to-bitmap shape glyph-width glyph-height
                                  :range range :padding padding
                                  :edge-width edge-width))))))))
