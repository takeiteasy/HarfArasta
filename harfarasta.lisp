;;;; src/rich-text.lisp

(in-package #:harfarasta)

(defun make-path-builder-draw-sink (builder)
  "Create a draw-sink that forwards HarfBuzz draw events to a sdf path-builder."
  (hb:make-draw-sink
   :move-to (lambda (x y)
              (path-move-to builder x y))
   :line-to (lambda (x y)
              (path-line-to builder x y))
   :quadratic-to (lambda (cx cy x y)
                   (path-quadratic-to builder cx cy x y))
   :cubic-to (lambda (cx0 cy0 cx1 cy1 x y)
               (path-cubic-to builder cx0 cy0 cx1 cy1 x y))
   :close-path (lambda ()
                 (path-close builder))))

(defun glyph-to-shape (font glyph-id)
  "Extract glyph outline from FONT as a shape, or NIL for blank glyphs.
FONT is an hb_font_t pointer. GLYPH-ID is the glyph codepoint (uint32)."
  (let* ((builder (make-path-builder))
         (sink (make-path-builder-draw-sink builder))
         (sink-id (hb:register-draw-data sink))
         (dfuncs (hb:make-hb-draw-funcs)))
    (unwind-protect
         (progn
           (hb:hb-font-draw-glyph
            font glyph-id dfuncs (cffi:make-pointer sink-id))
           (let ((shape (path-to-shape builder)))
             (if (null (shape-contours shape))
                 nil
                 shape)))
      (hb:hb-draw-funcs-destroy dfuncs)
      (hb:unregister-draw-data sink-id))))

;;;; ——— Font format detection ———

(defun %read-file-bytes (path)
  "Read an entire file into a (simple-array (unsigned-byte 8) (*))."
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let* ((len (file-length s))
           (buf (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      buf)))

(defun %font-format-magic (path)
  "Read first 4 bytes of PATH and return :woff1, :woff2, or :raw."
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((b0 (read-byte s nil 0))
          (b1 (read-byte s nil 0))
          (b2 (read-byte s nil 0))
          (b3 (read-byte s nil 0)))
      (let ((sig (logior (ash b0 24) (ash b1 16) (ash b2 8) b3)))
        (cond
          ((= sig #x774F4646) :woff1)   ; "wOFF"
          ((= sig #x774F4632) :woff2)   ; "wOF2"
          (t :raw))))))

(defun %font-format-magic-from-bytes (bytes)
  "Detect font format from first 4 bytes of BYTES array.
Returns :woff1, :woff2, :otf, :ttf, or :raw."
  (when (< (length bytes) 4)
    (return-from %font-format-magic-from-bytes :raw))
  (let ((sig (logior (ash (aref bytes 0) 24)
                     (ash (aref bytes 1) 16)
                     (ash (aref bytes 2) 8)
                     (aref bytes 3))))
    (cond
      ((= sig #x774F4646) :woff1)     ; "wOFF"
      ((= sig #x774F4632) :woff2)     ; "wOF2"
      ((= sig #x4F54544F) :otf)       ; "OTTO"
      ((= sig #x00010000) :ttf)       ; TrueType
      (t :raw))))

(defun identify-font-format (bytes)
  "Identify the font format from a byte vector.
Returns :ttf, :otf, :woff1, or :woff2, or NIL if unrecognized."
  (let ((fmt (%font-format-magic-from-bytes bytes)))
    (if (eq fmt :raw) nil fmt)))

(defun %decode-font-bytes (bytes)
  "Decode WOFF1/WOFF2 byte vector to raw font bytes. Returns decoded bytes,
or the original BYTES unchanged for TTF/OTF/raw formats."
  (let ((fmt (%font-format-magic-from-bytes bytes)))
    (case fmt
      (:woff1 (woff1-decode-to-vector bytes))
      (:woff2 (harfarasta/woff2:woff2-decode-to-vector bytes))
      (otherwise bytes))))

(defun %decode-font-file (path)
  "If PATH is WOFF1/WOFF2, decode to TTF/OTF bytes. Returns byte vector or NIL for raw fonts."
  (ecase (%font-format-magic path)
    (:woff1
     (woff1-decode-to-vector (%read-file-bytes path)))
    (:woff2
     (harfarasta/woff2:woff2-decode-to-vector (%read-file-bytes path)))
    (:raw nil)))

(defun %create-blob-from-bytes (byte-vector)
  "Create an hb_blob_t from a Lisp byte vector. The foreign memory is managed by HarfBuzz."
  (let* ((len (length byte-vector))
         (foreign-buf (cffi:foreign-alloc :uint8 :count len)))
    (loop for i below len
          do (setf (cffi:mem-aref foreign-buf :uint8 i) (aref byte-vector i)))
    (hb:hb-blob-create foreign-buf len :writable
                       foreign-buf (cffi:callback %free-foreign-buf-cb))))

(cffi:defcallback %free-foreign-buf-cb :void ((user-data :pointer))
  (cffi:foreign-free user-data))

;;;; ——— Persistent font management ———

(defun create-font (path &key (index 0))
  "Load a font from PATH (TTF, OTF, WOFF, or WOFF2).
Returns (VALUES font face blob upem). All three pointers must be freed with DESTROY-FONT."
  (let* ((decoded (%decode-font-file path))
         (blob (if decoded
                   (%create-blob-from-bytes decoded)
                   (hb:hb-blob-create-from-file (namestring path))))
         (face (hb:hb-face-create blob index))
         (font (hb:hb-font-create face))
         (upem (hb:hb-face-get-upem face)))
    (hb:hb-font-set-scale font upem upem)
    (values font face blob upem)))

(defun create-font-from-bytes (bytes &key (index 0))
  "Load a font from a byte vector (TTF, OTF, WOFF, or WOFF2).
Returns (VALUES font face blob upem). All three pointers must be freed with DESTROY-FONT."
  (let* ((decoded (%decode-font-bytes bytes))
         (blob (%create-blob-from-bytes decoded))
         (face (hb:hb-face-create blob index))
         (font (hb:hb-font-create face))
         (upem (hb:hb-face-get-upem face)))
    (hb:hb-font-set-scale font upem upem)
    (values font face blob upem)))

(defun destroy-font (font face blob)
  "Destroy font resources created by CREATE-FONT."
  (hb:hb-font-destroy font)
  (hb:hb-face-destroy face)
  (hb:hb-blob-destroy blob))

;;;; ——— Font loading macro (internal) ———

(defmacro %with-font-from-path ((font-var path index) &body body)
  "Internal: Load a font from PATH with face INDEX and bind to FONT-VAR.
Manages blob, face, and font lifecycle. Sets scale to face upem.
Transparently handles WOFF1 and WOFF2 formats."
  (let ((blob-var (gensym "BLOB"))
        (face-var (gensym "FACE"))
        (upem-var (gensym "UPEM"))
        (decoded-var (gensym "DECODED")))
    `(let* ((,decoded-var (%decode-font-file ,path))
            (,blob-var (if ,decoded-var
                           (%create-blob-from-bytes ,decoded-var)
                           (hb:hb-blob-create-from-file
                            (namestring ,path))))
            (,face-var (hb:hb-face-create ,blob-var ,index))
            (,font-var (hb:hb-font-create ,face-var))
            (,upem-var (hb:hb-face-get-upem ,face-var)))
       (hb:hb-font-set-scale ,font-var ,upem-var ,upem-var)
       (unwind-protect
            (progn ,@body)
         (hb:hb-font-destroy ,font-var)
         (hb:hb-face-destroy ,face-var)
         (hb:hb-blob-destroy ,blob-var)))))

(defmacro %with-font-from-bytes ((font-var bytes-form index) &body body)
  "Internal: Load a font from a byte vector with face INDEX and bind to FONT-VAR.
Manages blob, face, and font lifecycle. Sets scale to face upem.
Transparently handles WOFF1 and WOFF2 formats."
  (let ((blob-var (gensym "BLOB"))
        (face-var (gensym "FACE"))
        (upem-var (gensym "UPEM"))
        (decoded-var (gensym "DECODED")))
    `(let* ((,decoded-var (%decode-font-bytes ,bytes-form))
            (,blob-var (%create-blob-from-bytes ,decoded-var))
            (,face-var (hb:hb-face-create ,blob-var ,index))
            (,font-var (hb:hb-font-create ,face-var))
            (,upem-var (hb:hb-face-get-upem ,face-var)))
       (hb:hb-font-set-scale ,font-var ,upem-var ,upem-var)
       (unwind-protect
            (progn ,@body)
         (hb:hb-font-destroy ,font-var)
         (hb:hb-face-destroy ,face-var)
         (hb:hb-blob-destroy ,blob-var)))))

(defmacro with-font ((font-var first-arg &rest args) &body body)
  "Load a font and bind it to FONT-VAR for the duration of BODY.
Manages blob, face, and font lifecycle. Sets scale to face upem.

Calling conventions:
  (with-font (f \"/path/to/font.ttf\") ...)              ; path mode
  (with-font (f \"/path/to/font.ttf\" :index 1) ...)     ; path mode with index
  (with-font (f :family \"Helvetica\") ...)               ; discovery mode
  (with-font (f :family \"Roboto\" :weight :bold) ...)    ; discovery mode
  (with-font (f :bytes my-byte-vector) ...)              ; byte vector mode
  (with-font (f :bytes my-byte-vector :index 1) ...)     ; byte vector mode with index"
  (if (keywordp first-arg)
      (cond
        ;; Byte vector mode
        ((eq first-arg :bytes)
         (let* ((bytes-form (first args))
                (index (or (getf (rest args) :index) 0)))
           `(%with-font-from-bytes (,font-var ,bytes-form ,index) ,@body)))
        ;; Discovery mode: all args are a font-discovery plist
        (t
         (let ((path-var (gensym "PATH")))
           `(let ((,path-var (find-font-path ,first-arg ,@args)))
              (%with-font-from-path (,font-var ,path-var 0) ,@body)))))
      ;; Path mode: first-arg is the path, remaining args are &key index
      (let ((index (or (getf args :index) 0)))
        `(%with-font-from-path (,font-var ,first-arg ,index) ,@body))))


(defun find-font-path (&rest args &key family weight slant spacing stretch)
  "Find a system font file path matching the given criteria.
Returns a pathname. Signals an error if no matching font is found."
  (declare (ignore family weight slant spacing stretch))
  (org.shirakumo.font-discovery:init)
  (let ((font-obj (apply #'org.shirakumo.font-discovery:find-font args)))
    (unless font-obj
      (error "No font found matching ~{~S ~S~^, ~}" args))
    (org.shirakumo.font-discovery:file font-obj)))

;;;; ———  3: Single glyph rendering ———

;;; SDF/MSDF rendering

(defun shape-to-sdf (shape width height &key (range 4.0d0) (padding 2.0))
  "Render SHAPE to a 1-channel SDF bitmap of WIDTH x HEIGHT pixels.
RANGE is the distance field range in shape units. PADDING is border padding in pixels."
  (multiple-value-bind (scale tx ty)
      (auto-scale-shape shape width height :padding padding)
    (generate-sdf-from-shape shape width height
                                          :range range :scale scale
                                          :translate-x tx :translate-y ty)))

(defun shape-to-msdf (shape width height &key (range 4.0d0) (padding 2.0))
  "Render SHAPE to a 3-channel MSDF bitmap of WIDTH x HEIGHT pixels.
RANGE is the distance field range in shape units. PADDING is border padding in pixels."
  (multiple-value-bind (scale tx ty)
      (auto-scale-shape shape width height :padding padding)
    (generate-msdf shape width height
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
    (dolist (edge (contour-edges contour))
      (let ((n (if (= (edge-segment-edge-type edge)
                      +edge-type-linear+)
                   1
                   segments-per-edge)))
        (dotimes (i n)
          (let* ((t-param (/ (coerce i 'double-float) (coerce n 'double-float)))
                 (pt (edge-point edge t-param)))
            (push (cons (vec2-x pt) (vec2-y pt))
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

(defun %extrude-mesh (vertices-2d indices-2d contour-outlines depth)
  "Extrude a 2D mesh into 3D. VERTICES-2D is a flat array of x,y pairs.
INDICES-2D is the triangle index array. CONTOUR-OUTLINES is a list of lists,
each inner list containing vertex indices forming a closed contour outline.
DEPTH is the extrusion depth along Z.
Returns (VALUES vertices-3d indices-3d)."
  (let* ((n-verts (/ (length vertices-2d) 2))
         (n-face-indices (length indices-2d))
         ;; Count side wall triangles: 2 per contour edge
         (n-side-edges (loop for contour in contour-outlines sum (length contour)))
         (n-side-indices (* n-side-edges 6))
         ;; Front + back vertices, each with x,y,z
         (vertices-3d (make-array (* 2 n-verts 3) :element-type 'single-float))
         (indices-3d (make-array (+ (* 2 n-face-indices) n-side-indices)
                                 :element-type '(unsigned-byte 32)))
         (depth-f (coerce depth 'single-float))
         (idx-pos 0))
    ;; Front face vertices at z=0
    (loop for i from 0 below n-verts
          for src = (* i 2)
          for dst = (* i 3)
          do (setf (aref vertices-3d dst) (aref vertices-2d src)
                   (aref vertices-3d (+ dst 1)) (aref vertices-2d (+ src 1))
                   (aref vertices-3d (+ dst 2)) 0.0))
    ;; Back face vertices at z=depth
    (loop for i from 0 below n-verts
          for src = (* i 2)
          for dst = (* (+ i n-verts) 3)
          do (setf (aref vertices-3d dst) (aref vertices-2d src)
                   (aref vertices-3d (+ dst 1)) (aref vertices-2d (+ src 1))
                   (aref vertices-3d (+ dst 2)) depth-f))
    ;; Front face indices (same winding)
    (loop for i from 0 below n-face-indices
          do (setf (aref indices-3d idx-pos) (aref indices-2d i))
             (incf idx-pos))
    ;; Back face indices (reversed winding, offset by n-verts)
    (loop for i from 0 below n-face-indices by 3
          do (setf (aref indices-3d idx-pos) (+ (aref indices-2d i) n-verts)
                   (aref indices-3d (+ idx-pos 1)) (+ (aref indices-2d (+ i 2)) n-verts)
                   (aref indices-3d (+ idx-pos 2)) (+ (aref indices-2d (+ i 1)) n-verts))
             (incf idx-pos 3))
    ;; Side walls: two triangles per contour edge
    (dolist (contour contour-outlines)
      (let ((n (length contour)))
        (loop for i from 0 below n
              for v0 = (nth i contour)
              for v1 = (nth (mod (1+ i) n) contour)
              for v0-back = (+ v0 n-verts)
              for v1-back = (+ v1 n-verts)
              do ;; Triangle 1: v0, v1, v1-back
                 (setf (aref indices-3d idx-pos) v0
                       (aref indices-3d (+ idx-pos 1)) v1
                       (aref indices-3d (+ idx-pos 2)) v1-back)
                 (incf idx-pos 3)
                 ;; Triangle 2: v0, v1-back, v0-back
                 (setf (aref indices-3d idx-pos) v0
                       (aref indices-3d (+ idx-pos 1)) v1-back
                       (aref indices-3d (+ idx-pos 2)) v0-back)
                 (incf idx-pos 3))))
    (values vertices-3d indices-3d)))

(defun shape-to-mesh (shape &key (segments-per-edge 8) depth)
  "Triangulate SHAPE into an indexed mesh via constrained Delaunay triangulation.
Returns (VALUES vertices indices) where VERTICES is a (simple-array single-float (*))
of interleaved x,y pairs (2D) or x,y,z triples (3D when DEPTH is non-NIL)
and INDICES is a (simple-array (unsigned-byte 32) (*)) of triangle index triples.
SEGMENTS-PER-EDGE controls curve sampling resolution.
When DEPTH is a number, the mesh is extruded along Z with front face at z=0
and back face at z=DEPTH, with side walls connecting the contour edges."
  (let* ((contours (shape-contours shape))
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
           (ctx (make-context :bounds (list (list (- min-x margin) (- min-y margin))
                                                (list (+ max-x margin) (+ max-y margin)))))
           (constraint-id 0))
      ;; Insert contour edges as constraints
      (dolist (polygon contour-polygons)
        (let ((n (length polygon)))
          (loop for i from 0 below n
                for p1 = (nth i polygon)
                for p2 = (nth (mod (1+ i) n) polygon)
                do (insert-constraint ctx
                                          (car p1) (cdr p1)
                                          (car p2) (cdr p2)
                                          :id (incf constraint-id)))))
      ;; Get triangles and filter interior ones
      (let* ((triangles (get-triangles ctx
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
        ;; Build 2D output arrays
        (let ((vertices (make-array (* 2 vert-count) :element-type 'single-float))
              (indices (make-array (length index-list) :element-type '(unsigned-byte 32))))
          (loop for vt in (nreverse vert-list)
                for i from 0 by 2
                do (setf (aref vertices i) (car vt)
                         (aref vertices (1+ i)) (cdr vt)))
          (loop for idx in (nreverse index-list)
                for i from 0
                do (setf (aref indices i) idx))
          (if depth
              ;; Extrude to 3D: collect contour outline vertex indices
              (let ((contour-outlines
                      (mapcar (lambda (polygon)
                                (mapcar (lambda (pt)
                                          (let ((key (cons (coerce (car pt) 'single-float)
                                                           (coerce (cdr pt) 'single-float))))
                                            (gethash key vert-table)))
                                        polygon))
                              contour-polygons)))
                (%extrude-mesh vertices indices contour-outlines depth))
              (values vertices indices)))))))

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

(defun glyph-to-mesh (font glyph-id &key (segments-per-edge 8) depth)
  "Triangulate glyph GLYPH-ID from FONT into an indexed mesh, or NIL for blank glyphs.
Returns (VALUES vertices indices) — see SHAPE-TO-MESH for details."
  (let ((shape (glyph-to-shape font glyph-id)))
    (when shape
      (shape-to-mesh shape :segments-per-edge segments-per-edge :depth depth))))

;;; Fast mesh generation via earcut

(defun %polygon-signed-area (polygon)
  "Signed area of POLYGON (list of (x . y)). Positive = CCW, negative = CW."
  (let ((sum 0.0d0)
        (n (length polygon)))
    (declare (type double-float sum))
    (loop for i from 0 below n
          for p1 = (nth i polygon)
          for p2 = (nth (mod (1+ i) n) polygon)
          do (incf sum (* (- (float (car p1) 1.0d0)
                             (float (car p2) 1.0d0))
                          (+ (float (cdr p1) 1.0d0)
                             (float (cdr p2) 1.0d0)))))
    (/ sum 2.0d0)))

(defun %point-in-polygon-p (x y polygon)
  "Ray-casting point-in-polygon test for point (X,Y) in POLYGON (list of (x . y))."
  (let ((inside nil)
        (n (length polygon)))
    (loop for i from 0 below n
          for p1 = (nth i polygon)
          for p2 = (nth (mod (1+ i) n) polygon)
          for x1 single-float = (car p1)
          for y1 single-float = (cdr p1)
          for x2 single-float = (car p2)
          for y2 single-float = (cdr p2)
          do (when (and (not (eq (> y1 y) (> y2 y)))
                        (< x (+ x1 (/ (* (- y y1) (- x2 x1))
                                       (- y2 y1)))))
               (setf inside (not inside))))
    inside))

(defun shape-to-mesh-fast (shape &key (segments-per-edge 8) depth)
  "Triangulate SHAPE into an indexed mesh via ear-clipping (earcut).
Faster than SHAPE-TO-MESH but produces less uniform triangles.
Returns (VALUES vertices indices) — same format as SHAPE-TO-MESH.
SEGMENTS-PER-EDGE controls curve sampling resolution.
When DEPTH is a number, the mesh is extruded along Z."
  (let* ((contours (shape-contours shape))
         (contour-polygons (mapcar (lambda (c)
                                     (%linearize-contour c segments-per-edge))
                                   contours))
         ;; Classify contours by signed area
         (contour-areas (mapcar #'%polygon-signed-area contour-polygons))
         ;; Positive area = CCW = outer contour; negative = CW = hole
         (outers nil)
         (holes nil))
    ;; Separate outers and holes
    ;; TrueType convention: CW (negative area) = outer, CCW (positive) = hole
    ;; OpenType/PostScript: CCW (positive area) = outer, CW (negative) = hole
    ;; Detect convention from the largest contour (by absolute area)
    (let ((largest-area 0.0d0))
      (dolist (a contour-areas)
        (when (> (abs a) (abs largest-area))
          (setf largest-area a)))
      ;; If largest contour has negative area, it's TrueType convention (CW=outer)
      ;; If positive, it's PostScript convention (CCW=outer)
      (let ((outer-positive-p (> largest-area 0.0d0)))
        (loop for polygon in contour-polygons
              for area in contour-areas
              for idx from 0
              do (if (if outer-positive-p (> area 0.0d0) (< area 0.0d0))
                     (push (cons idx polygon) outers)
                     (push (cons idx polygon) holes)))))
    (setf outers (nreverse outers)
          holes (nreverse holes))
    ;; For each outer contour, find which holes belong to it
    (let ((all-vertices nil)
          (all-indices nil)
          (global-vert-offset 0))
      (dolist (outer-entry outers)
        (let* ((outer-polygon (cdr outer-entry))
               ;; Find holes contained in this outer contour
               (contained-holes
                 (loop for hole-entry in holes
                       for hole-polygon = (cdr hole-entry)
                       for first-pt = (first hole-polygon)
                       when (and first-pt
                                 (%point-in-polygon-p (car first-pt) (cdr first-pt)
                                                      outer-polygon))
                         collect hole-polygon))
               ;; Build flat coords: outer first, then holes
               (n-outer (length outer-polygon))
               (hole-starts nil)
               (total-verts n-outer))
          ;; Calculate total vertex count and hole start indices
          (dolist (hole contained-holes)
            (push total-verts hole-starts)
            (incf total-verts (length hole)))
          (setf hole-starts (nreverse hole-starts))
          ;; Build flat coordinate array
          (let ((flat-coords (make-array (* total-verts 2) :element-type 'single-float)))
            ;; Outer ring
            (loop for pt in outer-polygon
                  for i from 0
                  do (setf (aref flat-coords (* i 2)) (coerce (car pt) 'single-float)
                           (aref flat-coords (1+ (* i 2))) (coerce (cdr pt) 'single-float)))
            ;; Hole rings
            (let ((offset n-outer))
              (dolist (hole contained-holes)
                (loop for pt in hole
                      for i from offset
                      do (setf (aref flat-coords (* i 2)) (coerce (car pt) 'single-float)
                               (aref flat-coords (1+ (* i 2))) (coerce (cdr pt) 'single-float)))
                (incf offset (length hole))))
            ;; Triangulate
            (let ((tri-indices (earcut flat-coords hole-starts)))
              (when (> (length tri-indices) 0)
                ;; Remap indices to global vertex space
                (loop for i from 0 below (length tri-indices)
                      do (push (+ (aref tri-indices i) global-vert-offset) all-indices))
                ;; Add vertices
                (loop for i from 0 below total-verts
                      do (push (cons (aref flat-coords (* i 2))
                                     (aref flat-coords (1+ (* i 2))))
                               all-vertices))))
            (incf global-vert-offset total-verts))))
      ;; Build output arrays
      (setf all-vertices (nreverse all-vertices)
            all-indices (nreverse all-indices))
      (let* ((n-verts (length all-vertices))
             (n-indices (length all-indices))
             (vertices (make-array (* 2 n-verts) :element-type 'single-float))
             (indices (make-array n-indices :element-type '(unsigned-byte 32))))
        (loop for vt in all-vertices
              for i from 0 by 2
              do (setf (aref vertices i) (car vt)
                       (aref vertices (1+ i)) (cdr vt)))
        (loop for idx in all-indices
              for i from 0
              do (setf (aref indices i) idx))
        (if depth
            ;; Extrude to 3D
            (let ((contour-outlines
                    (let ((offset 0)
                          (outlines nil))
                      (dolist (outer-entry outers)
                        (let* ((outer-polygon (cdr outer-entry))
                               (n-outer (length outer-polygon))
                               (contained-holes
                                 (loop for hole-entry in holes
                                       for hole-polygon = (cdr hole-entry)
                                       for first-pt = (first hole-polygon)
                                       when (and first-pt
                                                 (%point-in-polygon-p (car first-pt) (cdr first-pt)
                                                                      outer-polygon))
                                         collect hole-polygon)))
                          ;; Outer contour outline
                          (push (loop for i from offset below (+ offset n-outer) collect i) outlines)
                          ;; Hole contour outlines
                          (let ((h-offset (+ offset n-outer)))
                            (dolist (hole contained-holes)
                              (push (loop for i from h-offset below (+ h-offset (length hole)) collect i)
                                    outlines)
                              (incf h-offset (length hole))))
                          (incf offset (+ n-outer (reduce #'+ contained-holes :key #'length :initial-value 0)))))
                      (nreverse outlines))))
              (%extrude-mesh vertices indices contour-outlines depth))
            (values vertices indices))))))

(defun glyph-to-mesh-fast (font glyph-id &key (segments-per-edge 8) depth)
  "Triangulate glyph GLYPH-ID from FONT via ear-clipping, or NIL for blank glyphs.
Returns (VALUES vertices indices) — see SHAPE-TO-MESH-FAST for details."
  (let ((shape (glyph-to-shape font glyph-id)))
    (when shape
      (shape-to-mesh-fast shape :segments-per-edge segments-per-edge :depth depth))))

(defstruct shaped-glyph
  "Per-glyph shaping output from HarfBuzz."
  (glyph-id 0 :type fixnum)
  (cluster 0 :type fixnum)
  (x-advance 0 :type fixnum)
  (y-advance 0 :type fixnum)
  (x-offset 0 :type fixnum)
  (y-offset 0 :type fixnum)
  (font nil)                     ; NIL = use primary font
  (skip nil))                    ; T = synthetic layout glyph; advance cursor but don't render

;;;; ——— Fallback font state ———

(defvar *fallback-fonts* nil
  "List of hb_font_t pointers tried in order when a glyph is missing.
Off by default (NIL). Users manage font lifecycle via create-font/destroy-font.")

(defmacro with-fallback-fonts (fonts &body body)
  "Bind *FALLBACK-FONTS* to FONTS for the duration of BODY."
  `(let ((*fallback-fonts* ,fonts)) ,@body))

;;;; ——— Font inspection ———

(defun font-has-char-p (font char-or-codepoint)
  "Return T if FONT contains a glyph for CHAR-OR-CODEPOINT (character or integer codepoint)."
  (let ((cp (etypecase char-or-codepoint
               (character (char-code char-or-codepoint))
               (integer char-or-codepoint))))
    (cffi:with-foreign-object (gid :uint32)
      (not (zerop (hb:hb-font-get-nominal-glyph font cp gid))))))

(defun font-missing-chars (font string)
  "Return a list of characters in STRING that FONT does not have glyphs for."
  (loop for ch across string
        unless (font-has-char-p font ch)
          collect ch))

(defun font-monospace-p (font)
  "Return T if FONT is monospaced, as indicated by the OpenType 'post' table isFixedPitch field."
  (let* ((face (hb:hb-font-get-face font))
         (blob (hb:hb-face-reference-table face (hb:hb-tag #\p #\o #\s #\t))))
    (unwind-protect
         (cffi:with-foreign-object (len :uint)
           (let* ((data (hb:hb-blob-get-data blob len))
                  (n    (cffi:mem-ref len :uint)))
             (when (>= n 16)
               (not (zerop (logior (ash (cffi:mem-aref data :uint8 12) 24)
                                   (ash (cffi:mem-aref data :uint8 13) 16)
                                   (ash (cffi:mem-aref data :uint8 14)  8)
                                        (cffi:mem-aref data :uint8 15)))))))
      (hb:hb-blob-destroy blob))))

;;;; ——— Fallback font helpers ———

(defun %utf8-char-byte-length (ch)
  "Return the number of UTF-8 bytes needed to encode character CH."
  (let ((cp (char-code ch)))
    (cond ((< cp #x80)    1)
          ((< cp #x800)   2)
          ((< cp #x10000) 3)
          (t              4))))

(defun %cluster-codepoint-map (text)
  "Build a hash-table mapping UTF-8 byte offsets (cluster values) to codepoints."
  (let ((table (make-hash-table :test 'eql))
        (byte-offset 0))
    (loop for ch across text
          do (setf (gethash byte-offset table) (char-code ch))
             (incf byte-offset (%utf8-char-byte-length ch)))
    table))

(defun %apply-fallback-fonts (shaped-glyphs text fallback-fonts)
  "For each shaped-glyph with glyph-id=0, search FALLBACK-FONTS for a replacement.
Returns a new list; unresolved glyphs are left with glyph-id=0."
  (let ((cluster-map (%cluster-codepoint-map text)))
    (mapcar
     (lambda (sg)
       (if (zerop (shaped-glyph-glyph-id sg))
           (let* ((cp (gethash (shaped-glyph-cluster sg) cluster-map))
                  found-font
                  (found-gid 0))
             (when cp
               (dolist (fb fallback-fonts)
                 (cffi:with-foreign-object (gid-out :uint32)
                   (when (not (zerop (hb:hb-font-get-nominal-glyph fb cp gid-out)))
                     (setf found-font fb
                           found-gid (cffi:mem-ref gid-out :uint32))
                     (return)))))
             (if found-font
                 (make-shaped-glyph
                  :glyph-id found-gid
                  :cluster  (shaped-glyph-cluster sg)
                  :x-advance (hb:hb-font-get-glyph-h-advance found-font found-gid)
                  :y-advance (shaped-glyph-y-advance sg)
                  :x-offset  (shaped-glyph-x-offset sg)
                  :y-offset  (shaped-glyph-y-offset sg)
                  :font      found-font)
                 sg))
           sg))
     shaped-glyphs)))

;;;; ——— Multiline helpers ———

(defun %split-lines (text)
  "Split TEXT on #\\Newline. Returns list of strings (empty string for blank lines)."
  (loop for start = 0 then (1+ pos)
        for pos   = (position #\Newline text :start start)
        collect (subseq text start (or pos (length text)))
        while pos))

(defun %get-font-upem (font)
  "Return the units-per-em of FONT."
  (hb:hb-face-get-upem (hb:hb-font-get-face font)))

;;;; ——— Word wrap helpers ———

(defun %split-words (text)
  "Split TEXT on spaces/tabs; return list of non-empty word strings."
  (let ((words '())
        (start nil))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (if (or (char= c #\Space) (char= c #\Tab))
                 (when start
                   (push (subseq text start i) words)
                   (setf start nil))
                 (unless start (setf start i))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun %measure-line-width (font text &key direction script language fallback-fonts basic)
  "Return total x-advance of TEXT shaped with FONT, in font units."
  (reduce #'+ (shape-text font text
                          :direction direction
                          :script script
                          :language language
                          :fallback-fonts (or fallback-fonts *fallback-fonts*)
                          :basic basic)
          :key #'shaped-glyph-x-advance
          :initial-value 0))

(defun %wrap-paragraph (font text max-width &key direction script language fallback-fonts basic)
  "Greedy word-wrap a single paragraph (no #\\Newline). Returns list of line strings.
A word wider than MAX-WIDTH is placed on its own line without splitting."
  (let ((words (%split-words text)))
    (if (null words)
        (list "")
        (let ((lines '())
              (current-words '()))
          (dolist (word words)
            (let* ((candidate (if current-words
                                  (format nil "~{~A~^ ~}" (append current-words (list word)))
                                  word))
                   (width (%measure-line-width font candidate
                                              :direction direction
                                              :script script
                                              :language language
                                              :fallback-fonts fallback-fonts
                                              :basic basic)))
              (if (and (> width max-width) current-words)
                  (progn
                    (push (format nil "~{~A~^ ~}" current-words) lines)
                    (setf current-words (list word)))
                  (setf current-words (append current-words (list word))))))
          (when current-words
            (push (format nil "~{~A~^ ~}" current-words) lines))
          (nreverse lines)))))

(defun %utf8-byte-offset->char-index (string byte-offset)
  "Convert a UTF-8 byte offset to the corresponding character index in STRING."
  (let ((byte-count 0))
    (loop for i from 0 below (length string)
          when (>= byte-count byte-offset) return i
          do (let ((code (char-code (char string i))))
               (incf byte-count (cond ((< code #x80) 1)
                                      ((< code #x800) 2)
                                      ((< code #x10000) 3)
                                      (t 4))))
          finally (return (length string)))))

(defun %wrap-paragraph-glyph (font text max-width &key direction script language fallback-fonts basic)
  "Glyph-boundary wrapping for a single paragraph. Returns list of line strings.
Breaks at any glyph boundary when accumulated advance would exceed MAX-WIDTH."
  (let* ((shaped (shape-text font text
                              :direction direction
                              :script script
                              :language language
                              :fallback-fonts (or fallback-fonts *fallback-fonts*)
                              :basic basic))
         (split-byte-offsets '())
         (current-width 0))
    (dolist (sg shaped)
      (let ((advance (shaped-glyph-x-advance sg))
            (cluster (shaped-glyph-cluster sg)))
        (when (and (> (+ current-width advance) max-width)
                   (> current-width 0))
          (push cluster split-byte-offsets)
          (setf current-width 0))
        (incf current-width advance)))
    (if (null split-byte-offsets)
        (list text)
        (let* ((offsets (mapcar (lambda (bo) (%utf8-byte-offset->char-index text bo))
                                (nreverse split-byte-offsets)))
               (result '())
               (prev 0))
          (dolist (offset offsets)
            (push (subseq text prev offset) result)
            (setf prev offset))
          (push (subseq text prev) result)
          (nreverse result)))))

(defun %wrap-text (font text max-width wrap &key direction script language fallback-fonts basic)
  "Pre-process TEXT inserting #\\Newline at wrap boundaries.
WRAP is :word (greedy word-boundary) or :glyph (any glyph boundary).
Hard #\\Newline in TEXT are preserved; each resulting paragraph is wrapped independently."
  (let ((paragraphs (%split-lines text)))
    (with-output-to-string (s)
      (loop for para in paragraphs
            for first-p = t then nil
            unless first-p do (write-char #\Newline s)
            do (let ((lines (if (string= para "")
                                (list "")
                                (ecase wrap
                                  (:word  (%wrap-paragraph font para max-width
                                                           :direction direction
                                                           :script script
                                                           :language language
                                                           :fallback-fonts fallback-fonts
                                                           :basic basic))
                                  (:glyph (%wrap-paragraph-glyph font para max-width
                                                                  :direction direction
                                                                  :script script
                                                                  :language language
                                                                  :fallback-fonts fallback-fonts
                                                                  :basic basic))))))
                 (loop for line in lines
                       for first-l = t then nil
                       unless first-l do (write-char #\Newline s)
                       do (write-string line s)))))))

(defun shape-text (font text &key direction script language
                               alignment line-height
                               max-width (wrap :word)
                               (fallback-fonts *fallback-fonts*)
                               basic)
  "Shape TEXT using FONT via HarfBuzz. Returns a list of shaped-glyph structs.
DIRECTION is a keyword (:ltr :rtl :ttb :btt) or NIL for auto-detection.
SCRIPT is a 4-char tag string (e.g. \"Latn\") or NIL.
LANGUAGE is a BCP-47 string (e.g. \"en\") or NIL.
ALIGNMENT is :left (default), :center, or :right for multi-line layout.
LINE-HEIGHT is the Y distance between lines in font units (default = upem).
MAX-WIDTH, when non-NIL, triggers automatic line wrapping at that width in font units.
WRAP controls the wrapping mode: :word (default, break at word boundaries) or
:glyph (break at any glyph boundary, allowing mid-word breaks).
FALLBACK-FONTS is a list of hb_font_t pointers tried for missing glyphs.
BASIC, when non-NIL, skips HarfBuzz shaping and maps each codepoint directly to its
nominal glyph ID using hb_font_get_nominal_glyph and hb_font_get_glyph_h_advance.
Unset properties are guessed by hb_buffer_guess_segment_properties.
For multi-line TEXT (containing #\\Newline), returns a flat list that includes
synthetic skip glyphs encoding cursor jumps; consumers use %map-shaped-glyphs."
  (let* ((effective-text (if max-width
                             (%wrap-text font text max-width wrap
                                         :direction direction :script script
                                         :language language :fallback-fonts fallback-fonts
                                         :basic basic)
                             text)))
  (if (find #\Newline effective-text)
      ;; Multiline path: split, shape each line, build flat list with skip glyphs
      (let* ((lines (%split-lines effective-text))
             (lh (or line-height (%get-font-upem font)))
             (lines-glyphs (mapcar (lambda (line)
                                     (if (string= line "")
                                         nil
                                         (shape-text font line
                                                     :direction direction
                                                     :script script
                                                     :language language
                                                     :fallback-fonts fallback-fonts
                                                     :basic basic)))
                                   lines))
             (line-widths (mapcar (lambda (glyphs)
                                    (reduce #'+ glyphs
                                            :key #'shaped-glyph-x-advance
                                            :initial-value 0))
                                  lines-glyphs))
             (max-line-width (reduce #'max line-widths :initial-value 0))
             (result '())
             (prev-end-x 0))
        (loop for glyphs in lines-glyphs
              for lw in line-widths
              for line-idx from 0
              for start-x = (ecase (or alignment :left)
                               (:left   0)
                               (:center (floor (- max-line-width lw) 2))
                               (:right  (- max-line-width lw)))
              ;; Skip glyph: advance cursor to start of this line
              do (push (make-shaped-glyph
                        :x-advance (- start-x prev-end-x)
                        :y-advance (if (zerop line-idx) 0 lh)
                        :skip t)
                       result)
                 (dolist (sg glyphs) (push sg result))
                 (setf prev-end-x (+ start-x lw)))
        (nreverse result))
      ;; Single-line path
      (if basic
          ;; Basic: nominal glyph lookup + default h-advance; no ligatures/kerning/BiDi
          (let ((byte-pos 0))
            (let ((result
                    (loop for char across effective-text
                          for cp = (char-code char)
                          for byte-len = (cond ((< cp #x80) 1)
                                               ((< cp #x800) 2)
                                               ((< cp #x10000) 3)
                                               (t 4))
                          for cluster = byte-pos
                          do (incf byte-pos byte-len)
                          collect (cffi:with-foreign-object (gid :uint32)
                                    (let* ((found (not (zerop (hb:hb-font-get-nominal-glyph
                                                               font cp gid))))
                                           (glyph-id (if found (cffi:mem-ref gid :uint32) 0)))
                                      (make-shaped-glyph
                                       :glyph-id glyph-id
                                       :cluster cluster
                                       :x-advance (hb:hb-font-get-glyph-h-advance font glyph-id)
                                       :y-advance 0
                                       :x-offset 0
                                       :y-offset 0))))))
              (if fallback-fonts
                  (%apply-fallback-fonts result effective-text fallback-fonts)
                  result)))
          ;; Full HarfBuzz shaping
          (let ((buf (hb:hb-buffer-create)))
        (unwind-protect
             (progn
               (hb:hb-buffer-add-utf8 buf effective-text -1 0 -1)
               (when direction
                 (hb:hb-buffer-set-direction buf direction))
               (when script
                 (hb:hb-buffer-set-script
                  buf (hb:hb-script-from-tag script)))
               (when language
                 (hb:hb-buffer-set-language
                  buf (hb:hb-language-from-string language -1)))
               (hb:hb-buffer-guess-segment-properties buf)
               (hb:hb-shape font buf (cffi:null-pointer) 0)
               (let ((result
                       (cffi:with-foreign-object (len :uint)
                         (let ((infos (hb:hb-buffer-get-glyph-infos buf len))
                               (count (cffi:mem-ref len :uint)))
                           (let ((positions (hb:hb-buffer-get-glyph-positions buf len)))
                             (loop for i from 0 below count
                                   for info = (cffi:mem-aptr infos
                                                             '(:struct hb:hb-glyph-info-t) i)
                                   for pos = (cffi:mem-aptr positions
                                                            '(:struct hb:hb-glyph-position-t) i)
                                   collect (make-shaped-glyph
                                            :glyph-id (cffi:foreign-slot-value
                                                       info '(:struct hb:hb-glyph-info-t)
                                                       'hb::codepoint)
                                            :cluster (cffi:foreign-slot-value
                                                      info '(:struct hb:hb-glyph-info-t)
                                                      'hb::cluster)
                                            :x-advance (cffi:foreign-slot-value
                                                        pos '(:struct hb:hb-glyph-position-t)
                                                        'hb::x-advance)
                                            :y-advance (cffi:foreign-slot-value
                                                        pos '(:struct hb:hb-glyph-position-t)
                                                        'hb::y-advance)
                                            :x-offset (cffi:foreign-slot-value
                                                       pos '(:struct hb:hb-glyph-position-t)
                                                       'hb::x-offset)
                                            :y-offset (cffi:foreign-slot-value
                                                       pos '(:struct hb:hb-glyph-position-t)
                                                       'hb::y-offset))))))))
                 (if fallback-fonts
                     (%apply-fallback-fonts result effective-text fallback-fonts)
                     result)))
          (hb:hb-buffer-destroy buf)))))))

(defun %map-shaped-glyphs (shaped-glyphs primary-font render-fn
                           &key (start-x 0) (start-y 0))
  "Walk SHAPED-GLYPHS accumulating cursor position, calling RENDER-FN for each glyph.
RENDER-FN receives (glyph-id font) where FONT is the glyph's fallback font or PRIMARY-FONT.
Returns a list of (pen-x pen-y . render-data) for non-NIL results."
  (let ((cursor-x start-x)
        (cursor-y start-y)
        (results nil))
    (dolist (sg shaped-glyphs)
      (let ((pen-x (+ cursor-x (shaped-glyph-x-offset sg)))
            (pen-y (+ cursor-y (shaped-glyph-y-offset sg)))
            (font  (or (shaped-glyph-font sg) primary-font)))
        (unless (shaped-glyph-skip sg)
          (let ((data (funcall render-fn (shaped-glyph-glyph-id sg) font)))
            (when data
              (push (list* pen-x pen-y data) results)))))
      (incf cursor-x (shaped-glyph-x-advance sg))
      (incf cursor-y (shaped-glyph-y-advance sg)))
    (nreverse results)))

(defun %map-shaped-glyphs-multiline (lines primary-font render-fn alignment line-height)
  "Layout LINES (list of shaped-glyph lists) with ALIGNMENT and stacked by LINE-HEIGHT.
Calls RENDER-FN as per %MAP-SHAPED-GLYPHS. Returns combined result list."
  (let* ((line-widths (mapcar (lambda (line)
                                (reduce #'+ line :key #'shaped-glyph-x-advance
                                                  :initial-value 0))
                              lines))
         (max-width (reduce #'max line-widths :initial-value 0))
         (current-y 0)
         (results nil))
    (loop for line in lines
          for lw  in line-widths
          for start-x = (ecase (or alignment :left)
                          (:left   0)
                          (:center (floor (- max-width lw) 2))
                          (:right  (- max-width lw)))
          do (setf results
                   (nconc results
                          (%map-shaped-glyphs line primary-font render-fn
                                              :start-x start-x
                                              :start-y current-y)))
             (incf current-y line-height))
    results))

(defun text-to-meshes (font text &key direction script language (segments-per-edge 8) depth
                                     alignment line-height max-width (wrap :word)
                                     (fallback-fonts *fallback-fonts*) basic)
  "Shape TEXT and triangulate each visible glyph into a positioned mesh.
Returns a list of (x y vertices indices). See SHAPE-TO-MESH for array formats.
Supports multi-line via #\\Newline and :alignment (:left/:center/:right) / :line-height.
MAX-WIDTH triggers automatic word wrapping at that width in font units; WRAP is :word or :glyph."
  (flet ((render (glyph-id font)
           (let ((shape (glyph-to-shape font glyph-id)))
             (when shape
               (multiple-value-bind (vertices indices)
                   (shape-to-mesh shape :segments-per-edge segments-per-edge :depth depth)
                 (list vertices indices))))))
    (let ((glyphs (shape-text font text
                               :direction direction :script script :language language
                               :alignment alignment :line-height line-height
                               :max-width max-width :wrap wrap
                               :fallback-fonts fallback-fonts :basic basic)))
      (%map-shaped-glyphs glyphs font #'render))))

(defun text-to-meshes-fast (font text &key direction script language (segments-per-edge 8) depth
                                          alignment line-height max-width (wrap :word)
                                          (fallback-fonts *fallback-fonts*) basic)
  "Shape TEXT and triangulate each visible glyph via ear-clipping (earcut).
Returns a list of (x y vertices indices). See SHAPE-TO-MESH-FAST for array formats.
Supports multi-line via #\\Newline and :alignment (:left/:center/:right) / :line-height.
MAX-WIDTH triggers automatic word wrapping at that width in font units; WRAP is :word or :glyph."
  (flet ((render (glyph-id font)
           (let ((shape (glyph-to-shape font glyph-id)))
             (when shape
               (multiple-value-bind (vertices indices)
                   (shape-to-mesh-fast shape :segments-per-edge segments-per-edge :depth depth)
                 (list vertices indices))))))
    (let ((glyphs (shape-text font text
                               :direction direction :script script :language language
                               :alignment alignment :line-height line-height
                               :max-width max-width :wrap wrap
                               :fallback-fonts fallback-fonts :basic basic)))
      (%map-shaped-glyphs glyphs font #'render))))

(defun text-to-sdfs (font text glyph-width glyph-height
                     &key direction script language (range 4.0d0) (padding 2.0)
                          alignment line-height max-width (wrap :word)
                          (fallback-fonts *fallback-fonts*) basic)
  "Shape TEXT and render each visible glyph as a positioned SDF bitmap.
Returns a list of (x y bitmap). See SHAPE-TO-SDF for bitmap format.
Supports multi-line via #\Newline and :alignment (:left/:center/:right) / :line-height.
MAX-WIDTH triggers automatic word wrapping at that width in font units; WRAP is :word or :glyph."
  (flet ((render (glyph-id font)
           (let ((shape (glyph-to-shape font glyph-id)))
             (when shape
               (list (shape-to-sdf shape glyph-width glyph-height
                                   :range range :padding padding))))))
    (let ((glyphs (shape-text font text
                               :direction direction :script script :language language
                               :alignment alignment :line-height line-height
                               :max-width max-width :wrap wrap
                               :fallback-fonts fallback-fonts :basic basic)))
      (%map-shaped-glyphs glyphs font #'render))))

(defun text-to-msdfs (font text glyph-width glyph-height
                      &key direction script language (range 4.0d0) (padding 2.0)
                           alignment line-height max-width (wrap :word)
                           (fallback-fonts *fallback-fonts*) basic)
  "Shape TEXT and render each visible glyph as a positioned MSDF bitmap.
Returns a list of (x y bitmap). See SHAPE-TO-MSDF for bitmap format.
Supports multi-line via #\Newline and :alignment (:left/:center/:right) / :line-height.
MAX-WIDTH triggers automatic word wrapping at that width in font units; WRAP is :word or :glyph."
  (flet ((render (glyph-id font)
           (let ((shape (glyph-to-shape font glyph-id)))
             (when shape
               (list (shape-to-msdf shape glyph-width glyph-height
                                    :range range :padding padding))))))
    (let ((glyphs (shape-text font text
                               :direction direction :script script :language language
                               :alignment alignment :line-height line-height
                               :max-width max-width :wrap wrap
                               :fallback-fonts fallback-fonts :basic basic)))
      (%map-shaped-glyphs glyphs font #'render))))


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
         (sdf-data (bitmap-data sdf))
         (bmp (make-bitmap width height 1))
         (bmp-data (bitmap-data bmp))
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
                        &key direction script language (range 4.0d0) (padding 2.0) (edge-width nil)
                             alignment line-height max-width (wrap :word)
                             (fallback-fonts *fallback-fonts*) basic)
  "Shape TEXT and render each visible glyph as a positioned anti-aliased bitmap.
Returns a list of (x y bitmap). See SHAPE-TO-BITMAP for bitmap format.
Supports multi-line via #\Newline and :alignment (:left/:center/:right) / :line-height.
MAX-WIDTH triggers automatic word wrapping at that width in font units; WRAP is :word or :glyph."
  (flet ((render (glyph-id font)
           (let ((shape (glyph-to-shape font glyph-id)))
             (when shape
               (list (shape-to-bitmap shape glyph-width glyph-height
                                      :range range :padding padding
                                      :edge-width edge-width))))))
    (let ((glyphs (shape-text font text
                               :direction direction :script script :language language
                               :alignment alignment :line-height line-height
                               :max-width max-width :wrap wrap
                               :fallback-fonts fallback-fonts :basic basic)))
      (%map-shaped-glyphs glyphs font #'render))))

;;;; ——— Fast (no-AA) bitmap rendering ———

(defun shape-to-bitmap-fast (shape width height &key (padding 2.0))
  "Render SHAPE to a 1-channel binary bitmap of WIDTH x HEIGHT pixels.
Uses direct winding-number rasterization — no SDF, no anti-aliasing.
Pixels are 1.0 (inside) or 0.0 (outside). Faster than SHAPE-TO-BITMAP."
  (multiple-value-bind (scale tx ty)
      (auto-scale-shape shape width height :padding padding)
    (let* ((bmp (make-bitmap width height 1))
           (bmp-data (bitmap-data bmp)))
      (loop for y fixnum from 0 below height
            do (loop for x fixnum from 0 below width
                     for px single-float = (coerce (/ (+ x 0.5d0 tx) scale) 'single-float)
                     for py single-float = (coerce (/ (+ y 0.5d0 ty) scale) 'single-float)
                     do (setf (aref bmp-data (+ x (* y width)))
                              (if (zerop (%shape-winding-at shape px py))
                                  0.0
                                  1.0))))
      bmp)))

(defun glyph-to-bitmap-fast (font glyph-id width height &key (padding 2.0))
  "Render glyph GLYPH-ID from FONT as a 1-channel binary bitmap, or NIL for blank glyphs."
  (let ((shape (glyph-to-shape font glyph-id)))
    (when shape
      (shape-to-bitmap-fast shape width height :padding padding))))

(defun text-to-bitmaps-fast (font text glyph-width glyph-height
                             &key direction script language (padding 2.0)
                                  alignment line-height max-width (wrap :word)
                                  (fallback-fonts *fallback-fonts*) basic)
  "Shape TEXT and render each visible glyph as a positioned binary bitmap.
Returns a list of (x y bitmap). No anti-aliasing; pixels are 1.0 or 0.0.
See SHAPE-TO-BITMAP-FAST for bitmap format."
  (flet ((render (glyph-id font)
           (let ((shape (glyph-to-shape font glyph-id)))
             (when shape
               (list (shape-to-bitmap-fast shape glyph-width glyph-height
                                           :padding padding))))))
    (let ((glyphs (shape-text font text
                               :direction direction :script script :language language
                               :alignment alignment :line-height line-height
                               :max-width max-width :wrap wrap
                               :fallback-fonts fallback-fonts :basic basic)))
      (%map-shaped-glyphs glyphs font #'render))))

;;;; ——— shape-text-lines ———

(defun shape-text-lines (font text &key direction script language
                                        alignment line-height
                                        max-width (wrap :word)
                                        (fallback-fonts *fallback-fonts*)
                                        basic)
  "Shape multi-line TEXT. Returns a list of plists (:y y-offset :x x-offset :glyphs shaped-glyphs).
Text is split on #\Newline; y-offsets are stacked by LINE-HEIGHT (default = upem).
ALIGNMENT is :left (default), :center, or :right.
MAX-WIDTH triggers automatic word wrapping at that width in font units; WRAP is :word or :glyph."
  (let* ((effective-text (if max-width
                             (%wrap-text font text max-width wrap
                                         :direction direction :script script
                                         :language language :fallback-fonts fallback-fonts
                                         :basic basic)
                             text))
         (lines (if (find #\Newline effective-text)
                    (%split-lines effective-text)
                    (list effective-text)))
         (lh (or line-height (%get-font-upem font)))
         (lines-glyphs (mapcar (lambda (line)
                                 (if (string= line "")
                                     nil
                                     (shape-text font line
                                                 :direction direction :script script
                                                 :language language
                                                 :fallback-fonts fallback-fonts
                                                 :basic basic)))
                               lines))
         (line-widths (mapcar (lambda (glyphs)
                                (reduce #'+ glyphs :key #'shaped-glyph-x-advance
                                                    :initial-value 0))
                              lines-glyphs))
         (max-line-width (reduce #'max line-widths :initial-value 0))
         (current-y 0))
    (loop for glyphs in lines-glyphs
          for lw in line-widths
          for start-x = (ecase (or alignment :left)
                          (:left   0)
                          (:center (floor (- max-line-width lw) 2))
                          (:right  (- max-line-width lw)))
          collect (list :y current-y :x start-x :glyphs glyphs)
          do (incf current-y lh))))

;;;; ——— string-bounds ———

(defun string-bounds (font text &key direction script language
                                    alignment line-height
                                    max-width (wrap :word)
                                    depth
                                    (fallback-fonts *fallback-fonts*)
                                    basic)
  "Calculate the ink bounding box of TEXT shaped with FONT, without rendering.
All values are in font units. Coordinates follow OBJ/3D convention: X rightward,
Y upward, baseline at Y=0. For multi-line text, lines stack downward from the first.

Without DEPTH: returns (VALUES width height) — the unsigned ink extent.
With DEPTH:    returns (VALUES min-x min-y min-z max-x max-y max-z).

When the text contains no visible glyphs, returns (VALUES 0 0) or all zeros."
  (let ((glyphs (shape-text font text
                             :direction direction :script script :language language
                             :alignment alignment :line-height line-height
                             :max-width max-width :wrap wrap
                             :fallback-fonts fallback-fonts :basic basic))
        (cursor-x 0)
        (cursor-y 0)
        (ink-min-x nil)
        (ink-min-y nil)
        (ink-max-x nil)
        (ink-max-y nil))
    (dolist (sg glyphs)
      (let ((pen-x (+ cursor-x (shaped-glyph-x-offset sg)))
            (pen-y (+ cursor-y (shaped-glyph-y-offset sg)))
            (gly-font (or (shaped-glyph-font sg) font)))
        (unless (shaped-glyph-skip sg)
          (let ((shape (glyph-to-shape gly-font (shaped-glyph-glyph-id sg))))
            (when shape
              (multiple-value-bind (gmin-x gmin-y gmax-x gmax-y)
                  (shape-bounds shape)
                ;; pen-y is Y-down; glyph shapes are Y-up.
                ;; Effective Y = shape-Y - pen-Y (matches OBJ export convention).
                (let ((eff-min-x (+ pen-x gmin-x))
                      (eff-max-x (+ pen-x gmax-x))
                      (eff-min-y (- gmin-y pen-y))
                      (eff-max-y (- gmax-y pen-y)))
                  (setf ink-min-x (if ink-min-x (min ink-min-x eff-min-x) eff-min-x)
                        ink-max-x (if ink-max-x (max ink-max-x eff-max-x) eff-max-x)
                        ink-min-y (if ink-min-y (min ink-min-y eff-min-y) eff-min-y)
                        ink-max-y (if ink-max-y (max ink-max-y eff-max-y) eff-max-y)))))))
        (incf cursor-x (shaped-glyph-x-advance sg))
        (incf cursor-y (shaped-glyph-y-advance sg))))
    (let ((min-x (or ink-min-x 0.0))
          (max-x (or ink-max-x 0.0))
          (min-y (or ink-min-y 0.0))
          (max-y (or ink-max-y 0.0)))
      (if depth
          (values min-x min-y 0.0 max-x max-y (coerce depth 'single-float))
          (values (- max-x min-x) (- max-y min-y))))))
