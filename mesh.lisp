;;;; src/mesh/mesh.lisp
;;;; common-shapes mesh integration for harfarasta

(defpackage #:harfarasta/mesh
  (:nicknames #:rich-text/mesh)
  (:use #:cl)
  (:local-nicknames (#:hb #:harfarasta/harfbuzz)
                     (#:cs #:common-shapes))
  (:export
   #:glyph-mesh
   #:text-meshes
   #:text-mesh))

(in-package #:harfarasta/mesh)

;;; --- Internals ---

(defun %font-scale (font size)
  "Return the scale factor mapping FONT's font-unit coordinates to SIZE units."
  (let ((upem (cffi:with-foreign-objects ((x :int) (y :int))
                (hb:hb-font-get-scale font x y)
                (cffi:mem-ref x :int))))
    (/ (coerce size 'single-float)
       (coerce upem 'single-float))))

(defun %pack-vertices (vertices stride scale pen-x pen-y)
  "Copy VERTICES (flat, stride STRIDE, font units) into a fresh array with
SCALE applied and (PEN-X . PEN-Y) baked in as an offset. Y is flipped from
font-unit Y-down to +Y-up, matching harfarasta/export's OBJ writer."
  (let* ((count (length vertices))
         (out (make-array count :element-type 'single-float)))
    (loop for i from 0 below count by stride
          for vx = (+ (* (aref vertices i) scale) (* pen-x scale))
          for vy = (- (* (aref vertices (1+ i)) scale) (* pen-y scale))
          do (setf (aref out i) vx)
             (setf (aref out (1+ i)) vy)
             (when (= stride 3)
               (setf (aref out (+ i 2)) (* (aref vertices (+ i 2)) scale))))
    out))

(defun %flat-z-normals (vert-count)
  "Return a fresh (VERT-COUNT * 3) single-float array of +Z normals, for
flat (unextruded) 2D meshes embedded in a common-shapes mesh."
  (let ((out (make-array (* vert-count 3) :element-type 'single-float
                                           :initial-element 0.0)))
    (loop for i from 0 below vert-count
          do (setf (aref out (+ (* i 3) 2)) 1.0))
    out))

(defun %entry->mesh (vertices indices &key depth normals scale (pen-x 0) (pen-y 0))
  "Convert a single harfarasta VERTICES/INDICES pair (as returned by
SHAPE-TO-MESH / SHAPE-TO-MESH-FAST) into a COMMON-SHAPES:MESH, applying SCALE
and the (PEN-X . PEN-Y) glyph offset. DEPTH is only used to determine stride
(pass the same value used when the mesh was triangulated). When NORMALS is
true and DEPTH is NIL, a flat +Z normal is filled per vertex; extruded (3D)
meshes never get normals here -- COMMON-SHAPES:COMPUTE-NORMALS would smooth
across the hard front/back/side-wall seams, so callers wanting normals on an
extruded mesh should compute them explicitly."
  (let* ((stride (if depth 3 2))
         (vert-count (floor (length vertices) stride))
         (packed-vertices (%pack-vertices vertices stride scale pen-x pen-y))
         (packed-indices (make-array (length indices) :element-type '(unsigned-byte 32))))
    (replace packed-indices indices)
    (cs:make-mesh :vertices packed-vertices
                  :indices packed-indices
                  :normals (when (and normals (not depth))
                             (%flat-z-normals vert-count))
                  :dimensions stride)))

;;; --- Public API ---

(defun glyph-mesh (font glyph-id &key (size 64) depth fast normals (segments-per-edge 8))
  "Triangulate GLYPH-ID from FONT into a COMMON-SHAPES:MESH.
SIZE is the target coordinate scale (font units are divided by the font's
units-per-em and multiplied by SIZE). DEPTH, when non-NIL, extrudes the glyph
along Z by DEPTH units (mesh becomes 3D). FAST selects ear-clipping (earcut)
triangulation instead of constrained Delaunay. NORMALS, when true, fills +Z
normals for flat (non-extruded) meshes. Returns NIL for blank glyphs (e.g.
space)."
  (let* ((scale (%font-scale font size))
         (depth-fu (when depth (/ (coerce depth 'single-float) scale)))
         (shape (rich-text:glyph-to-shape font glyph-id)))
    (when shape
      (multiple-value-bind (vertices indices)
          (if fast
              (rich-text:shape-to-mesh-fast shape :segments-per-edge segments-per-edge :depth depth-fu)
              (rich-text:shape-to-mesh shape :segments-per-edge segments-per-edge :depth depth-fu))
        (%entry->mesh vertices indices :depth depth-fu :normals normals :scale scale)))))

(defun text-meshes (font text &key (size 64) depth fast normals
                                   direction script language (segments-per-edge 8)
                                   alignment line-height max-width (wrap :word)
                                   fallback-fonts basic)
  "Shape TEXT with FONT and triangulate each visible glyph into a positioned
COMMON-SHAPES:MESH. Returns a list of meshes, one per rendered glyph, each
already translated to its pen position (glyph identity/order is preserved,
so callers can transform or animate letters individually). See GLYPH-MESH for
SIZE/DEPTH/FAST/NORMALS. The remaining keys are passed through to
RICH-TEXT:TEXT-TO-MESHES / TEXT-TO-MESHES-FAST for shaping and layout."
  (let* ((scale (%font-scale font size))
         (depth-fu (when depth (/ (coerce depth 'single-float) scale)))
         (mesh-fn (if fast #'rich-text:text-to-meshes-fast #'rich-text:text-to-meshes))
         (entries (funcall mesh-fn font text
                            :direction direction :script script :language language
                            :segments-per-edge segments-per-edge :depth depth-fu
                            :alignment alignment :line-height line-height
                            :max-width max-width :wrap wrap
                            :fallback-fonts fallback-fonts :basic basic)))
    (loop for (pen-x pen-y vertices indices) in entries
          collect (%entry->mesh vertices indices :depth depth-fu :normals normals
                                                  :scale scale :pen-x pen-x :pen-y pen-y))))

(defun text-mesh (font text &key (size 64) depth fast normals
                                 direction script language (segments-per-edge 8)
                                 alignment line-height max-width (wrap :word)
                                 fallback-fonts basic)
  "Shape TEXT with FONT and triangulate it into a single merged
COMMON-SHAPES:MESH (all glyphs share one vertex/index namespace). See
GLYPH-MESH for SIZE/DEPTH/FAST/NORMALS; the remaining keys are passed through
to RICH-TEXT:TEXT-TO-MESHES / TEXT-TO-MESHES-FAST for shaping and layout.
Returns a mesh with zero vertices/indices (but valid DIMENSIONS) if TEXT has
no visible glyphs."
  (let* ((scale (%font-scale font size))
         (depth-fu (when depth (/ (coerce depth 'single-float) scale)))
         (mesh-fn (if fast #'rich-text:text-to-meshes-fast #'rich-text:text-to-meshes))
         (entries (funcall mesh-fn font text
                            :direction direction :script script :language language
                            :segments-per-edge segments-per-edge :depth depth-fu
                            :alignment alignment :line-height line-height
                            :max-width max-width :wrap wrap
                            :fallback-fonts fallback-fonts :basic basic))
         (stride (if depth 3 2))
         (total-verts 0)
         (total-indices 0))
    (dolist (entry entries)
      (incf total-verts (length (third entry)))
      (incf total-indices (length (fourth entry))))
    (let ((merged-vertices (make-array total-verts :element-type 'single-float))
          (merged-indices (make-array total-indices :element-type '(unsigned-byte 32)))
          (vertex-cursor 0)
          (index-cursor 0)
          (global-vertex-offset 0))
      (loop for (pen-x pen-y vertices indices) in entries
            for vert-count = (floor (length vertices) stride)
            do (let ((packed (%pack-vertices vertices stride scale pen-x pen-y)))
                 (replace merged-vertices packed :start1 vertex-cursor)
                 (incf vertex-cursor (length packed)))
               (loop for i from 0 below (length indices)
                     do (setf (aref merged-indices index-cursor)
                              (+ (aref indices i) global-vertex-offset))
                        (incf index-cursor))
               (incf global-vertex-offset vert-count))
      (cs:make-mesh :vertices merged-vertices
                    :indices merged-indices
                    :normals (when (and normals (not depth) (plusp total-verts))
                               (%flat-z-normals (floor total-verts stride)))
                    :dimensions stride))))
