;;;; tests/tests.lisp

(defpackage #:harfarasta/tests
  (:use #:cl #:harfarasta #:harfarasta/export)
  (:local-nicknames (#:cs #:common-shapes)
                     (#:hmesh #:harfarasta/mesh))
  (:export #:render-tests #:mesh-tests))

(in-package #:harfarasta/tests)

(defun %glyph-id-for-char (font char)
  "Shape a single-character string with FONT and return its glyph ID."
  (rich-text:shaped-glyph-glyph-id (first (rich-text:shape-text font (string char)))))

(defun %signed-area-2d (vertices i0 i1 i2)
  "Signed area of the triangle formed by 2D vertex indices I0/I1/I2 in the
flat, stride-2 VERTICES array. Positive = counter-clockwise."
  (let ((x0 (aref vertices (* i0 2))) (y0 (aref vertices (1+ (* i0 2))))
        (x1 (aref vertices (* i1 2))) (y1 (aref vertices (1+ (* i1 2))))
        (x2 (aref vertices (* i2 2))) (y2 (aref vertices (1+ (* i2 2)))))
    (/ (- (* (- x1 x0) (- y2 y0))
          (* (- x2 x0) (- y1 y0)))
       2.0)))

(defun mesh-tests (&key font-path)
  "Exercise harfarasta/mesh: build glyph and text meshes and assert they are
well-formed COMMON-SHAPES:MESH objects with the expected array types,
dimensions, and (for 2D) counter-clockwise winding."
  (let ((path (or (and font-path (pathname font-path))
                  (rich-text:find-font-path :family "Arial"))))
    (format t "~%=== harfarasta/mesh tests ===~%")
    (format t "Font: ~A~%~%" path)
    (rich-text:with-font (font path)
      ;; 1. Single glyph, 2D
      (format t "1. glyph-mesh: 'A' at size 64, 2D~%")
      (let* ((glyph-id (%glyph-id-for-char font #\A))
             (mesh (hmesh:glyph-mesh font glyph-id :size 64)))
        (assert (cs:mesh-p mesh) () "glyph-mesh did not return a common-shapes mesh")
        (assert (typep (cs:mesh-vertices mesh) '(simple-array single-float (*))) ()
                "glyph-mesh vertices have the wrong array type")
        (assert (typep (cs:mesh-indices mesh) '(simple-array (unsigned-byte 32) (*))) ()
                "glyph-mesh indices have the wrong array type")
        (assert (= (cs:mesh-dimensions mesh) 2) () "2D glyph-mesh should have dimensions 2")
        (assert (plusp (cs:vertex-count mesh)) () "glyph-mesh has no vertices")
        (assert (plusp (cs:triangle-count mesh)) () "glyph-mesh has no triangles")
        (assert (>= (%signed-area-2d (cs:mesh-vertices mesh)
                                      (aref (cs:mesh-indices mesh) 0)
                                      (aref (cs:mesh-indices mesh) 1)
                                      (aref (cs:mesh-indices mesh) 2))
                    0.0)
                () "glyph-mesh triangle winding is not counter-clockwise")
        (format t "   vertices=~D triangles=~D dimensions=~D~%"
                (cs:vertex-count mesh) (cs:triangle-count mesh) (cs:mesh-dimensions mesh)))

      ;; 2. Single glyph, 2D, with normals
      (format t "2. glyph-mesh: 'A' at size 64, with :normals t~%")
      (let* ((glyph-id (%glyph-id-for-char font #\A))
             (mesh (hmesh:glyph-mesh font glyph-id :size 64 :normals t)))
        (assert (typep (cs:mesh-normals mesh) '(simple-array single-float (*))) ()
                "glyph-mesh with :normals t should have a normals array")
        (assert (= (length (cs:mesh-normals mesh)) (* (cs:vertex-count mesh) 3)) ()
                "glyph-mesh normals array has the wrong length"))

      ;; 3. Single glyph, extruded (3D)
      (format t "3. glyph-mesh: 'A' at size 64, :depth 0.1 (3D)~%")
      (let* ((glyph-id (%glyph-id-for-char font #\A))
             (mesh (hmesh:glyph-mesh font glyph-id :size 64 :depth 0.1)))
        (assert (= (cs:mesh-dimensions mesh) 3) () "extruded glyph-mesh should have dimensions 3")
        (assert (null (cs:mesh-normals mesh)) ()
                "extruded glyph-mesh should not get normals from glyph-mesh")
        (format t "   vertices=~D triangles=~D dimensions=~D~%"
                (cs:vertex-count mesh) (cs:triangle-count mesh) (cs:mesh-dimensions mesh)))

      ;; 4. Per-glyph text meshes vs merged text mesh
      (format t "4. text-meshes / text-mesh: 'Hi' at size 64~%")
      (let* ((per-glyph (hmesh:text-meshes font "Hi" :size 64))
             (merged (hmesh:text-mesh font "Hi" :size 64))
             (sum-verts (reduce #'+ per-glyph :key #'cs:vertex-count :initial-value 0))
             (sum-tris (reduce #'+ per-glyph :key #'cs:triangle-count :initial-value 0)))
        (assert (= (length per-glyph) 2) () "text-meshes should return one mesh per glyph")
        (dolist (m per-glyph)
          (assert (cs:mesh-p m) () "text-meshes entry is not a common-shapes mesh"))
        (assert (= (cs:vertex-count merged) sum-verts) ()
                "merged text-mesh vertex count should equal sum of per-glyph vertex counts")
        (assert (= (cs:triangle-count merged) sum-tris) ()
                "merged text-mesh triangle count should equal sum of per-glyph triangle counts")
        (format t "   per-glyph: ~D meshes, merged: vertices=~D triangles=~D~%"
                (length per-glyph) (cs:vertex-count merged) (cs:triangle-count merged)))

      ;; 5. Earcut (fast) path
      (format t "5. glyph-mesh: 'A' at size 64, :fast t (earcut)~%")
      (let* ((glyph-id (%glyph-id-for-char font #\A))
             (mesh (hmesh:glyph-mesh font glyph-id :size 64 :fast t)))
        (assert (plusp (cs:triangle-count mesh)) () "fast glyph-mesh has no triangles")))

    (format t "~%harfarasta/mesh tests passed.~%")))

(defun render-tests (&key (output-dir (asdf:system-relative-pathname :harfarasta "tests/"))
                          font-path)
  "Run a suite of export tests, writing results to OUTPUT-DIR.
Uses FONT-PATH if given, otherwise discovers Arial."
  (ensure-directories-exist output-dir)
  (let ((path (or (and font-path (pathname font-path))
                  (rich-text:find-font-path :family "Arial"))))
    (flet ((out (name)
                (merge-pathnames name output-dir)))
      (format t "~%=== harfarasta/export render tests ===~%")
      (format t "Font: ~A~%" path)
      (format t "Output: ~A~%~%" output-dir)

      ;; PNG tests
      (format t "1. PNG: 'Hello' white on transparent, 64px~%")
      (time (render-string "Hello" (out "hello-white-64.png")
                           :as :png :font-path path :size 64
                           :color '(255 255 255)))

      (format t "1.5 PNG: 'Hello' white on transparent (fast), 64px~%")
      (time (render-string "Hello" (out "hello-white-64-fast.png")
                           :as :png :font-path path :size 64
                           :color '(255 255 255)
                           :anti-alias nil))

      (format t "2. PNG: a long string in red on transparent, 128px~%")
      (time (render-string "The quick brown fox jumps over the lazy dog" (out "long-red-128.png")
                           :as :png :font-path path :size 128
                           :color '(255 0 0)))
      
      (format t "2.5 PNG: a long string in red on transparent (fast), 128px~%")
      (time (render-string "The quick brown fox jumps over the lazy dog" (out "long-red-128-fast.png")
                           :as :png :font-path path :size 128
                           :color '(255 0 0)
                           :anti-alias nil))

      (format t "3. PNG: 'ABCDEF' green on transparent, 48px~%")
      (time (render-string "ABCDEF" (out "abcdef-green-48.png")
                           :as :png :font-path path :size 48
                           :color '(0 200 0)))

      (format t "4. PNG: multiline 'harfarasta / renders / great' blue, centered, 96px~%")
      (time (render-string (format nil "harfarasta~%renders~%great") (out "harfarasta-blue-96.png")
                           :as :png :font-path path :size 96
                           :alignment :center
                           :color '(80 140 255)))

      ;; OBJ tests
      (format t "5. OBJ: 'Hi' mesh, size 1.0~%")
      (time (render-string "Hi" (out "hi.obj")
                           :as :obj :font-path path :size 1.0))

      (time (rich-text:with-font (f (rich-text:find-font-path :family "Arial"))
              (render-string "HarfArasta (آراسته)" (out "harfarasta.obj")
                             :as :obj :font-path path :size 0.5 :depth 0.01
                             :fallback-fonts (list f))))
      ;; Fontstash test
      (format t "7. Fontstash: atlas of 'ABCabc123' glyphs, SDF mode~%")
      (time
       (rich-text:with-font (f path)
         (let ((atlas (harfarasta/fontstash:make-font-atlas
                       :width 256 :height 256 :mode :sdf :padding 2)))
           (let ((entries (harfarasta/fontstash:atlas-add-text
                           atlas f "ABCabc123" 32 32)))
             (format t "   Packed ~D glyphs into atlas~%" (count-if #'identity entries))
             (dolist (e entries)
               (when e
                 (let ((r (harfarasta/fontstash:atlas-entry-region e)))
                   (format t "   glyph ~D @ (~D,~D) ~Dx~D  UV (~,3F,~,3F)-(~,3F,~,3F)~%"
                           (harfarasta/fontstash:atlas-entry-glyph-id e)
                           (harfarasta/fontstash:atlas-region-x r)
                           (harfarasta/fontstash:atlas-region-y r)
                           (harfarasta/fontstash:atlas-region-width r)
                           (harfarasta/fontstash:atlas-region-height r)
                           (harfarasta/fontstash:atlas-entry-u0 e)
                           (harfarasta/fontstash:atlas-entry-v0 e)
                           (harfarasta/fontstash:atlas-entry-u1 e)
                           (harfarasta/fontstash:atlas-entry-v1 e))))))
           (harfarasta/fontstash:atlas-to-png atlas (out "atlas-sdf.png"))
           (format t "   Atlas PNG written to ~A~%" (out "atlas-sdf.png")))))

      ;; Word wrap tests
      (format t "8. PNG: word wrap 'The quick brown fox...' at 200px, 64px~%")
      (time (render-string "The quick brown fox jumps over the lazy dog" (out "wordwrap-200.png")
                           :as :png :font-path path :size 64
                           :color '(255 200 100) :max-width 200))

      (format t "9. PNG: glyph wrap 'Thequickbrownfox' at 150px, 64px~%")
      (time (render-string "Thequickbrownfox" (out "glyphwrap-150.png")
                           :as :png :font-path path :size 64
                           :color '(100 200 255) :max-width 150 :wrap :glyph))

      (format t "10. PNG: fixed canvas 200x80, 'Hello' 64px~%")
      (time (render-string "Hello" (out "fixed-canvas-200x80.png")
                           :as :png :font-path path :size 64
                           :color '(255 255 255) :png-size '(200 80)))

      (format t "11. PNG: word wrap + fixed canvas 200x200, 48px~%")
      (time (render-string "Wrap and clip together" (out "wrap-and-clip.png")
                           :as :png :font-path path :size 48
                           :color '(200 100 255) :max-width 200 :png-size '(200 200)))

      ;; Earcut (fast mesh) OBJ export
      (format t "12. OBJ: 'OBA' via earcut (anti-alias nil)~%")
      (time (render-string "OBA" (out "earcut-oba.obj")
                           :as :obj :font-path path :size 1.0 :depth 0.01
                           :anti-alias nil))

      (format t "~%Done. ~D files written to ~A~%" 12 output-dir))))
