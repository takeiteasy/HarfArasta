;;;; tests/tests.lisp

(defpackage #:cl-rich-text/tests
  (:use #:cl #:fiveam)
  (:export #:cl-rich-text-suite))

(in-package #:cl-rich-text/tests)

(def-suite cl-rich-text-suite
  :description "Tests for cl-rich-text")

(in-suite cl-rich-text-suite)

(defvar *test-font-path*
  (asdf:system-relative-pathname :cl-rich-text
                                 "harfbuzz/perf/fonts/Roboto-Regular.ttf"))

;;; ——— Phase 0: Library loading ———

(test library-loads
  "HarfBuzz shared library loads successfully"
  (is-true (cffi:foreign-library-loaded-p 'cl-rich-text/harfbuzz::libharfbuzz)))

;;; ——— Phase 1: CFFI bindings ———

(test blob-lifecycle
  "Create blob from file, verify length, destroy"
  (let ((blob (cl-rich-text/harfbuzz:hb-blob-create-from-file
               (namestring *test-font-path*))))
    (is (> (cl-rich-text/harfbuzz:hb-blob-get-length blob) 0))
    (cl-rich-text/harfbuzz:hb-blob-destroy blob)))

(test face-from-file
  "Create face from blob, verify glyph count and upem"
  (let* ((blob (cl-rich-text/harfbuzz:hb-blob-create-from-file
                (namestring *test-font-path*)))
         (face (cl-rich-text/harfbuzz:hb-face-create blob 0)))
    (is (> (cl-rich-text/harfbuzz:hb-face-get-glyph-count face) 0))
    (is (= (cl-rich-text/harfbuzz:hb-face-get-upem face) 2048))
    (cl-rich-text/harfbuzz:hb-face-destroy face)
    (cl-rich-text/harfbuzz:hb-blob-destroy blob)))

(test font-create-and-scale
  "Create font, set and get scale"
  (let* ((blob (cl-rich-text/harfbuzz:hb-blob-create-from-file
                (namestring *test-font-path*)))
         (face (cl-rich-text/harfbuzz:hb-face-create blob 0))
         (font (cl-rich-text/harfbuzz:hb-font-create face)))
    (cl-rich-text/harfbuzz:hb-font-set-scale font 2048 2048)
    (cffi:with-foreign-objects ((x :int) (y :int))
      (cl-rich-text/harfbuzz:hb-font-get-scale font x y)
      (is (= (cffi:mem-ref x :int) 2048))
      (is (= (cffi:mem-ref y :int) 2048)))
    (cl-rich-text/harfbuzz:hb-font-destroy font)
    (cl-rich-text/harfbuzz:hb-face-destroy face)
    (cl-rich-text/harfbuzz:hb-blob-destroy blob)))

(test buffer-shape-latin
  "Shape 'Hello' and verify 5 glyphs with positive advances"
  (let* ((blob (cl-rich-text/harfbuzz:hb-blob-create-from-file
                (namestring *test-font-path*)))
         (face (cl-rich-text/harfbuzz:hb-face-create blob 0))
         (font (cl-rich-text/harfbuzz:hb-font-create face))
         (buf (cl-rich-text/harfbuzz:hb-buffer-create)))
    (cl-rich-text/harfbuzz:hb-font-set-scale font 2048 2048)
    (cl-rich-text/harfbuzz:hb-buffer-add-utf8 buf "Hello" -1 0 -1)
    (cl-rich-text/harfbuzz:hb-buffer-guess-segment-properties buf)
    (cl-rich-text/harfbuzz:hb-shape font buf (cffi:null-pointer) 0)
    (cffi:with-foreign-object (len :uint)
      (let ((infos (cl-rich-text/harfbuzz:hb-buffer-get-glyph-infos buf len)))
        (declare (ignore infos))
        (is (= (cffi:mem-ref len :uint) 5)))
      (let ((positions (cl-rich-text/harfbuzz:hb-buffer-get-glyph-positions buf len)))
        (dotimes (i (cffi:mem-ref len :uint))
          (let ((pos (cffi:mem-aptr positions '(:struct cl-rich-text/harfbuzz:hb-glyph-position-t) i)))
            (is (> (cffi:foreign-slot-value pos '(:struct cl-rich-text/harfbuzz:hb-glyph-position-t) 'cl-rich-text/harfbuzz::x-advance) 0))))))
    (cl-rich-text/harfbuzz:hb-buffer-destroy buf)
    (cl-rich-text/harfbuzz:hb-font-destroy font)
    (cl-rich-text/harfbuzz:hb-face-destroy face)
    (cl-rich-text/harfbuzz:hb-blob-destroy blob)))

(test language-round-trip
  "Convert 'en' to hb_language and back"
  (let* ((lang (cl-rich-text/harfbuzz:hb-language-from-string "en" -1))
         (str (cl-rich-text/harfbuzz:hb-language-to-string lang)))
    (is (string= str "en"))))

(test tag-encoding
  "hb-script-from-tag encodes 'Latn' correctly"
  (let ((tag (cl-rich-text/harfbuzz:hb-script-from-tag "Latn")))
    ;; "Latn" = #x4C61746E
    (is (= tag #x4C61746E))))

(test draw-callbacks-fire
  "Draw glyph 'H' and verify move-to and close-path callbacks fire"
  (let* ((blob (cl-rich-text/harfbuzz:hb-blob-create-from-file
                (namestring *test-font-path*)))
         (face (cl-rich-text/harfbuzz:hb-face-create blob 0))
         (font (cl-rich-text/harfbuzz:hb-font-create face))
         (upem (cl-rich-text/harfbuzz:hb-face-get-upem face))
         (move-count 0)
         (close-count 0))
    (cl-rich-text/harfbuzz:hb-font-set-scale font upem upem)
    ;; Shape "H" to get its glyph ID
    (let ((buf (cl-rich-text/harfbuzz:hb-buffer-create)))
      (cl-rich-text/harfbuzz:hb-buffer-add-utf8 buf "H" -1 0 -1)
      (cl-rich-text/harfbuzz:hb-buffer-guess-segment-properties buf)
      (cl-rich-text/harfbuzz:hb-shape font buf (cffi:null-pointer) 0)
      (cffi:with-foreign-object (len :uint)
        (let* ((infos (cl-rich-text/harfbuzz:hb-buffer-get-glyph-infos buf len))
               (glyph-id (cffi:foreign-slot-value
                          (cffi:mem-aptr infos '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t) 0)
                          '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t)
                          'cl-rich-text/harfbuzz::codepoint)))
          ;; Now draw it
          (let* ((sink (cl-rich-text/harfbuzz:make-draw-sink
                        :move-to (lambda (x y) (declare (ignore x y)) (incf move-count))
                        :line-to (lambda (x y) (declare (ignore x y)))
                        :quadratic-to (lambda (cx cy x y) (declare (ignore cx cy x y)))
                        :cubic-to (lambda (cx0 cy0 cx1 cy1 x y) (declare (ignore cx0 cy0 cx1 cy1 x y)))
                        :close-path (lambda () (incf close-count))))
                 (sink-id (cl-rich-text/harfbuzz:register-draw-data sink))
                 (dfuncs (cl-rich-text/harfbuzz:make-hb-draw-funcs)))
            (cl-rich-text/harfbuzz:hb-font-draw-glyph
             font glyph-id dfuncs (cffi:make-pointer sink-id))
            (cl-rich-text/harfbuzz:hb-draw-funcs-destroy dfuncs)
            (cl-rich-text/harfbuzz:unregister-draw-data sink-id))))
      (cl-rich-text/harfbuzz:hb-buffer-destroy buf))
    (is (> move-count 0) "move-to should fire at least once")
    (is (> close-count 0) "close-path should fire at least once")
    (cl-rich-text/harfbuzz:hb-font-destroy font)
    (cl-rich-text/harfbuzz:hb-face-destroy face)
    (cl-rich-text/harfbuzz:hb-blob-destroy blob)))

;;; ——— Phase 2: Glyph outline extraction ———

(test glyph-to-shape-basic
  "glyph-to-shape on 'A' returns a shape with contours"
  (rich-text:with-font (font *test-font-path*)
    ;; Shape "A" to get its glyph ID
    (let ((buf (cl-rich-text/harfbuzz:hb-buffer-create)))
      (cl-rich-text/harfbuzz:hb-buffer-add-utf8 buf "A" -1 0 -1)
      (cl-rich-text/harfbuzz:hb-buffer-guess-segment-properties buf)
      (cl-rich-text/harfbuzz:hb-shape font buf (cffi:null-pointer) 0)
      (cffi:with-foreign-object (len :uint)
        (let* ((infos (cl-rich-text/harfbuzz:hb-buffer-get-glyph-infos buf len))
               (glyph-id (cffi:foreign-slot-value
                          (cffi:mem-aptr infos '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t) 0)
                          '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t)
                          'cl-rich-text/harfbuzz::codepoint))
               (shape (rich-text:glyph-to-shape font glyph-id)))
          (is-true shape "Shape should not be NIL for 'A'")
          (is (= (length (trivial-sdf:shape-contours shape)) 2)
              "Letter 'A' should have 2 contours (outer + inner)")))
      (cl-rich-text/harfbuzz:hb-buffer-destroy buf))))

(test glyph-to-shape-space
  "glyph-to-shape on space returns NIL"
  (rich-text:with-font (font *test-font-path*)
    ;; Shape " " to get space glyph ID
    (let ((buf (cl-rich-text/harfbuzz:hb-buffer-create)))
      (cl-rich-text/harfbuzz:hb-buffer-add-utf8 buf " " -1 0 -1)
      (cl-rich-text/harfbuzz:hb-buffer-guess-segment-properties buf)
      (cl-rich-text/harfbuzz:hb-shape font buf (cffi:null-pointer) 0)
      (cffi:with-foreign-object (len :uint)
        (let* ((infos (cl-rich-text/harfbuzz:hb-buffer-get-glyph-infos buf len))
               (glyph-id (cffi:foreign-slot-value
                          (cffi:mem-aptr infos '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t) 0)
                          '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t)
                          'cl-rich-text/harfbuzz::codepoint))
               (shape (rich-text:glyph-to-shape font glyph-id)))
          (is-false shape "Shape should be NIL for space glyph")))
      (cl-rich-text/harfbuzz:hb-buffer-destroy buf))))

(test with-font-macro
  "with-font provides a working font handle"
  (rich-text:with-font (font *test-font-path*)
    (is-true (not (cffi:null-pointer-p font))
             "Font should not be a null pointer")
    ;; Verify font works by checking scale
    (cffi:with-foreign-objects ((x :int) (y :int))
      (cl-rich-text/harfbuzz:hb-font-get-scale font x y)
      (is (= (cffi:mem-ref x :int) 2048)
          "Scale should be set to upem (2048)"))))

;;; ——— Phase 3: Single glyph rendering ———

(defun %glyph-id-for-char (font char-string)
  "Shape a single character and return its glyph ID. Test helper."
  (let ((buf (cl-rich-text/harfbuzz:hb-buffer-create)))
    (cl-rich-text/harfbuzz:hb-buffer-add-utf8 buf char-string -1 0 -1)
    (cl-rich-text/harfbuzz:hb-buffer-guess-segment-properties buf)
    (cl-rich-text/harfbuzz:hb-shape font buf (cffi:null-pointer) 0)
    (cffi:with-foreign-object (len :uint)
      (let* ((infos (cl-rich-text/harfbuzz:hb-buffer-get-glyph-infos buf len))
             (glyph-id (cffi:foreign-slot-value
                        (cffi:mem-aptr infos '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t) 0)
                        '(:struct cl-rich-text/harfbuzz:hb-glyph-info-t)
                        'cl-rich-text/harfbuzz::codepoint)))
        (cl-rich-text/harfbuzz:hb-buffer-destroy buf)
        glyph-id))))

;; SDF tests

(test shape-to-sdf-basic
  "shape-to-sdf returns a 1-channel bitmap with correct dimensions"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "A"))
           (shape (rich-text:glyph-to-shape font gid))
           (bmp (rich-text:shape-to-sdf shape 64 64)))
      (is-true bmp "Bitmap should not be NIL")
      (is (= (trivial-sdf:bitmap-width bmp) 64))
      (is (= (trivial-sdf:bitmap-height bmp) 64))
      (is (= (trivial-sdf:bitmap-channels bmp) 1)))))

(test shape-to-sdf-content
  "SDF bitmap has near-boundary and far-outside distance values"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "A"))
           (shape (rich-text:glyph-to-shape font gid))
           (bmp (rich-text:shape-to-sdf shape 64 64))
           (data (trivial-sdf:bitmap-data bmp))
           (min-v 999.0)
           (max-v -999.0))
      (loop for i from 0 below (length data)
            for v = (aref data i)
            do (when (< v min-v) (setf min-v v))
               (when (> v max-v) (setf max-v v)))
      (is (> max-v 0.3) "SDF should have near-boundary values (> 0.3)")
      (is (< min-v 0.1) "SDF should have far-outside values (< 0.1)")
      (is (> (- max-v min-v) 0.2) "SDF should have meaningful distance variation"))))

;; MSDF tests

(test shape-to-msdf-basic
  "shape-to-msdf returns a 3-channel bitmap with correct dimensions"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "A"))
           (shape (rich-text:glyph-to-shape font gid))
           (bmp (rich-text:shape-to-msdf shape 64 64)))
      (is-true bmp "Bitmap should not be NIL")
      (is (= (trivial-sdf:bitmap-width bmp) 64))
      (is (= (trivial-sdf:bitmap-height bmp) 64))
      (is (= (trivial-sdf:bitmap-channels bmp) 3)))))

(test shape-to-msdf-content
  "MSDF bitmap has varied channel values spanning inside/outside"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "A"))
           (shape (rich-text:glyph-to-shape font gid))
           (bmp (rich-text:shape-to-msdf shape 64 64))
           (data (trivial-sdf:bitmap-data bmp))
           (has-inside nil)
           (has-outside nil))
      (loop for i from 0 below (length data)
            for v = (aref data i)
            do (when (> v 0.6) (setf has-inside t))
               (when (< v 0.4) (setf has-outside t)))
      (is-true has-inside "MSDF should have inside values (> 0.6)")
      (is-true has-outside "MSDF should have outside values (< 0.4)"))))

;; Glyph-level convenience tests

(test glyph-to-sdf-convenience
  "End-to-end glyph-to-sdf works"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "H"))
           (bmp (rich-text:glyph-to-sdf font gid 32 32)))
      (is-true bmp "glyph-to-sdf should return a bitmap")
      (is (= (trivial-sdf:bitmap-channels bmp) 1)))))

(test glyph-to-msdf-convenience
  "End-to-end glyph-to-msdf works"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "H"))
           (bmp (rich-text:glyph-to-msdf font gid 32 32)))
      (is-true bmp "glyph-to-msdf should return a bitmap")
      (is (= (trivial-sdf:bitmap-channels bmp) 3)))))

(test glyph-to-sdf-space-returns-nil
  "glyph-to-sdf returns NIL for blank glyphs (space)"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font " "))
           (bmp (rich-text:glyph-to-sdf font gid 32 32)))
      (is-false bmp "glyph-to-sdf should return NIL for space"))))

;; Mesh tests

(test shape-to-mesh-basic
  "shape-to-mesh returns vertices/indices arrays with correct structure"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "A"))
           (shape (rich-text:glyph-to-shape font gid)))
      (multiple-value-bind (vertices indices)
          (rich-text:shape-to-mesh shape)
        (is-true vertices "Vertices should not be NIL")
        (is-true indices "Indices should not be NIL")
        (is (typep vertices '(simple-array single-float (*))))
        (is (typep indices '(simple-array (unsigned-byte 32) (*))))
        (is (zerop (mod (length vertices) 2))
            "Vertices length should be even (x,y pairs)")
        (is (zerop (mod (length indices) 3))
            "Indices length should be multiple of 3 (triangles)")
        (is (> (length vertices) 0) "Should have some vertices")
        (is (> (length indices) 0) "Should have some indices")))))

(test shape-to-mesh-indices-valid
  "All mesh indices are less than vertex count"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "A"))
           (shape (rich-text:glyph-to-shape font gid)))
      (multiple-value-bind (vertices indices)
          (rich-text:shape-to-mesh shape)
        (let ((vert-count (/ (length vertices) 2)))
          (loop for i from 0 below (length indices)
                do (is (< (aref indices i) vert-count)
                       (format nil "Index ~A (~A) should be < ~A"
                               i (aref indices i) vert-count))))))))

(test shape-to-mesh-hole-glyph
  "'A' mesh produces multiple triangles spanning non-zero range"
  (rich-text:with-font (font *test-font-path*)
    (let* ((gid (%glyph-id-for-char font "A"))
           (shape (rich-text:glyph-to-shape font gid)))
      (multiple-value-bind (vertices indices)
          (rich-text:shape-to-mesh shape)
        (is (> (/ (length indices) 3) 1)
            "Should have multiple triangles for 'A'")
        ;; Check that vertices span a non-zero range
        (let ((min-x most-positive-single-float)
              (max-x most-negative-single-float))
          (loop for i from 0 below (length vertices) by 2
                for x = (aref vertices i)
                do (when (< x min-x) (setf min-x x))
                   (when (> x max-x) (setf max-x x)))
          (is (> (- max-x min-x) 0.0)
              "Vertices should span a non-zero x range"))))))

(test glyph-to-mesh-convenience
  "End-to-end glyph-to-mesh works"
  (rich-text:with-font (font *test-font-path*)
    (let ((gid (%glyph-id-for-char font "H")))
      (multiple-value-bind (vertices indices)
          (rich-text:glyph-to-mesh font gid)
        (is-true vertices "Vertices should not be NIL")
        (is-true indices "Indices should not be NIL")
        (is (> (length vertices) 0))
        (is (> (length indices) 0))))))

(test glyph-to-mesh-space-returns-nil
  "glyph-to-mesh returns NIL for blank glyphs (space)"
  (rich-text:with-font (font *test-font-path*)
    (let ((gid (%glyph-id-for-char font " ")))
      (multiple-value-bind (vertices indices)
          (rich-text:glyph-to-mesh font gid)
        (is-false vertices "Vertices should be NIL for space")
        (is-false indices "Indices should be NIL for space")))))
