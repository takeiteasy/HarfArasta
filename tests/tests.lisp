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
