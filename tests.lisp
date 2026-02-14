;;;; tests/tests.lisp

(defpackage #:harfarasta/tests
  (:use #:cl #:harfarasta #:harfarasta/export)
  (:export #:render-tests))

(in-package #:harfarasta/tests)

(defun render-tests (&key (output-dir (asdf:system-relative-pathname :harfarasta "export-tests/"))
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
      (render-string "Hello" (out "hello-white-64.png")
                     :as :png :font-path path :size 64
                     :color '(255 255 255))

      (format t "2. PNG: a long string in red on transparent, 128px~%")
      (render-string "The quick brown fox jumps over the lazy dog" (out "long-red-128.png")
                     :as :png :font-path path :size 128
                     :color '(255 0 0))

      (format t "3. PNG: 'ABCDEF' green on transparent, 48px~%")
      (render-string "ABCDEF" (out "abcdef-green-48.png")
                     :as :png :font-path path :size 48
                     :color '(0 200 0))

      (format t "4. PNG: 'harfarasta' blue, 96px~%")
      (render-string "harfarasta" (out "harfarasta-blue-96.png")
                     :as :png :font-path path :size 96
                     :color '(80 140 255))
      ;; OBJ tests
      (format t "5. OBJ: 'Hi' mesh, size 1.0~%")
      (render-string "Hi" (out "hi.obj")
                     :as :obj :font-path path :size 1.0)

      (format t "6. OBJ: 'HarfArasta' mesh, size 0.5~%")
      (render-string "HarfArasta (آراسته)" (out "harfarasta.obj")
                     :as :obj :font-path path :size 0.5)

      (format t "~%Done. ~D files written to ~A~%" 6 output-dir))))
