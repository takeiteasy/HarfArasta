;;;; tests/tests.lisp

(defpackage #:harfarasta/tests
  (:use #:cl #:harfarasta #:harfarasta/export)
  (:export #:render-tests))

(in-package #:harfarasta/tests)

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

      (format t "2. PNG: a long string in red on transparent, 128px~%")
      (time (render-string "The quick brown fox jumps over the lazy dog" (out "long-red-128.png")
                           :as :png :font-path path :size 128
                           :color '(255 0 0)))

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

      (format t "~%Done. ~D files written to ~A~%" 11 output-dir))))
