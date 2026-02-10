;;;; src/package.lisp

(defpackage #:cl-rich-text
  (:nicknames #:rich-text)
  (:use #:cl)
  (:export
   #:glyph-to-shape
   #:with-font
   ;; Phase 3: Single glyph rendering
   #:shape-to-sdf
   #:shape-to-msdf
   #:shape-to-mesh
   #:glyph-to-sdf
   #:glyph-to-msdf
   #:glyph-to-mesh))
