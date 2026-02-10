;;;; src/package.lisp

(defpackage #:cl-rich-text
  (:nicknames #:rich-text)
  (:use #:cl)
  (:export
   #:glyph-to-shape
   #:with-font))
