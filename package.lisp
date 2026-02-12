;;;; src/package.lisp

(defpackage #:harfarasta
  (:nicknames #:rich-text)
  (:use #:cl)
  (:local-nicknames (#:hb #:harfarasta/harfbuzz))
  (:export
   #:glyph-to-shape
   #:with-font
   #:shape-to-sdf
   #:shape-to-msdf
   #:shape-to-mesh
   #:glyph-to-sdf
   #:glyph-to-msdf
   #:glyph-to-mesh
   #:shaped-glyph
   #:make-shaped-glyph
   #:shaped-glyph-glyph-id
   #:shaped-glyph-cluster
   #:shaped-glyph-x-advance
   #:shaped-glyph-y-advance
   #:shaped-glyph-x-offset
   #:shaped-glyph-y-offset
   #:shape-text
   #:text-to-meshes
   #:text-to-sdfs
   #:text-to-msdfs
   #:find-font-path
   #:shape-to-bitmap
   #:glyph-to-bitmap
   #:text-to-bitmaps))
