;;;; src/package.lisp

(defpackage #:harfarasta
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
   #:glyph-to-mesh
   ;; Phase 4: String shaping + rendering
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
   ;; Phase 4.5: Font discovery + bitmap rendering
   #:find-font-path
   #:shape-to-bitmap
   #:glyph-to-bitmap
   #:text-to-bitmaps))
