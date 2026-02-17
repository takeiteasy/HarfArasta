;;;; src/package.lisp

(defpackage #:harfarasta
  (:nicknames #:rich-text)
  (:use #:cl)
  (:local-nicknames (#:hb #:harfarasta/harfbuzz)
                    (#:woff2 #:harfarasta/woff2))
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
   #:shaped-glyph-font
   #:shaped-glyph-skip
   #:shape-text
   #:shape-text-lines
   #:text-to-meshes
   #:text-to-sdfs
   #:text-to-msdfs
   #:find-font-path
   #:shape-to-bitmap
   #:glyph-to-bitmap
   #:text-to-bitmaps
   ;; Bitmap struct accessors
   #:bitmap
   #:bitmap-data
   #:bitmap-width
   #:bitmap-height
   #:bitmap-channels
   #:bitmap-to-bytes
   ;; Shape utilities
   #:shape-bounds
   #:auto-scale-shape
   ;; Font inspection
   #:font-has-char-p
   #:font-missing-chars
   #:font-monospace-p
   ;; Fallback fonts
   #:*fallback-fonts*
   #:with-fallback-fonts
   ;; Persistent font management
   #:create-font
   #:create-font-from-bytes
   #:destroy-font
   ;; Font format identification
   #:identify-font-format
   ;; Low-level SDF generation (explicit scale/translate)
   #:generate-sdf-from-shape
   #:make-bitmap))
