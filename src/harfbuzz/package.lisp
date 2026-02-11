;;;; src/harfbuzz/package.lisp

(defpackage #:harfarasta/harfbuzz
  (:use #:cl #:cffi)
  (:export
   ;; Opaque types
   #:hb-blob
   #:hb-face
   #:hb-font
   #:hb-buffer
   #:hb-draw-funcs
   #:hb-language

   ;; Enums
   #:hb-memory-mode
   #:hb-direction
   #:hb-script
   #:hb-buffer-content-type

   ;; Tag helpers
   #:hb-tag
   #:hb-script-from-tag

   ;; Blob
   #:hb-blob-create
   #:hb-blob-create-from-file
   #:hb-blob-destroy
   #:hb-blob-get-length

   ;; Face
   #:hb-face-create
   #:hb-face-destroy
   #:hb-face-get-glyph-count
   #:hb-face-get-upem
   #:hb-face-reference

   ;; Font
   #:hb-font-create
   #:hb-font-destroy
   #:hb-font-set-scale
   #:hb-font-get-scale
   #:hb-font-set-ppem
   #:hb-font-get-ppem
   #:hb-font-reference

   ;; Buffer
   #:hb-buffer-create
   #:hb-buffer-destroy
   #:hb-buffer-add-utf8
   #:hb-buffer-set-direction
   #:hb-buffer-get-direction
   #:hb-buffer-set-script
   #:hb-buffer-get-script
   #:hb-buffer-set-language
   #:hb-buffer-get-language
   #:hb-buffer-set-content-type
   #:hb-buffer-get-content-type
   #:hb-buffer-get-length
   #:hb-buffer-get-glyph-infos
   #:hb-buffer-get-glyph-positions
   #:hb-buffer-guess-segment-properties
   #:hb-buffer-reference

   ;; Shape
   #:hb-shape

   ;; Language
   #:hb-language-from-string
   #:hb-language-to-string

   ;; Draw funcs
   #:hb-draw-funcs-create
   #:hb-draw-funcs-destroy
   #:hb-draw-funcs-set-move-to-func
   #:hb-draw-funcs-set-line-to-func
   #:hb-draw-funcs-set-quadratic-to-func
   #:hb-draw-funcs-set-cubic-to-func
   #:hb-draw-funcs-set-close-path-func
   #:hb-font-draw-glyph

   ;; Structs
   #:hb-glyph-info-t
   #:hb-glyph-position-t
   #:hb-draw-state-t
   #:hb-feature-t

   ;; Draw sink infrastructure
   #:draw-sink
   #:make-draw-sink
   #:draw-sink-move-to
   #:draw-sink-line-to
   #:draw-sink-quadratic-to
   #:draw-sink-cubic-to
   #:draw-sink-close-path
   #:register-draw-data
   #:unregister-draw-data
   #:get-draw-data
   #:make-hb-draw-funcs))
