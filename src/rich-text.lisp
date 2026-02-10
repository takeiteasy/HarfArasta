;;;; src/rich-text.lisp
;;;; Phase 2: Glyph outline extraction

(in-package #:cl-rich-text)

(defun make-path-builder-draw-sink (builder)
  "Create a draw-sink that forwards HarfBuzz draw events to a trivial-sdf path-builder."
  (cl-rich-text/harfbuzz:make-draw-sink
   :move-to (lambda (x y)
              (trivial-sdf:path-move-to builder x y))
   :line-to (lambda (x y)
              (trivial-sdf:path-line-to builder x y))
   :quadratic-to (lambda (cx cy x y)
                   (trivial-sdf:path-quadratic-to builder cx cy x y))
   :cubic-to (lambda (cx0 cy0 cx1 cy1 x y)
               (trivial-sdf:path-cubic-to builder cx0 cy0 cx1 cy1 x y))
   :close-path (lambda ()
                 (trivial-sdf:path-close builder))))

(defun glyph-to-shape (font glyph-id)
  "Extract glyph outline from FONT as a trivial-sdf:shape, or NIL for blank glyphs.
FONT is an hb_font_t pointer. GLYPH-ID is the glyph codepoint (uint32)."
  (let* ((builder (trivial-sdf:make-path-builder))
         (sink (make-path-builder-draw-sink builder))
         (sink-id (cl-rich-text/harfbuzz:register-draw-data sink))
         (dfuncs (cl-rich-text/harfbuzz:make-hb-draw-funcs)))
    (unwind-protect
         (progn
           (cl-rich-text/harfbuzz:hb-font-draw-glyph
            font glyph-id dfuncs (cffi:make-pointer sink-id))
           (let ((shape (trivial-sdf:path-to-shape builder)))
             (if (null (trivial-sdf:shape-contours shape))
                 nil
                 shape)))
      (cl-rich-text/harfbuzz:hb-draw-funcs-destroy dfuncs)
      (cl-rich-text/harfbuzz:unregister-draw-data sink-id))))

(defmacro with-font ((font-var path &key (index 0)) &body body)
  "Load a font from PATH and bind it to FONT-VAR for the duration of BODY.
Manages blob, face, and font lifecycle. Sets scale to face upem."
  (let ((blob-var (gensym "BLOB"))
        (face-var (gensym "FACE"))
        (upem-var (gensym "UPEM")))
    `(let* ((,blob-var (cl-rich-text/harfbuzz:hb-blob-create-from-file
                        (namestring ,path)))
            (,face-var (cl-rich-text/harfbuzz:hb-face-create ,blob-var ,index))
            (,font-var (cl-rich-text/harfbuzz:hb-font-create ,face-var))
            (,upem-var (cl-rich-text/harfbuzz:hb-face-get-upem ,face-var)))
       (cl-rich-text/harfbuzz:hb-font-set-scale ,font-var ,upem-var ,upem-var)
       (unwind-protect
            (progn ,@body)
         (cl-rich-text/harfbuzz:hb-font-destroy ,font-var)
         (cl-rich-text/harfbuzz:hb-face-destroy ,face-var)
         (cl-rich-text/harfbuzz:hb-blob-destroy ,blob-var)))))
