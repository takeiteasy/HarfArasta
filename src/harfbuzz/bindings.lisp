;;;; src/harfbuzz/bindings.lisp
;;;; CFFI bindings to HarfBuzz

(in-package #:cl-rich-text/harfbuzz)

;;; ——— Helper: HB_TAG encoding ———

(defun hb-tag (c1 c2 c3 c4)
  "Encode four characters into an hb_tag_t (uint32), matching HB_TAG macro."
  (logior (ash (logand (char-code c1) #xFF) 24)
          (ash (logand (char-code c2) #xFF) 16)
          (ash (logand (char-code c3) #xFF) 8)
          (logand (char-code c4) #xFF)))

(defun hb-script-from-tag (tag-string)
  "Convert a 4-character script tag string (e.g. \"Latn\") to an hb_script_t uint32."
  (assert (= (length tag-string) 4))
  (hb-tag (char tag-string 0)
          (char tag-string 1)
          (char tag-string 2)
          (char tag-string 3)))

;;; ——— Opaque pointer types ———

(defctype hb-blob :pointer)
(defctype hb-face :pointer)
(defctype hb-font :pointer)
(defctype hb-buffer :pointer)
(defctype hb-draw-funcs :pointer)
(defctype hb-language :pointer)

;;; ——— Enums ———

(defcenum hb-memory-mode
  (:duplicate 0)
  (:readonly 1)
  (:writable 2)
  (:readonly-may-make-writable 3))

(defcenum hb-direction
  (:invalid 0)
  (:ltr 4)
  (:rtl 5)
  (:ttb 6)
  (:btt 7))

(defcenum hb-buffer-content-type
  (:invalid 0)
  (:unicode 1)
  (:glyphs 2))

;;; ——— Structs ———

(defcstruct hb-glyph-info-t
  (codepoint :uint32)
  (mask :uint32)
  (cluster :uint32)
  (var1 :uint32)
  (var2 :uint32))

(defcstruct hb-glyph-position-t
  (x-advance :int32)
  (y-advance :int32)
  (x-offset :int32)
  (y-offset :int32)
  (var :uint32))

(defcstruct hb-draw-state-t
  (path-open :int)
  (path-start-x :float)
  (path-start-y :float)
  (current-x :float)
  (current-y :float)
  (reserved1 :uint32)
  (reserved2 :uint32)
  (reserved3 :uint32)
  (reserved4 :uint32)
  (reserved5 :uint32)
  (reserved6 :uint32)
  (reserved7 :uint32))

(defcstruct hb-feature-t
  (tag :uint32)
  (value :uint32)
  (start :uint)
  (end :uint))

;;; ——— Blob functions ———

(defcfun ("hb_blob_create" hb-blob-create) hb-blob
  (data :pointer)
  (length :uint)
  (mode hb-memory-mode)
  (user-data :pointer)
  (destroy :pointer))

(defcfun ("hb_blob_create_from_file" hb-blob-create-from-file) hb-blob
  (file-name :string))

(defcfun ("hb_blob_destroy" hb-blob-destroy) :void
  (blob hb-blob))

(defcfun ("hb_blob_get_length" hb-blob-get-length) :uint
  (blob hb-blob))

;;; ——— Face functions ———

(defcfun ("hb_face_create" hb-face-create) hb-face
  (blob hb-blob)
  (index :uint))

(defcfun ("hb_face_destroy" hb-face-destroy) :void
  (face hb-face))

(defcfun ("hb_face_get_glyph_count" hb-face-get-glyph-count) :uint
  (face hb-face))

(defcfun ("hb_face_get_upem" hb-face-get-upem) :uint
  (face hb-face))

(defcfun ("hb_face_reference" hb-face-reference) hb-face
  (face hb-face))

;;; ——— Font functions ———

(defcfun ("hb_font_create" hb-font-create) hb-font
  (face hb-face))

(defcfun ("hb_font_destroy" hb-font-destroy) :void
  (font hb-font))

(defcfun ("hb_font_set_scale" hb-font-set-scale) :void
  (font hb-font)
  (x-scale :int)
  (y-scale :int))

(defcfun ("hb_font_get_scale" hb-font-get-scale) :void
  (font hb-font)
  (x-scale :pointer)
  (y-scale :pointer))

(defcfun ("hb_font_set_ppem" hb-font-set-ppem) :void
  (font hb-font)
  (x-ppem :uint)
  (y-ppem :uint))

(defcfun ("hb_font_get_ppem" hb-font-get-ppem) :void
  (font hb-font)
  (x-ppem :pointer)
  (y-ppem :pointer))

(defcfun ("hb_font_reference" hb-font-reference) hb-font
  (font hb-font))

;;; ——— Buffer functions ———

(defcfun ("hb_buffer_create" hb-buffer-create) hb-buffer)

(defcfun ("hb_buffer_destroy" hb-buffer-destroy) :void
  (buffer hb-buffer))

(defcfun ("hb_buffer_add_utf8" hb-buffer-add-utf8) :void
  (buffer hb-buffer)
  (text :string)
  (text-length :int)
  (item-offset :uint)
  (item-length :int))

(defcfun ("hb_buffer_set_direction" hb-buffer-set-direction) :void
  (buffer hb-buffer)
  (direction hb-direction))

(defcfun ("hb_buffer_get_direction" hb-buffer-get-direction) hb-direction
  (buffer hb-buffer))

(defcfun ("hb_buffer_set_script" hb-buffer-set-script) :void
  (buffer hb-buffer)
  (script :uint32))

(defcfun ("hb_buffer_get_script" hb-buffer-get-script) :uint32
  (buffer hb-buffer))

(defcfun ("hb_buffer_set_language" hb-buffer-set-language) :void
  (buffer hb-buffer)
  (language hb-language))

(defcfun ("hb_buffer_get_language" hb-buffer-get-language) hb-language
  (buffer hb-buffer))

(defcfun ("hb_buffer_set_content_type" hb-buffer-set-content-type) :void
  (buffer hb-buffer)
  (content-type hb-buffer-content-type))

(defcfun ("hb_buffer_get_content_type" hb-buffer-get-content-type) hb-buffer-content-type
  (buffer hb-buffer))

(defcfun ("hb_buffer_get_length" hb-buffer-get-length) :uint
  (buffer hb-buffer))

(defcfun ("hb_buffer_get_glyph_infos" hb-buffer-get-glyph-infos) :pointer
  (buffer hb-buffer)
  (length :pointer))

(defcfun ("hb_buffer_get_glyph_positions" hb-buffer-get-glyph-positions) :pointer
  (buffer hb-buffer)
  (length :pointer))

(defcfun ("hb_buffer_guess_segment_properties" hb-buffer-guess-segment-properties) :void
  (buffer hb-buffer))

(defcfun ("hb_buffer_reference" hb-buffer-reference) hb-buffer
  (buffer hb-buffer))

;;; ——— Shape ———

(defcfun ("hb_shape" hb-shape) :void
  (font hb-font)
  (buffer hb-buffer)
  (features :pointer)
  (num-features :uint))

;;; ——— Language ———

(defcfun ("hb_language_from_string" hb-language-from-string) hb-language
  (str :string)
  (len :int))

(defcfun ("hb_language_to_string" hb-language-to-string) :string
  (language hb-language))

;;; ——— Draw functions creation/destruction ———

(defcfun ("hb_draw_funcs_create" hb-draw-funcs-create) hb-draw-funcs)

(defcfun ("hb_draw_funcs_destroy" hb-draw-funcs-destroy) :void
  (dfuncs hb-draw-funcs))

;;; ——— Draw set-*-func ———

(defcfun ("hb_draw_funcs_set_move_to_func" hb-draw-funcs-set-move-to-func) :void
  (dfuncs hb-draw-funcs)
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defcfun ("hb_draw_funcs_set_line_to_func" hb-draw-funcs-set-line-to-func) :void
  (dfuncs hb-draw-funcs)
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defcfun ("hb_draw_funcs_set_quadratic_to_func" hb-draw-funcs-set-quadratic-to-func) :void
  (dfuncs hb-draw-funcs)
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defcfun ("hb_draw_funcs_set_cubic_to_func" hb-draw-funcs-set-cubic-to-func) :void
  (dfuncs hb-draw-funcs)
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defcfun ("hb_draw_funcs_set_close_path_func" hb-draw-funcs-set-close-path-func) :void
  (dfuncs hb-draw-funcs)
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

;;; ——— Font draw glyph ———

(defcfun ("hb_font_draw_glyph" hb-font-draw-glyph) :void
  (font hb-font)
  (glyph :uint32)
  (dfuncs hb-draw-funcs)
  (draw-data :pointer))

;;; ——— Draw callback infrastructure ———

(defstruct draw-sink
  "Holds Lisp callback functions for HarfBuzz draw events."
  (move-to (constantly nil) :type function)
  (line-to (constantly nil) :type function)
  (quadratic-to (constantly nil) :type function)
  (cubic-to (constantly nil) :type function)
  (close-path (constantly nil) :type function))

(defvar *draw-data-table* (make-hash-table :test 'eql)
  "Maps integer IDs to draw-sink objects for use in draw callbacks.")

(defvar *draw-data-counter* 0
  "Monotonically increasing counter for draw-data IDs.")

(defun register-draw-data (sink)
  "Register a draw-sink and return its integer ID."
  (let ((id (incf *draw-data-counter*)))
    (setf (gethash id *draw-data-table*) sink)
    id))

(defun unregister-draw-data (id)
  "Remove a draw-sink by its integer ID."
  (remhash id *draw-data-table*))

(defun get-draw-data (id)
  "Look up a draw-sink by its integer ID."
  (gethash id *draw-data-table*))

;;; ——— Draw callbacks ———

(defcallback draw-move-to-cb :void
    ((dfuncs :pointer) (draw-data :pointer) (st :pointer)
     (to-x :float) (to-y :float) (user-data :pointer))
  (declare (ignore dfuncs st user-data))
  (let ((sink (get-draw-data (pointer-address draw-data))))
    (when sink
      (funcall (draw-sink-move-to sink) to-x to-y))))

(defcallback draw-line-to-cb :void
    ((dfuncs :pointer) (draw-data :pointer) (st :pointer)
     (to-x :float) (to-y :float) (user-data :pointer))
  (declare (ignore dfuncs st user-data))
  (let ((sink (get-draw-data (pointer-address draw-data))))
    (when sink
      (funcall (draw-sink-line-to sink) to-x to-y))))

(defcallback draw-quadratic-to-cb :void
    ((dfuncs :pointer) (draw-data :pointer) (st :pointer)
     (cx :float) (cy :float) (to-x :float) (to-y :float)
     (user-data :pointer))
  (declare (ignore dfuncs st user-data))
  (let ((sink (get-draw-data (pointer-address draw-data))))
    (when sink
      (funcall (draw-sink-quadratic-to sink) cx cy to-x to-y))))

(defcallback draw-cubic-to-cb :void
    ((dfuncs :pointer) (draw-data :pointer) (st :pointer)
     (cx0 :float) (cy0 :float) (cx1 :float) (cy1 :float)
     (to-x :float) (to-y :float) (user-data :pointer))
  (declare (ignore dfuncs st user-data))
  (let ((sink (get-draw-data (pointer-address draw-data))))
    (when sink
      (funcall (draw-sink-cubic-to sink) cx0 cy0 cx1 cy1 to-x to-y))))

(defcallback draw-close-path-cb :void
    ((dfuncs :pointer) (draw-data :pointer) (st :pointer)
     (user-data :pointer))
  (declare (ignore dfuncs st user-data))
  (let ((sink (get-draw-data (pointer-address draw-data))))
    (when sink
      (funcall (draw-sink-close-path sink)))))

;;; ——— Convenience: make-hb-draw-funcs ———

(defun make-hb-draw-funcs ()
  "Create an hb_draw_funcs_t with all 5 draw callbacks registered.
Returns the foreign pointer. Caller must call hb-draw-funcs-destroy when done."
  (let ((dfuncs (hb-draw-funcs-create)))
    (hb-draw-funcs-set-move-to-func dfuncs (callback draw-move-to-cb)
                                    (null-pointer) (null-pointer))
    (hb-draw-funcs-set-line-to-func dfuncs (callback draw-line-to-cb)
                                    (null-pointer) (null-pointer))
    (hb-draw-funcs-set-quadratic-to-func dfuncs (callback draw-quadratic-to-cb)
                                         (null-pointer) (null-pointer))
    (hb-draw-funcs-set-cubic-to-func dfuncs (callback draw-cubic-to-cb)
                                     (null-pointer) (null-pointer))
    (hb-draw-funcs-set-close-path-func dfuncs (callback draw-close-path-cb)
                                       (null-pointer) (null-pointer))
    dfuncs))
