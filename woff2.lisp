;;;; woff2.lisp — CFFI bindings to libwoff2shim

(defpackage #:harfarasta/woff2
  (:use #:cl #:cffi)
  (:export #:woff2-decode-to-vector))

(in-package #:harfarasta/woff2)

(pushnew (asdf:system-relative-pathname :harfarasta/woff2 "build/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library libwoff2shim
  (:darwin "libwoff2shim.dylib")
  (:unix "libwoff2shim.so")
  (:windows "woff2shim.dll")
  (t (:default "libwoff2shim")))

(cffi:use-foreign-library libwoff2shim)

(cffi:defcfun ("woff2_decode" %woff2-decode) :pointer
  (data :pointer)
  (length :size)
  (out-len :pointer))

(cffi:defcfun ("woff2_decode_free" %woff2-decode-free) :void
  (buf :pointer))

(defun woff2-decode-to-vector (woff2-bytes)
  "Decode a WOFF2 byte vector to a TTF/OTF byte vector.
WOFF2-BYTES is a (vector (unsigned-byte 8)). Returns a new (vector (unsigned-byte 8))."
  (let ((len (length woff2-bytes)))
    (cffi:with-foreign-object (out-len :size)
      (cffi:with-foreign-array (in-buf woff2-bytes `(:array :uint8 ,len))
        (let ((result (%woff2-decode in-buf len out-len)))
          (when (cffi:null-pointer-p result)
            (error "WOFF2 decoding failed"))
          (unwind-protect
               (let* ((rlen (cffi:mem-ref out-len :size))
                      (vec (make-array rlen :element-type '(unsigned-byte 8))))
                 (loop for i below rlen
                       do (setf (aref vec i)
                                (cffi:mem-aref result :uint8 i)))
                 vec)
            (%woff2-decode-free result)))))))
