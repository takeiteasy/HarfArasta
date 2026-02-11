;;;; src/harfbuzz/library.lisp

(in-package #:harfarasta/harfbuzz)

(pushnew (asdf:system-relative-pathname :harfarasta/harfbuzz "build/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library libharfbuzz
  (:darwin "libharfbuzz.dylib")
  (:unix "libharfbuzz.so")
  (:windows "harfbuzz.dll")
  (t (:default "libharfbuzz")))

(cffi:use-foreign-library libharfbuzz)
