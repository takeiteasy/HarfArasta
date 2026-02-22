;;;; harfarasta.asd

(asdf:defsystem #:harfarasta/harfbuzz
  :description "CFFI bindings to HarfBuzz for harfarasta"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "harfbuzz")))

(asdf:defsystem #:harfarasta/woff2
  :description "CFFI bindings to libwoff2shim for WOFF2 decoding"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "woff2")))

(asdf:defsystem #:harfarasta
  :description "Platform/backend-agnostic text rendering and shaping for Common Lisp"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:harfarasta/harfbuzz
               #:harfarasta/woff2
               #:font-discovery)
  :serial t
  :components ((:file "package")
               (:file "inflate")
               (:file "woff")
               (:file "types")
               (:file "delaunay")
               (:file "earcut")
               (:file "edge-coloring")
               (:file "math")
               (:file "distance")
               (:file "msdf")
               (:file "utils")
               (:file "harfarasta")))

(asdf:defsystem #:harfarasta/fontstash
  :description "Font atlas / glyph packing for harfarasta"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:harfarasta #:zpng)
  :serial t
  :components ((:file "fontstash")))

(asdf:defsystem #:harfarasta/export
  :description "PNG and OBJ export utilities for harfarasta"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:harfarasta #:zpng)
  :serial t
  :components ((:file "export")))

(asdf:defsystem #:harfarasta/tests
  :description "Tests for harfarasta"
  :depends-on (#:harfarasta/export #:harfarasta/fontstash)
  :serial t
  :components ((:file "tests")))