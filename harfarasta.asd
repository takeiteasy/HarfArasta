;;;; harfarasta.asd

(asdf:defsystem #:harfarasta/harfbuzz
  :description "CFFI bindings to HarfBuzz for harfarasta"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "harfbuzz")))

(asdf:defsystem #:harfarasta
  :description "Platform/backend-agnostic text rendering and shaping for Common Lisp"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:harfarasta/harfbuzz
               #:font-discovery)
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "delaunay")
               (:file "edge-coloring")
               (:file "math")
               (:file "msdf")
               (:file "distance")
               (:file "utils")
               (:file "harfarasta"))
  :in-order-to ((test-op (test-op #:harfarasta/tests))))

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
  :depends-on (#:harfarasta #:fiveam)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :harfarasta-suite
                                                  :harfarasta/tests))))
