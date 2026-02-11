;;;; harfarasta.asd

(asdf:defsystem #:harfarasta/harfbuzz
  :description "CFFI bindings to HarfBuzz for harfarasta"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:cffi)
  :serial t
  :components ((:module "src/harfbuzz"
                :components ((:file "package")
                             (:file "library")
                             (:file "bindings")))))

(asdf:defsystem #:harfarasta
  :description "Platform/backend-agnostic text rendering and shaping for Common Lisp"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:harfarasta/harfbuzz
               #:trivial-signed-distance-fields
               #:trivial-delaunay-triangulation
               #:font-discovery)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "rich-text"))))
  :in-order-to ((test-op (test-op #:harfarasta/tests))))

(asdf:defsystem #:harfarasta/export
  :description "PNG and OBJ export utilities for harfarasta"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:harfarasta #:zpng)
  :serial t
  :components ((:module "src/export"
                :components ((:file "package")
                             (:file "export")))))

(asdf:defsystem #:harfarasta/tests
  :description "Tests for harfarasta"
  :depends-on (#:harfarasta #:fiveam)
  :serial t
  :components ((:file "tests/tests"))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :harfarasta-suite
                                                  :harfarasta/tests))))
