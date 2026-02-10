;;;; cl-rich-text.asd

(asdf:defsystem #:cl-rich-text/harfbuzz
  :description "CFFI bindings to HarfBuzz for cl-rich-text"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:cffi)
  :serial t
  :components ((:module "src/harfbuzz"
                :components ((:file "package")
                             (:file "library")
                             (:file "bindings")))))

(asdf:defsystem #:cl-rich-text
  :description "Platform/backend-agnostic text rendering and shaping for Common Lisp"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on (#:cl-rich-text/harfbuzz
               #:trivial-signed-distance-fields
               #:trivial-delaunay-triangulation)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "rich-text"))))
  :in-order-to ((test-op (test-op #:cl-rich-text/tests))))

(asdf:defsystem #:cl-rich-text/tests
  :description "Tests for cl-rich-text"
  :depends-on (#:cl-rich-text #:fiveam)
  :serial t
  :components ((:file "tests/tests"))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :cl-rich-text-suite
                                                  :cl-rich-text/tests))))
