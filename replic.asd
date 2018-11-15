#|
  This file is a part of replic project.
|#

(asdf:defsystem "replic"
  :version "0.0"
  :author "vindarel"
  :license "MIT"
  :depends-on (:cl-readline
               :alexandria
               :str
               :cl-ansi-text
               :unix-opts
               :py-configparser)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "completion")
                 (:file "config")
                 (:file "replic")
                 (:file "help")
                 (:file "base"))))

  :build-operation "program-op"
  :build-pathname "replic"
  :entry-point "replic:main"

  :description "A framework to build readline applications out of existing code."
  ;; :long-description
  ;; #.(read-file-string
     ;; (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "replic-test"))))
