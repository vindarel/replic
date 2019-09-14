#|
  This file is a part of replic project.
|#

(asdf:defsystem "replic"
  ;; :version (:read-file-form "version.lisp-expr")
  ;; This is relative to the load path, so it fails from another library.
  :version "0.11"
  :author "vindarel"
  :license "MIT"
  :depends-on (:cl-readline
               :str
               :cl-ansi-text
               :unix-opts
               :py-configparser
               :shlex)
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
