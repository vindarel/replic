#|
  This file is a part of replic project.
|#

(asdf:defsystem "replic"
  :version "0.3.1"
  :author "vindarel"
  :license "MIT"
  :depends-on (:cl-readline
               :alexandria
               :str
               :cl-ansi-text
               :unix-opts)
  :components ((:module "src"
                :components
                ((:file "completion")
                 (:file "replic")
                 (:file "help")
                 (:file "utils")
                 (:file "base"))))

  :build-operation "program-op"
  :build-pathname "replic"
  :entry-point "replic:main"

  :description ""
  ;; :long-description
  ;; #.(read-file-string
     ;; (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "replic-test"))))
