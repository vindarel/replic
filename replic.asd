#|
  This file is a part of replic project.
|#

(asdf:defsystem "replic"
  ;; :version (:read-file-form "version.lisp-expr")
  ;; This is relative to the load path, so it fails from another library.
  :version "0.12"
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

;; From 78 to 18MB
;; However, from 0.04 to 0.26 startup time, which is noticeable.
;; #+sb-core-compression
;; (defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  ;; (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
