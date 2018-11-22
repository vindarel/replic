#|
  This file is a part of replic project.
|#

(in-package :cl-user)
(defpackage replic-test-asd
  (:use :cl :asdf))
(in-package :replic-test-asd)

;; I used to to asdf:defsystem but read it isn't recommended.
(defsystem replic-test
  :author "vindarel"
  :license "MIT"
  :depends-on (:replic
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "tests"
                        :components
                        ((:test-file "test-replic")
                         (:test-file "test-config"))))
  :description "Test system for replic."
  ;; Do this run and exit tests correctly ? prove-asdf is not found so far.
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
