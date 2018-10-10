#|
  This file is a part of replic project.
|#

(asdf:defsystem "replic-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("replic"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "test-replic"))))
  :description "Test system for replic"

  ;; :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
  )
