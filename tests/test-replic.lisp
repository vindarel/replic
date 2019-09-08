(defpackage replic-test
  (:use :cl
        :replic.completion
        :prove))
(in-package :replic-test)


(plan nil)

(defpackage fixture
  (:use :cl)
  (:export :foo
           :main))

(defmacro with-fixtures (&body body)
  `(let ((replic.completion::*commands* nil))
     ,@body))

(with-fixtures
  (is (replic.completion:functions-to-commands :fixture)
      '("main" "foo")
      "Adding functions from a package."))

(with-fixtures
    (is (replic.completion:functions-to-commands :fixture :exclude '("main"))
        '("foo")
        "Exclude functions."))

(with-fixtures
  (replic.completion:add-command "hello" :test)
  (is (replic::custom-complete "he" 0 t)
      (list "hello")
      "custom complete simple case")
  (replic.completion:add-command "help" :test)
  (is (replic::custom-complete "he" 0 t)
      (list "hel" "help" "hello")
      "custom complete two candidates"))

;; test default command completion function.
(with-fixtures
  (setf replic.completion:*default-command-completion* (lambda () (list "yo")))
  (is (replic.completion:candidates "hello")
      (list "yo")
      "get candidates with default completion function.")
  (is (replic::custom-complete "" 6 t "hello")
      (list "yo")
      "complete with default function (no initial input)")
  (is (replic::custom-complete "y" 6 t "hello")
      (list "yo")
      "complete with default function (first letter input)")
  (is (replic::custom-complete "" 6 t "hello yo")
      (list "yo")
      "complete with default function (second argument)"))

(finalize)
