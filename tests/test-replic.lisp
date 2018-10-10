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
      '("foo" "main")
      "Adding functions from a package."))

(with-fixtures
    (is (replic.completion:functions-to-commands :fixture :exclude '("main"))
        '("foo")
        "Exclude functions."))

(finalize)
