(defpackage replic-test
  (:use :cl
        :replic
        :prove))
(in-package :replic-test)

;; NOTE: To run this test file, execute `(asdf:test-system :replic)' in your Lisp.

(plan nil)

(defpackage fixture
  (:use :cl)
  (:export :foo
           :main))

(defmacro with-fixtures (&body body)
  `(let ((replic.completion::*commands* nil))
     ,@body))

(with-fixtures
  (is (replic:functions-to-commands :fixture)
      '("foo" "main")
      "Adding functions from a package."))

(with-fixtures
    (is (replic:functions-to-commands :fixture :exclude '("main"))
        '("foo")
        "Exclude functions."))

(finalize)
