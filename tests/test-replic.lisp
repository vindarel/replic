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
  `(let ((replic.completion::*commands* nil)
         (replic.completion::*args-completions* nil)
         (replic.completion::*variables* nil)
         (replic.completion::*default-command-completion* nil))
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

;; We can complete differently for different arguments.
(with-fixtures
  (add-completion "hello"
                  (lambda () (list "first"))
                  (list "second" "working")
                  (list "third"))

  (is (replic.completion:candidates "hello")
      (list "first"))
  (is (replic.completion:candidates "hello" :position 1)
      (list "second" "working"))
  (is (replic.completion:candidates "hello" :position 2)
      (list "third"))
  (is (replic.completion:candidates "hello" :position 3)
      nil
      "candidates at a big position: returns no candidates.")

  (is (replic::custom-complete "" 6 t "hello")
      (list "first"))
  (is (replic::custom-complete "f" 8 t "hello ")
      (list "first")
      "a space after the command")
  (is (replic::custom-complete "" 8 t "hello f")
      (list "first")
      "first argument, first letter")
  (is (replic::custom-complete "" 12 t "hello first s")
      (list "" "second" "working")
      "second argument")
  (is (replic::custom-complete "w" 13 t "hello first ")
      (list "working")
      "second argument, with a space")
  (is (replic::custom-complete "w" 13 t "hello first w")
      (list "working")
      "second argument, with input")
  (is (replic::custom-complete "t" 13 t "hello first second t")
      (list "third"))
  )

(with-fixtures
  (setf replic.completion:*default-command-completion* (lambda () (list "yo")))
  (is (replic::custom-complete "" 6 t "nope")
      (list "yo"))
  (is (replic::custom-complete "" 6 t "hello")
      (list "yo"))
  (is (replic::custom-complete "" 6 t "hello f")
      (list "yo"))
  (is (replic::custom-complete "" 6 t "hello first s")
      (list "yo")))

(finalize)
