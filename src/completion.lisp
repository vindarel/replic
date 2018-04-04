(defpackage replic.completion
  (:use :cl)
  (:export :add-command
           :add-variable
           :add-completion
           :is-function
           :is-variable
           :get-function
           :get-variable
           :get-package
           :commands
           :variables
           :candidates
           :*default-command-completion*))

(in-package :replic.completion)


(defvar *args-completions* nil
  "Alist to associate a verb (str) to its completion candidates.")

(defvar *commands* nil "List of commands.")

(defvar *variables* nil "List of variables.")

(defvar *packages* nil
  "Association of a verb or command and the package it comes from (so
   than we can call it!).")

(defparameter *default-command-completion* nil
  "A variable, list or function to use to complete all commands that don't have an associated completion method.")

(defun commands ()
  "Return the list of available commands."
  *commands*)

(defun variables ()
  "Return the list of available variables."
  *variables*)

(defun add-command (it package)
  "Define a command.
   Specify the package it comes from.
   Takes a symbol or a string and stores a lowercase string.
   "
  (let ((it (string-downcase (string it))))
    (push (cons it package) *packages*)
    (push it *commands*)))

(defun add-variable (it package)
  "Define a variable."
  (let ((it (string-downcase (string it))))
    (push (cons it package) *packages*)
    (push it *variables*)))

(defun is-function (verb)
  "Is this string a registered verb ?"
  (member verb *commands* :test #'equal))

(defun is-variable (arg)
  (member arg *variables* :test #'equal))

(defun get-function (verb)
  ""
  (find-symbol (string-upcase verb) (assoc-value *packages* verb :test #'equal)))

(defun get-variable (arg)
  (get-function arg))

(defun get-package (name)
  "Return the package this arg is from."
  (find-symbol (string-upcase name) (assoc-value *packages* name :test #'equal)))

(defun add-completion (verb list-or-fn)
  ;; set-completion ?
  "Associate the completion for the given verb. The completion candidates can be:

  a) a list of strings

  b) a function, returning a list of strings.

  (if you'd like to give a symbol to be evaluated as a list... just use a function.
  "
  ;; old behavior: instead of giving a symbol evaluating a list of
  ;; strings, just use a function.
  ;; (push (string-downcase (string it)) *variables*)
  (push (cons verb list-or-fn) *args-completions*))


(defun assoc-value (alist key &key (test #'equalp))
  (cdr (assoc key alist :test test)))

;; (defun get-completions (verb)
(defun candidates (verb)
  "Return the completion candidates (list of strings) for this verb."
  (let ((list-or-function (or (assoc-value *args-completions* verb :test 'equal)
                              *default-command-completion*)))
    (cond
      ((functionp list-or-function)
       (funcall list-or-function))

      (t
       list-or-function))))

;;
;; Development helpers.
;;
(defun reset ()
  "Reset state. Set all lists to nil.
   For use in the REPL.
   "
  (setf *commands* nil)
  (setf *variables* nil)
  (setf *packages* nil)
  (setf *args-completions* nil))
