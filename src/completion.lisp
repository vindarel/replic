(defpackage replic.completion
  (:use :cl)
  (:export :add-command
           :add-variable
           :add-completion
           :is-function
           :is-variable
           :get-function
           :get-variable
           :get-symbol
           :commands
           :variables
           :functions-to-commands
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
  "Return the package this arg is from.

  `name`: string designating a symbol, registered with `add-command`
    or `add-variable`."
  (assoc-value *packages* (string-downcase name) :test #'equal))

(defun get-symbol (name)
  "Return the symbol associated to name (str)."
  (find-symbol (string-upcase name)
               (get-package name)))

(defun packages ()
  "Get a list of uniq package symbols used in the application."
  (let (packlist)
    (mapcar (lambda (alist)
              (unless (member (cdr alist) packlist)
                (push (cdr alist) packlist)))
            replic.completion::*packages*)
    packlist))

(defun add-completion (verb list-or-fn)
  ;; set-completion ?
  "Associate the completion for the given verb. The completion candidates can be:

  a) a list of strings

  b) a function, returning a list of strings.

  (if you'd like to give a symbol to be evaluated as a list... just use a function.
  "
  ;; (push (string-downcase (string it)) *variables*)
  (push (cons verb list-or-fn) *args-completions*))


(defun assoc-value (alist key &key (test #'equalp))
  (cdr (assoc key alist :test test)))

;; (defun get-completions (verb)
(defun candidates (verb)
  "Return the completion candidates (list of strings) for this verb."
  (let ((list-or-function (or (assoc-value *args-completions* verb :test #'equal)
                              *default-command-completion*)))
    (cond
      ((functionp list-or-function)
       (funcall list-or-function))

      (t
       list-or-function))))
(defun functions-to-commands (package &key exclude)
  "Add exported functions of `package` to the list of commands to complete,
   add exported variables to the list of `set`-able variables.

   Ignore the functions given in the `:exclude` list (names as strings, for example \"main\").
  "
  (assert (symbolp package))
  (assert (or nil (listp exclude)))
  (do-external-symbols (it package)
    ;; we'll use strings because symbols have a package prefix: my-package:main != :main
    ;; and we want the most straightforward thing for the user.
    (unless (member (string-downcase (symbol-name it))
                    exclude
                    :test #'equal)
      (if (str:starts-with? "*" (string it))
          (replic.completion:add-variable it package)
          (replic.completion:add-command it package))))
  (values
   (reverse (replic.completion:commands))
   (reverse (replic.completion:variables)))
  )

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
