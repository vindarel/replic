(defpackage replic
  (:use :cl)
  (:shadow #:set)
  (:export :main
           :help
           :set
           ;; examples:
           :goodbye
           :hello
           :echo
           :*verbose*))

;; The package to be used in the user's init files.
(defpackage replic.user
  (:use :cl))

(in-package :replic)

(defparameter *init-file* #p"~/.replic.lisp"
              "The init file to load at startup, containing any lisp
              code to be `load`ed. It exports variables and functions
              to be used at the CLI, and how to complete them and
              their arguments.")

(defparameter *verbs* '()
  "List of commands for the REPL.")

(defparameter *args-completions* '()
  "Alist that associates a command name (verb) to:

  1) either a list of strings,

  2) either a function returning the completion candidates. This
  function takes the partially entered argument as argument.

  Example usage:

  (push '(\"goodbye\" . *names*) *args-completions*)

  ")

;;
;; Examples
;;
(defparameter *verbose* nil "Example setting.")

(defparameter *names* '()
  "List of names (string) given to `hello`. Will be autocompleted by `goodbye`.")

(defun hello (name)
  "Takes only one argument. Adds the given name to the global
  `*names*` global variable, used to complete arguments of `goodbye`.
  "
  (format t "hello ~a~&" name)
  (push name *names*))

(defun goodbye (name)
  "Says goodbye to name, where `name` should be completed from what was given to `hello`."
  (when *verbose*
    (format t "[lo]g - verbose is ~a~&" *verbose*))
  (format t "goodbye ~a~&" name))

(defun complete-hello ()
  ;; todo
  '("john" "maria"))

;;
;; Lib
;;
;; (defun assoc-value (alist key &key (test #'equalp))
;;   ;; Don't import Alexandria just for that.
;;   ;; See also Quickutil to import only the utility we need.
;;   ;; http://quickutil.org/lists/
;;   (cdr (assoc key alist :test test)))

(defun init-completions ()
  (push '("goodbye" . *names*) *args-completions*)
  (push '("hello" . #'complete-hello) *args-completions*)
  ;; the following is needed.
  (push '("set" . *variables*) *args-completions*))

(defun echo (string &rest more)
  "Print the rest of the line. Takes any number of arguments."
  (format t "~a~{ ~a~}~&" string more))

;; shadow works with build but not on Slime ??
(defun set (var arg)
  "Change this variable."
  (setf (symbol-value (find-symbol (string-upcase var))) (if (string= "t" arg)
                                                             t
                                                             (if (string= "nil" arg)
                                                                 nil
                                                                 arg)))
  (format t "~a set to ~a~&" var arg))

(defun common-prefix (items)
  ;; tmp waiting for cl-str 0.5 in Quicklisp february.
  (when items (subseq
               (car items)
               0
               (apply
                #'min
                (mapcar
                 #'(lambda (i) (or (mismatch (car items) i) (length i)))
                 (cdr items))))))


(defun complete-from-list (text list)
  "Select all verbs from `list' that start with `text'."
  (let ((els (remove-if-not (alexandria:curry #'str:starts-with? text)
                            list)))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

(defun complete-args (text line)
  "Completion for arguments."
  (let* ((verb (first (str:words line)))
         (list-or-function (alexandria:assoc-value *args-completions* verb :test 'equal)))
    (if list-or-function
        ;; with a list of strings.
        ;; todo: with a function.
        (complete-from-list text (symbol-value list-or-function)))))

(defun custom-complete (text start end)
  "Complete a symbol.

  text is the partially entered word. start and end are the position on `rl:*line-buffer*'.

  When the cursor is at the beginning of the prompt, complete from `*verbs*`.

  "
  (declare (ignore end))
  (if (zerop start)
      (if (str:starts-with? "*" text)
          (complete-from-list text *variables*)
          (complete-from-list text *verbs*))
      (complete-args text rl:*line-buffer*)))

(defparameter *variables* '()
  "List of parameters (str), setable with `set`.")

(defun functions-to-verbs (&optional (package *package*))
  "Add exported functions of `*package*` to the list of `*verbs*` to complete,
   add exported variables to the list of `set`-able variables.

   Remove any symbol named 'main'.

  "
  (do-external-symbols (it package)
    (if (str:starts-with? "*" (string it))
        (push (string-downcase (string it)) *variables*)
        (push (string-downcase (string it)) *verbs*)))
  (values
   (setf *verbs* (remove "main" *verbs* :test 'equal))
   *variables*)
  )

(defun confirm ()
  ""
  (member (rl:readline :prompt (format nil  "~%Do you want to quit ? [Y]/n : "))
          '("y" "Y" "")
          :test 'equal))

(defun repl ()
  (in-package :replic) ;; needed for executable

  ;; register completion
  (rl:register-function :complete #'custom-complete)
  (init-completions) ;; inside a function for executable.

  (functions-to-verbs :replic)
  (functions-to-verbs :replic.user)

  (handler-case
      (do ((i 0 (1+ i))
           (text "")
           (verb "")
           (function nil)
           (variable nil)
           (args ""))
          ((string= "quit" (str:trim text)))
        (setf text
              (rl:readline :prompt (cl-ansi-text:green "replic > ")
                           :add-history t))
        (setf verb (first (str:words text)))
        (setf function (if (member verb *verbs* :test 'equal)
                           ;; might do better than this or.
                           (or (find-symbol (string-upcase verb))
                               (find-symbol (string-upcase verb) :replic.user))))
        (setf variable (if (member verb *variables* :test 'equal)
                           (or (find-symbol (string-upcase verb))
                               (find-symbol (string-upcase verb) :replic.user))))
        (setf args (rest (str:words text)))

        (if (string= verb "NIL")
            ;; that's a C-d
            (if (confirm)
                (uiop:quit))
            (if function
                (handler-case
                    (apply function args)
                  (error (c) (format t "Error: ~a~&" c)))

                (if variable
                    (format t "~a~&" (symbol-value variable))
                    (format t "No command or variable bound to ~a~&" verb))))

        (finish-output)

        )

    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (when (confirm)
             (uiop:quit))))
    (error (c)
      (format t "Unknown error: ~&~a~&" c)))
  )

(defun format-error (msg)
  "Print this message in red on error output."
  (format *error-output* (cl-ansi-text:red msg)))

(defun handle-parser-error (c)
  (format t "Bad command line argument: ~a~&" (opts:option c))
  (opts:describe)
  (uiop:quit))

(defun load-init (&optional file)
  "Load `~/.replic.lisp` or the given file."
  (if file
      (if (probe-file file)
          (progn (load file)
                 (setf *init-file* file))
          (progn (format-error (format nil "The file ~a does not exist.~&" file))
                 (uiop:quit)))
      (when (probe-file *init-file*)
        (load *init-file*))))

(defun main ()
  "Parse command line arguments and start the repl.
  "
  (opts:define-opts
    (:name :help
           :description "Print this help and exit."
           :short #\h
           :long "help")

    (:name :quiet
           :description "Do not load the init file."
           :short #\q
           :long "quiet")

    (:name :load
           :description "Load the given file."
           :short #\l
           :long "load"
           :arg-parser #'string))

  (multiple-value-bind (options)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (handler-case
        (progn
          (if (getf options :help)
              (progn (opts:describe)
                     (uiop:quit)))

          (unless (getf options :quiet)
            (load-init (getf options :load)))

          (repl)
          )
      (error (c) (progn (format *error-output* "~a~&" c)
                        (uiop:quit)))

      )))
