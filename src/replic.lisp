(defpackage replic
  (:use :cl)
  (:shadow #:set)
  (:import-from :alexandria
                :curry
                :assoc-value)
  (:export :main
           :confirm
           :repl
           :init-completions
           :functions-to-commands
           :help
           :set
           :reload
           ;; examples:
           :goodbye
           :hello
           :echo
           :vim
           :sleep3
           ;; settings
           :*args-completions*
           :*commands*
           :*custom-complete*
           :*default-command-completion*
           :*highlight*
           :*highlight-words*
           :*prompt*
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

(defparameter *prompt* "> "
  "The prompt. Can contain ansi colours (use cl-ansi-text:green etc).")

(defparameter *commands* '()
  "List of commands for the REPL.")

(defparameter *args-completions* '()
  "Alist that associates a command name (verb) to:

  a) either a list of strings,

  b) either a function returning the completion candidates. This
  function takes the partially entered argument as argument.

  Example usage:

  (push '(\"goodbye\" . *names*) *args-completions*)

  ")

(defparameter *default-command-completion* nil
  "A variable, list or function to use to complete all commands that don't have an associated completion method.")

;;
;; Highlight / colorize words on output.
;;
(defparameter *highlight* t
  "If true, highlight words of *highlight-words* on standard output. Defaults to true.")

(defparameter *highlight-words* nil
  "A list of words to highlight in standard output.")

(defparameter *highlight-default-color* :yellow
  "Default color for highlighting. Symbol of the *colors* list. Defaults to yellow.")

(defparameter *colors* '(:black :red :green :yellow :blue :magenta :cyan :white)
  "List of color symbols available for text highlighting. From cl-ansi-text.")

(defparameter *colors-functions* '(
                                   (:blue . cl-ansi-text:blue)
                                   (:green . cl-ansi-text:green)
                                   (:yellow . cl-ansi-text:yellow)
                                   (:cyan . cl-ansi-text:cyan)
                                   (:magenta . cl-ansi-text:magenta)
                                   (:red . cl-ansi-text:red))
  "Alist of a symbol - its function to colorize text. From cl-ansi-text.")

(defparameter *highlight-default-style :foreground
  "Default cl-ansi-text style, :foreground (or :background) (a yellow background isn't always readable).")

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

(defun sleep3 ()
  (sleep 3))

(defun vim ()
  "Run vim."
  (uiop:run-program "vim"
                    :output :interactive
                    :input :interactive))

;;
;; Lib
;;
;; (defun assoc-value (alist key &key (test #'equalp))
;;   ;; Don't import Alexandria just for that.
;;   ;; See also Quickutil to import only the utility we need.
;;   ;; http://quickutil.org/lists/
;;   (cdr (assoc key alist :test test)))

(defparameter *help-preamble* ""
  "Text to display before the list of commands and variables.")

(defparameter *help-postamble* "For details, see the help of each command or variable."
  "Text to display after the list of commands and variables.")

(defun init-completions ()
  (push '("goodbye" . *names*) *args-completions*)
  (push '("set" . *variables*) *args-completions*)
  (push  (cons "help" #'help-completion) *args-completions*))

(defun echo (string &rest more)
  "Print the rest of the line. Takes any number of arguments."
  (format t "~a~{ ~a~}~&" string more))

;; shadow works with build but not on Slime ??
(defun set (var arg)
  "Change this variable. t and nil denote true and false."
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
  "Select all commands from `list' that start with `text'."
  (let ((els (remove-if-not (curry #'str:starts-with? text)
                            list)))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

(defun complete-args (text line)
  "Completion for arguments.

   Take the list of completion candidates from the `*args-completions*` alist."
  (let* ((verb (first (str:words line)))
         (list-or-function (or
                            (assoc-value *args-completions* verb :test 'equal)
                            *default-command-completion*)))
    (when list-or-function
      (cond
        ((symbolp list-or-function)
         ;; with a variable referencing a list of strings.
         (complete-from-list text (symbol-value list-or-function)))

        ((functionp list-or-function)
         ;; with a function that returns a list of strings.
         (complete-from-list text (funcall list-or-function)))

        ;; can be a list (consp).
        (t
         (complete-from-list text list-or-function))))))

(defun custom-complete (text start end)
  "Complete a symbol.

  text is the partially entered word. start and end are the position on `rl:*line-buffer*'.

  When the cursor is at the beginning of the prompt, complete from `*commands*`.

  "
  (declare (ignore end))
  (if (zerop start)
      (if (str:starts-with? "*" text)
          (complete-from-list text *variables*)
          (complete-from-list text *commands*))
      (complete-args text rl:*line-buffer*)))

(defparameter *custom-complete* #'custom-complete
  "Completion function.

   When the cursor is at the beginning of the prompt, the default
   function tries to complete a command or a variable (starting with
   `*`.

   Afterwards, it reads how to complete the function/variable
   arguments from `complete-args`.")

(defparameter *variables* '()
  "List of parameters (str), setable with `set`.")

(defparameter *commands-package* '()
  "Association of a command and its package (symbol).

  Used to find it, read its documentation, etc. Use find-package to
  get the package object.")

(defun functions-to-commands (&optional package)
  "Add exported functions of `*package*` to the list of `*commands*` to complete,
   add exported variables to the list of `set`-able variables.

   Remove any command named 'main'.

  "
  (assert (symbolp package))
  (do-external-symbols (it package)
    (if (str:starts-with? "*" (string it))
        (push (string-downcase (string it)) *variables*)
        (push (string-downcase (string it)) *commands*))
    ;; Remember what package this function/variable comes from (specially to get its help).
    ;; xxx use an interface.
    (push (cons (string-downcase (string it)) package) *commands-package*))
  (values
   (setf *commands* (remove "main" *commands* :test 'equal))
   *variables*)
  )

(defun confirm ()
  "Ask confirmation. Nothing means yes."
  (member (rl:readline :prompt (format nil  "~%Do you want to quit ? [Y]/n : "))
          '("y" "Y" "")
          :test 'equal))

(defun print-results (input &key (stream t))
  "Print the given results string, apply transformations if needed (highlight text)."
  (assert (stringp input))
  ;; see highlight.lisp
  (setf input (highlight-input input))
  (format stream "~&~a~&" input))

(defun repl ()
  (in-package :replic) ;; needed for executable

  ;; register completion
  (rl:register-function :complete #'custom-complete)

  (handler-case
      (do ((i 0 (1+ i))
           (text "")
           (verb "")
           (function nil)
           (variable nil)
           (args "")
           (standard-output ""))
          ((string= "quit" (str:trim text)))
        (setf text
              (rl:readline :prompt *prompt*
                           :add-history t))

        (if (string= text "NIL")
            ;; that's a C-d, a blank input is just "".
            (when (confirm)
              (uiop:quit)))

        (unless (str:blank? text)
          (setf verb (first (str:words text)))
          (setf function (if (member verb *commands* :test 'equal)
                             ;; might do better than this or.
                             (find-symbol (string-upcase verb) (assoc-value *commands-package* verb :test #'equal))))
          (setf variable (if (member verb *variables* :test 'equal)
                             (find-symbol (string-upcase verb) (assoc-value *commands-package* verb :test #'equal))))
          (setf args (rest (str:words text)))


          (if (and verb function)
              (handler-case
                  ;; Call the function.
                  ;; Capture standard output in order to post-process it (highlight stuff).
                  (progn
                    (let ((*standard-output* (make-string-output-stream)))
                      (apply function args)
                      (setf standard-output (get-output-stream-string *standard-output*)))
                    (print-results standard-output))
                (#+sbcl sb-sys:interactive-interrupt (c)
                        (declare (ignore c))
                        (terpri))
                (error (c) (format t "Error: ~a~&" c)))

              (if variable
                  (format t "~a~&" (symbol-value variable))
                  (format t "No command or variable bound to ~a~&" verb)))

          (finish-output)))

    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (when (confirm)
             (uiop:quit))))
    (error (c)
      (format t "~&Unknown error: ~&~a~&" c)))
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

(defun reload ()
  "Reload the lisp file loaded at startup."
  (load-init *init-file*))

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

          (setf *prompt* (cl-ansi-text:green "replic > "))

          ;; replic initialization:
          (init-completions)

          ;; create commands from the exported functions and variables.
          (functions-to-commands :replic.base)

          ;; launch the repl.
          (repl))
      (error (c) (progn (format *error-output* "~a~&" c)
                        (uiop:quit))))))
