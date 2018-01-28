(defpackage replic
  (:use :cl)
  (:export :main
           :help
           :goodbye
           :hello
           :echo))
(in-package :replic)


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
(defparameter *names* '()
  "List of names (string) given to `hello`. Will be autocompleted by `goodbye`.")

(defun hello (name)
  "Takes only one argument. Adds the given name to the global
  `*names*` global variable, used to complete arguments of `goodbye`.
  "
  (format t "hello ~a~&" name)
  (push name *names*))

(defun goodbye (name)
  "Says goodby to name, where `name` should be completed from what was given to `hello`."
  (format t "goodbye ~a~&" name))

(defun init-completions ()
  (push '("goodbye" . *names*) *args-completions*))

(defun echo (string &rest more)
  "Print the rest of the line. Takes any number of arguments."
  (format t "~a~{ ~a~}~&" string more))

;;
;; Lib
;;
;; (defun assoc-value (alist key &key (test #'equalp))
;;   ;; Don't import Alexandria just for that.
;;   ;; See also Quickutil to import only the utility we need.
;;   ;; http://quickutil.org/lists/
;;   (cdr (assoc key alist :test test)))

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


(defun select-completions (text list)
  "Select all verbs from `list' that start with `text'."
  (let ((els (remove-if-not (alexandria:curry #'str:starts-with? text)
                            list)))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

(defun complete-for-args (text line)
  "Completion for arguments."
  (let* ((verb (first (str:words line)))
         (list-or-function (alexandria:assoc-value *args-completions* verb :test 'equal)))
    (if list-or-function
        ;; with a list of strings.
        ;; todo: with a function.
        (select-completions text (symbol-value list-or-function)))))

(defun custom-complete (text start end)
  "Complete a symbol.

  text is the partially entered word. start and end are the position on `rl:*line-buffer*'.

  When the cursor is at the beginning of the prompt, complete from `*verbs*`.

  "
  (declare (ignore end))
  (if (zerop start)
      (select-completions text *verbs*)
      (complete-for-args text rl:*line-buffer*)))

(defun functions-to-verbs ()
  "Add exported functions of `*package*` to the list of `*verbs*` to complete,
   add exported variables to the list of `set`-able variables.

   Remove any symbol named 'main'.

  "
  (do-external-symbols (it)
    (push (string-downcase (string it)) *verbs*))
  (setf *verbs* (remove "main" *verbs* :test 'equal))
  )

(defun repl ()
  (in-package :replic) ;; needed for executable

  ;; register completion
  (rl:register-function :complete #'custom-complete)
  (init-completions)                    ;; inside a function for executable.

  (functions-to-verbs)
  (format t "verbs: ~a~&" *verbs*)
  (handler-case
      (do ((i 0 (1+ i))
           (text "")
           (verb "")
           (function nil)
           (args ""))
          ((string= "quit" (str:trim text)))
        (setf text
              (rl:readline :prompt (cl-ansi-text:green "replic > ")
                           :add-history t))
        (setf verb (first (str:words text)))
        (setf function (find-symbol (string-upcase verb)))
        (setf args (rest (str:words text)))

        (if function
            (handler-case
                (apply function args)
              (error (c) (format t "Error: ~a~&" c)))

            (format t "No command bound to ~a~&" verb))

        (finish-output)

        )

    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (uiop:quit)))
    (error (c)
      (format t "Unknown error: ~&~a~&" c)))
  )

(defun handle-parser-error (c)
  (format t "cli args parser error: ~a~&" (opts:option c)))

(defun main ()
  "Parse command line arguments and start the repl.
  "
  (opts:define-opts
    (:name :help
           :description "Print this help and exit."
           :short #\h
           :long "help"))

  (multiple-value-bind (options)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (if (getf options :help)
        (opts:describe))

    (repl)))
