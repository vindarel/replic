(in-package :replic)

;; Some functions around printing the help.

(defun help-completion ()
  "Return a list of strings, strings that will be completion candidates."
  ;; used in init-completions
  (append (replic.completion:commands) (replic.completion:variables)))

(defun format-h1 (txt &key (stream *standard-output*))
  "Write txt with an underline."
  (format stream "~&~a~%~a~%" txt (str:repeat (length txt) "=")))

(defun format-help (name function-or-variable &key short)
  "Format a line of help (with right justification etc).
   If `short` is t, print only the first paragraph denoted by two newline charaters (for the overview).
  "
  ;; usage: (find-symbol "HELLO" :replic.base)
  (let* ((doc (documentation (replic.completion:get-package name)
                             function-or-variable))
         (doc (if short
                  (first (cl-ppcre:split "\\n\\n" doc))
                  doc)))
    (format t "~10a~t... ~a~&" name doc)))

(defun help-all ()
  "Print all the help."
  ;; Preamble.
  (unless (str:blank? *help-preamble*)
    (format-code *help-preamble*)
    (format t "~%~%"))

  ;; Help.
  (format-h1 "Available commands")
  (mapcar (lambda (it)
            ;; xxx justify text
            (format-help it 'function :short t))
          ;; sort is destructive and IS harmful !
          (sort (copy-seq (replic.completion:commands)) #'string<))
  (terpri)

  (format-h1 "Available variables")
  (mapcar (lambda (it)
            ;; xxx justify text
            (format-help it 'variable :short t))
          (sort (copy-seq (replic.completion:variables)) #'string<))

  ;; Postamble.
  (unless (str:blank? *help-postamble*)
    (terpri)
    (format t "~a~&" *help-postamble*)))

(defun help-arg (arg)
  "Print the documentation of this command or variable."
  (when (replic.completion:is-function arg)
    (format-help arg 'function))
  (when (replic.completion:is-variable arg)
    (format-help arg 'variable)))

(defun help (&optional arg)
  "Print the help of all available commands. If given an argument, print its documentation.
   Print the first sentence of each command, or print the full text for a particular command.

   Example:
   help help
  "
  ;; possible: show arguments list (swank-backend:arglist), color markdown,
  ;; xxx the 10 padding should adapt to the largest command.
  (if arg
      (help-arg arg)
      (help-all)))
