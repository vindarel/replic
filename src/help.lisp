(in-package :replic)

;; Some functions around printing the help.

(defun help-completion ()
  "Return a list of strings, strings that will be completion candidates."
  ;; used in init-completions
  (append *commands* *variables*))

(defun format-h1 (txt &key (stream *standard-output*))
  "Write txt with an underline."
  (format stream "~&~a~%~a~%" txt (str:repeat (length txt) "=")))

(defun format-help (name function-or-variable)
  "Format a line of help (with right justification etc)."
  (format t "~10a~t...~a~&" name (documentation (find-symbol (string-upcase name)) function-or-variable)))

(defun help-all ()
  "Print all the help."
  (when *help-preamble*
    (format-code *help-preamble*)
    (format t "~%~%"))
  (format-h1 "Available commands")
  (mapcar (lambda (it)
            ;; xxx justify text
            (format-help it 'function))
          (sort *commands* #'string<))
  (terpri)
  (format-h1 "Available variables")
  (mapcar (lambda (it)
            ;; xxx justify text
            (format-help it 'variable))
          (sort *variables* #'string<)))

(defun help-arg (arg)
  "Print the documentation of this command or variable."
  (when (member arg *commands* :test #'equal)
    (format-help arg 'function))
  (when (member arg *variables* :test #'equal)
    (format-help arg 'variable)))

(defun help (&optional arg)
  "Print the help of all available commands. If given an argument, print its documentation."
  ;; possible: show arguments list (swank-backend:arglist), color markdown,
  ;; preamble, postamble,...
  ;; xxx the 10 padding should adapt to the largest command.
  (if arg
      (help-arg arg)
      (help-all)))
