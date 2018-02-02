(in-package :replic)

(defparameter *colorize-output* t
  "Enable printing text with ansi colors.")

;; uiop:run-program searches PATH on at least some implementations,
;; may need to specify full path or pass :FORCE-SHELL T to
;; uiop:run-program if it doesn't on others
;; thanks 3bmd
(defparameter *pygments-command* "pygmentize")

;; run-program expects utf8
(defparameter *pygments-args* '("-f" "console"
                                "-P" "encoding=utf-8"
                                "-P" "noclobber_cssfile=True"))

(defun format-code (txt &key (lang "md") params (stream t))
  "Output formatted txt for terminal display. The default is to treat txt as markdown and to output ansi color codes for display in a console. The input format can be changed with `:lang`.

   Needs pygments >= 2.2 (sudo pip install -U pygments)

   pygmentize:

   -l <lexer> one of md, cl, lisp,... see all with -L.

   -f <formatter> Set the formatter name. If not given, it will be
    guessed from the extension of the output file name. If no output
    file is given, the terminal formatter will be used by default.
    => console, console256, html,...
   "
  ;; thanks 3bmd
  (if *colorize-output*
      (handler-case
          (let ((formatted (uiop:run-program `(,*pygments-command*
                                               ,@ *pygments-args*
                                               ,@ (when lang
                                                    `("-l" ,lang))
                                               ,@ (when params
                                                    `("-O" ,params)))
                                             :external-format :utf-8
                                             :input (make-string-input-stream txt)
                                             :output :string)))
            (format stream "~a~&" formatted))
        (error (c) (progn
                     (format *error-output* "~a~&Did you install pygments ?~&~%" c)
                     (format stream txt))))
      (format stream txt)))
