(in-package :replic.user)

;;
;; Example functions one can copy in ~/.replic.lisp
;;

(defun assoc-value (alist key &key (test #'equalp))
  ;; utility.
  ;; Don't import Alexandria just for that.
  (cdr (assoc key alist :test test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demo examples.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun vim ()
  "Run vim."
  (uiop:run-program "vim"
                    :output :interactive
                    :input :interactive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listen to radios. Autocomplete their names.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *radios* '(
                         ("fip" . "http://direct.fipradio.fr/live/fip-midfi.mp3")
                         ("lora" . "http://live.lora924.de:8000/lora.ogg")
                         ("culture" . "http://direct.franceculture.fr/live/franceculture-midfi.mp3"))

  "Alist of radio names / url.")

(defparameter *radios-names* (mapcar (lambda (it)
                                       (first it))
                                     *radios*)
  "List of radio names (for completion).")

(defun radio (it)
  "Choose and listen a radio stream from a list."
  (uiop:run-program (list "mpv" (assoc-value *radios* it :test #'equal))
                    :input :interactive
                    :output :interactive))

(replic.completion:add-completion "radio" *radios-names*)

;; Add the list of radios to the command documentation
;; so that it is visible by "help radio".
;; Print a coma-separated list of radios, finished by a "and" and a point.
;; thanks http://random-state.net/features-of-common-lisp.html#The_FORMAT_function
(let ((doc (documentation #'radio 'function)))
  (setf (documentation #'radio 'function) (format nil "~a~&~%The list of available radio streams is: ~a" doc (format nil "~{~a~#[.~; and ~:;, ~]~}" *radios-names*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run vim.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vim ()
  "Run vim."
  (uiop:run-program "vim"
                    :output :interactive
                    :input :interactive))
;; todo: file completion.

(export '(vim
          radio
          ))
