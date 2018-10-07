(defpackage replic.config
  (:use :cl)
  (:export :apply-config))

(in-package :replic.config)

(defvar *cfg-file* ".replic.conf"
  "Default name of the config file.")

(defvar *cfg* (py-configparser:make-config)
  "The config read from the config.conf files found, in order: in this
  project root, ~/.config/replic.conf, in the current directory.")

(defvar *cfg-sources* nil
  "List of files to read the config, in order.")

(defvar *section* "default"
  "Default section header of the config file(s) to read parameters from.")


(defun read-config (package)
  (setf *cfg-sources* (list
                       (asdf:system-relative-pathname package *cfg-file*)
                       ;; Setting here and not in defparameter:
                       ;; ensure this is the user's value, not where the binary was built on.
                       (merge-pathnames (str:concat ".config/" *cfg-file*) (user-homedir-pathname))
                       (merge-pathnames *cfg-file* (user-homedir-pathname))
                       *cfg-file*))
  (loop for it in *cfg-sources*
     do (progn
          (when (probe-file it)
            ;; xxx verbose
            ;; (format t "reading config ~a~&" it)
            ;; read-files reads a list.
            (py-configparser:read-files *cfg* (list it)))))
  *cfg*)

(defun config-files ()
  *cfg-sources*)

(defun has-config-p ()
  "Return nil if either we didn't find config files or they don't have
  any section."
  (when (or (config-files)
            (py-configparser:sections *cfg*))
    t))

(defun has-option-p (option &key (section *section*))
  (ignore-errors
    (py-configparser:has-option-p *cfg* section option)))

(defun option (option &key (section *section*))
  "Return this option's value (as string)."
  (if (py-configparser:has-section-p *cfg* section)
      (py-configparser:get-option *cfg* section option)
      (values nil (format nil  "no such section: ~a" section))))

;;
;; Apply replic's default config:
;; - get all replic's exported variables,
;; - search for them without earmuffs in the config file,
;; - get the option and set the variable back in the package.
;;
(defun get-exported-variables (package)
  (assert (symbolp package))
  (let ((replic.completion::*variables* nil)
        (replic.completion::*commands* nil))
    (replic.completion:functions-to-commands package)
    (replic.completion:variables)))

;; (defun copy-no-earmuffs (vars)
;;   "From this list of variables (strings), duplicate all of them, but without earmuffs.
;;    So writing the config file is normal for a non-lispy user, while without surprise for a lisper (which shall have a lisp configuration file anyway)."
;;   (assert (listp vars))
;;   (when vars
;;     (append vars
;;             (mapcar (lambda (it)
;;                       (when (str:starts-with? "*" it)
;;                         (string-trim "*" it)))
;;                     vars))))

(defun no-earmuffs (var)
  "Return this parameter's name, without earmuffs ('*').
   ;; So the config file can suit non-lispers and lispers (who also shall have a lisp configuration file anyway)."
  (if (str:starts-with? "*" var)
      (string-trim "*" var)))

(defparameter *true-list* '("t" "true" "True")
  "List of strings meaning 'true'.")

(defparameter *false-list* '("nil" "false" "False")
  "List of strings meaning 'false'.")

(defun truthy (it)
  (position it *true-list* :test #'equalp))

(defun falthy (it)
  (position it *false-list* :test #'equalp))

(defun set-option (var val package)
  "Get the symbol associated to `var` in 'package' and set it."
  (setf (symbol-value (find-symbol (string-upcase var) package))
        val))

(defun read-option (key package)
  "Interpret the value of this option: t, true or 1 means true, parse integers, etc.

   key: an existing variable of the given package, which will be set from the config file."
  (let ((val (option (no-earmuffs key))))
    (cond
      ((truthy val)
       (format t "~a is truthy~&" key)
       (set-option key t package))

      ((falthy val)
       (format t "~a is falthy.~&" key)
       (set-option key nil package))

      ((null (ignore-errors (parse-integer val)))
       (format t "~a is a string: ~a~&" key val)
       (set-option key val package))

      (t
       ;; Integer ? Try to parse it.
       (unless
           (ignore-errors
             (when (parse-integer val)
               (format t "~a is an int: ~a~&" key val)
               (set-option key (parse-integer val) package))))))))

(defun apply-config (package)
  "Read the config files and for every variable of this package, get its new value.
   In the config file, variables don't get lispy earmuffs."
  (declare (ignorable package))
  (format t "-- reading config...~&")
  (force-output)
  (read-config package) ;TODO: very long ??
  (format t "-- config: ~a~&" *cfg*)
  (mapcar (lambda (var)
            (when (has-option-p (no-earmuffs var))
              (read-option var package)))
          (get-exported-variables package)))
