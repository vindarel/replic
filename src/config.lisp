(defpackage replic.config
  (:use :cl)
  (:import-from :replic.utils
                :truthy
                :falsy)
  (:export :apply-config
           :read-config
           :has-option-p
           :option))

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


(defun read-config (&optional (cfg-file *cfg-file*))
  "Search for the config files, parse the config, and return the config object.
   Three locations: in the package root, in ~/config/, in the home.
   If no `cfg-file` argument is given, use the global `*cfg-file*` (\".replic.conf\")."
  (setf *cfg-sources* (list
                       ;; Setting here and not in defparameter:
                       ;; ensure this is the user's value, not where the binary was built on.
                       (merge-pathnames (str:concat ".config/" cfg-file) (user-homedir-pathname))
                       (merge-pathnames cfg-file (user-homedir-pathname))
                       cfg-file))
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

(defun has-option-p (option &optional (package/section "default"))
  "Check if the config object has `option` in section
  `package/section` (the section is inferred from the package name)."
  (ignore-errors
    (py-configparser:has-option-p *cfg*
                                  ;; :PACKAGE -> "package" section name.
                                  (str:downcase package/section)
                                  (no-earmuffs option))))

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
      (string-trim "*" var)
      var))

(defun set-option (var val package)
  "Get the symbol associated to `var` in 'package' and set it."
  (setf (symbol-value (find-symbol (string-upcase var) package))
        val))

(defun read-option (key package)
  "Interpret the value of this option: t, true or 1 means true, parse integers, etc.

   key: an existing variable of the given package, which will be set from the config file."
  (let ((val (option (no-earmuffs key) :section (str:downcase package))))
    (cond
      ((truthy val)
       (set-option key t package))

      ((falsy val)
       (set-option key nil package))

      ((null (ignore-errors (parse-integer val)))
       (set-option key val package))

      (t
       ;; Integer ? Try to parse it.
       (unless
           (ignore-errors
             (when (parse-integer val)
               (set-option key (parse-integer val) package))))))))

(defun print-options (&optional (section "default" section-p))
  (loop for section in (if section-p (list section)
                           (py-configparser:sections *cfg*))
     do (loop for item in (py-configparser:items *cfg* section)
           do (format t "~a: ~a~&" (car item) (cdr item)))))

(defun apply-config (&optional (section "default") (package :replic) (cfg-file *cfg-file*))
  "Read the config files and for every variable of this section, get its new value.
   Apply the configuration settings for the default package's variables.
   In the config file, variables don't have lispy earmuffs.

   Example .replic.conf:

   [default]
   confirm-exit: false

   Then call:

   (replic.config:apply-config)

   by default, this reads the 'default' section and looks for parameters of the REPLIC package.

   You could read another section for your app, for instance:

   [default]
   confirm-exit: true

   [my-app]
   confirm-exit: false

   (replic.config:apply-config \"my-app\")

"
  (declare (ignorable section))
  (read-config cfg-file)
  (mapcar (lambda (var)
            ;; (format t "apply-config: has ~a an option for ~a? ~a~&" (str:downcase section)
            ;;         var
            ;;         (has-option-p var section))
            (when (has-option-p var (str:downcase section))
              ;; (uiop:format! t "~tapplying option: ~a~&" var (has-option-p var section))
              (read-option var package)))
          (get-exported-variables package))
  t)
