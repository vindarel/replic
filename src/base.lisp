;;
;; Base commands for a replic repl.
;;

(defpackage replic.base
  (:use :cl)
  (:import-from :replic
                :help
                :reload
                :highlight
                :unhighlight
                :*verbose*)
  (:shadowing-import-from :replic
                          :set)
  (:shadow :quit)
  (:export :help
           :set
           :reload
           :*verbose*
           :highlight
           :unhighlight
           :quit))

(in-package replic.base)

(replic.completion:add-completion "set" #'replic.completion:variables)

(replic.completion:add-completion "help" #'replic::help-completion)

(defun quit ()
  "Quit the application."
  (uiop:quit))
