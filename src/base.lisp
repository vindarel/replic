;;
;; Base commands for a replic repl.
;;

(defpackage replic.base
  (:use :cl)
  (:import-from :replic
                :help
                :reload
                :hello
                :goodbye
                :vim
                :*verbose*)
  (:shadowing-import-from :replic
                          :set)
  (:export :help
           :set
           :reload
           :goodbye
           :hello
           :vim
           :*verbose*))
