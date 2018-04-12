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
  (:export :help
           :set
           :reload
           :*verbose*
           :highlight
           :unhighlight))
