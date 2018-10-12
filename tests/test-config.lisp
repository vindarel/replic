(in-package :replic-test)

(use-package :replic.config)

(defvar *cfg-input* nil)

(setf *cfg-input*
"[default]
bar = baz
verbose = true
an-int = 1
")

(defmacro with-config (&body body)
  `(let* ((replic.config::*cfg* (py-configparser:read-stream replic.config::*cfg* (make-string-input-stream *cfg-input*))))
     ,@body))

(with-config
  (ok (replic.config::read-config :replic)
      "Read config files."))

(with-config
  (ok (replic.config::has-config-p)
      "has-config-p"))

(with-config
  (ok (position "*confirm-exit*"
                (replic.config::get-exported-variables :replic)
                :test #'equalp)
      "get-exported-variables"))

;xxx: fix api: "default", :section, :replic...
(with-config
  (ok (replic.config::has-option-p "bar" "default")
      "has-option-p"))

(with-config
  (is (replic.config::option "bar" :section "default")
      "baz"
      "option value"))

(with-config
  (ok (not (replic.config::read-option "*confirm-exit*" :replic))
      "read-option"))

(finalize)
