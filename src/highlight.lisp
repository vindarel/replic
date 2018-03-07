(in-package :replic)

(defun get-color-fn (name)
  "Return a color function from a color name (a symbol)."
  (let ((fun (assoc-value *colors-functions* name :test #'equal)))
    (unless fun
      ;; ~( ~) = lower string, ~{ ~} = print each element of a list.
      (format *error-output* "~&Color ~(:~a~) is not a valid color name. Choices are ~{~(:~a~) ~}" name *colors*)
      (setf fun (assoc-value *colors-functions* *highlight-default-color* :test #'equal)))
    fun))

(defun highlight-input (input &key (color *highlight-default-color*))
  "Highlight words from *highlight-words* in the given input text (str), only if *highlight* is true.
  Return a string."
  (when *highlight*
    (let ((color (or (find color *colors*)
                     *highlight-default-color*))
          (colorize (get-color-fn color)))
      (print color)
      (print (cl-ansi-text:with-color (color) (print "rst")))
      (loop for word in *highlight-words*
         do (setf input (str:replace-all word
                                         (funcall colorize word)
                                         input)))))
  input)
