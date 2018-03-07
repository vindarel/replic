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
    (let* ((color (or (find color *colors*)
                     *highlight-default-color*))
           (colorize (get-color-fn color)))
      (loop for word in *highlight-words*
         do (setf input (str:replace-all word
                                         (funcall colorize word)
                                         input)))))
  input)


(defun highlight (word &rest words)
  ;; readline command.
  "Highlight the given words. For this session only,
  use your ~/.replic.lisp to set them permanently."
  (format t "let's highlight ~{~a ~} in ~(~a~)" (cons word words) *highlight-default-color*)
  (setf *highlight-words* (append *highlight-words* (cons word words))))

(defun unhighlight (word &rest words)
  "Un-highlight the given words (for this session)."
  (loop for it in (cons word words)
     do (setf *highlight-words* (remove it *highlight-words* :test #'equal)))
  (format t "~{~a ~} won't be highlighted." (cons word words)))
