LISP?=sbcl

build:
	$(LISP) --load replic.asd \
		--eval '(ql:quickload :replic)' \
		--eval '(asdf:make :replic)' \
		--eval '(quit)'
