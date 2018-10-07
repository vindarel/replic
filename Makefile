LISP?=sbcl

build:
	$(LISP)	--non-interactive \
		--load replic.asd \
		--eval '(ql:quickload :replic)' \
		--eval '(asdf:make :replic)'
