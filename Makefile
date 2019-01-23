LISP?=sbcl

all: test

build:
	$(LISP)	--non-interactive \
		--eval '#-quicklisp (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))' \
		--load replic.asd \
		--eval '(ql:quickload :replic)' \
		--eval '(asdf:make :replic)'

test:
	$(LISP) --non-interactive \
		--eval '#-quicklisp (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))' \
		--load replic.asd \
		--load replic-test.asd \
		--eval '(ql:quickload :replic)' \
		--eval '(ql:quickload :replic-test)' \
	     	--eval '(prove:run #P"tests/test-replic.lisp")' \
	     	--eval '(prove:run #P"tests/test-config.lisp")'

# Install dependencies, mostly for Docker.
install:
	# cl-readline needs the update of may, 10th for history read and write.
	git clone https://github.com/vindarel/cl-readline ~/quicklisp/local-projects/
