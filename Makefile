LISP ?= sbcl

build:
		sbcl --core '/home/josh/documents/lisp/sbcl/sbcl.core' \
        --load matrix-moonbot.asd  \
		--eval '(ql:quickload "matrix-moonbot")' \
		--eval '(asdf:make "matrix-moonbot")' \
		--eval '(quit)'
