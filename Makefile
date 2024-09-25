LISP ?= sbcl

build:
		sbcl \
        --load matrix-moonbot.asd  \
		--eval '(ql:quickload "matrix-moonbot")' \
		--eval '(asdf:make "matrix-moonbot")' \
		--eval '(quit)'
