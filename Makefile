-include config.mk
include default.mk

.PHONY: lisp clean

all: lisp

help:
	$(info make all          - generate byte-code and autoloads)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make clean        - remove byte-code and autoloads)
	@printf "\n"

lisp:
	@$(MAKE) -C lisp lisp

clean:
	@$(MAKE) -C lisp clean

