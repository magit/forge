-include config.mk
include default.mk

.PHONY: lisp docs clean

all: lisp docs

help:
	$(info make all          - generate lisp and manual)
	$(info make docs         - generate most manual formats)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make texi         - generate texi manual (see comments))
	$(info make info         - generate info manual)
	$(info make html         - generate html manual file)
	$(info make html-dir     - generate html manual directory)
	$(info make pdf          - generate pdf manual)
	$(info make publish      - publish snapshot manuals)
	$(info make release      - publish release manuals)
	$(info make clean        - remove most generated files)
	@printf "\n"

lisp:
	@$(MAKE) -C lisp lisp

docs:
	@$(MAKE) -C docs docs

texi:
	@$(MAKE) -C docs texi

info:
	@$(MAKE) -C docs info

html:
	@$(MAKE) -C docs html

html-dir:
	@$(MAKE) -C docs html-dir

pdf:
	@$(MAKE) -C docs pdf

publish:
	@$(MAKE) -C docs publish

release:
	@$(MAKE) -C docs release

clean:
	@printf "Cleaning...\n"
	@$(MAKE) -C lisp clean
	@$(MAKE) -C docs clean
