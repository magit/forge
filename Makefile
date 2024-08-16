-include config.mk
include default.mk

.PHONY: lisp docs

all: lisp docs

help:
	$(info make all          - generate lisp and manual)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make redo         - re-generate byte-code and autoloads)
	$(info make docs         - generate all manual formats)
	$(info make redo-docs    - re-generate all manual formats)
	$(info make texi         - generate texi manual)
	$(info make info         - generate info manual)
	$(info make html         - generate html manual file)
	$(info make html-dir     - generate html manual directory)
	$(info make pdf          - generate pdf manual)
	$(info make publish      - publish snapshot manuals)
	$(info make release      - publish release manuals)
	$(info make stats        - generate statistics)
	$(info make stats-upload - publish statistics)
	$(info make clean        - remove most generated files)
	@printf "\n"

lisp:
	@$(MAKE) -C lisp lisp
redo:
	@$(MAKE) -C lisp clean lisp

docs:
	@$(MAKE) -C docs docs
redo-docs:
	@$(MAKE) -C docs redo-docs
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

stats:
	@$(MAKE) -C docs stats
stats-upload:
	@$(MAKE) -C docs stats-upload

clean:
	@$(MAKE) -C lisp clean
	@$(MAKE) -C docs clean
