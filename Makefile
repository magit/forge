-include config.mk
include default.mk

.PHONY: lisp docs

all: lisp docs

help:
	$(info make all          -- Generate lisp and manual)
	$(info make lisp         -- Generate byte-code and autoloads)
	$(info make redo         -- Re-generate byte-code and autoloads)
	$(info make docs         -- Generate all manual formats)
	$(info make redo-docs    -- Re-generate all manual formats)
	$(info make texi         -- Generate texi manual)
	$(info make info         -- Generate info manual)
	$(info make html         -- Generate html manual file)
	$(info make html-dir     -- Generate html manual directory)
	$(info make pdf          -- Generate pdf manual)
	$(info make publish      -- Publish snapshot manuals)
	$(info make release      -- Publish release manuals)
	$(info make stats        -- Generate statistics)
	$(info make stats-upload -- Publish statistics)
	$(info make clean        -- Remove most generated files)
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
