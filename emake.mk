-include config.mk
include default.mk

.PHONY: run clean install

$(EMAKE_WORKDIR)/elpa: EMACS_ARGS += --batch
$(EMAKE_WORKDIR)/elpa: $(EMAKE_WORKDIR)/emake.el
	$(EMAKE) install

$(EMAKE_WORKDIR)/emake.el:
	@mkdir -p $(EMAKE_WORKDIR)
	$(CURL) 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.el' \
	    --output '$(EMAKE_WORKDIR)/emake.el'

clean:
	@printf "Cleaning...\n"
	@rm -rf $(EMAKE_WORKDIR)

# Run Emacs with minimal configuration
dev-run: $(EMAKE_WORKDIR)/emake.el $(EMAKE_WORKDIR)/elpa
	@$(EMAKE_BASE) -f emake-setup-load-path

# Like `dev-run', but always install from MELPA
run: EMAKE_ENV += EMAKE_USE_LOCAL="NEVER"
run: dev-run

install: $(EMAKE_WORKDIR)/elpa

compile: EMACS_ARGS += --batch
compile: $(EMAKE_WORKDIR)/elpa $(EMAKE_WORKDIR)/emake.el
	@$(EMAKE) compile

loaddefs: EMACS_ARGS += --batch
loaddefs: $(PACKAGE_LISP) $(EMAKE_WORKDIR)/emake.el
	@$(EMAKE) autoloads
