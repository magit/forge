-include ../config.mk
include ../default.mk

.PHONY: stats

docs: texi info html html-dir pdf

texi:     $(PKG).texi
info:     $(PKG).info dir
html:     $(PKG).html
html-dir: $(PKG)/index.html
pdf:      $(PKG).pdf

ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --eval "\
(defun org-texinfo--sanitize-content (text)\
  (replace-regexp-in-string \"[@@{}]\" \"@@\\&\" text))"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

redo-docs:
	@touch $(PKG).org
	@make docs

.revdesc: ;
_    := $(shell test "$(REVDESC)" = "$$(cat .revdesc 2> /dev/null)" ||\
        echo "$(REVDESC)" > .revdesc)

%.texi: %.org .orgconfig .revdesc
	@printf "Generating $@\n"
	@$(EMACS_ORG) $< $(ORG_EVAL)

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@printf "Generating $@\n"
	@printf "%s" $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

HTML_FIXUP_CSS    = '/<link rel="stylesheet" type="text\/css" href="https:\/\/$(DOMAIN)\/assets\/page.css">/a\
<link rel="icon" href="https://$(DOMAIN)/assets/magit_alt1.ico">\
\n<link class="s-css-s--style" rel="stylesheet"           title="Default"               href="https://$(DOMAIN)/assets/themes/default.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Default high contrast" href="https://$(DOMAIN)/assets/themes/default-high-contrast.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Solarized dark xterm"  href="https://$(DOMAIN)/assets/themes/solarized-dark-xterm.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Black on white"        href="https://$(DOMAIN)/assets/themes/black-on-white.css">\
\n<script src="https://$(DOMAIN)/assets/js/simple-css-switch.js"></script>'
HTML_FIXUP_ONLOAD = 's/<body lang="en">/<body lang="en" onload="simpleCssSwitch()">/'
HTML_FIXUP_MENU   = '/<\/body>/i<div id="s-css-s--menu"><\/div>'

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<
	@sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $@

%/index.html: %.texi
	@printf "Generating $(PKG)/*.html\n"
	@rm -rf $(PKG)
	@$(MAKEINFO) --html -o $(PKG)/ $(MANUAL_HTML_ARGS) $<
	@for f in $$(find $(PKG) -name '*.html') ; do \
	sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $$f ; \
	done

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

DOCS_DOMAIN = docs.$(DOMAIN)
STAT_DOMAIN = stats.$(DOMAIN)
SNAP_TARGET = $(subst .,_,$(DOCS_DOMAIN)):devel/$(PKG)/
DOCS_TARGET = $(subst .,_,$(DOCS_DOMAIN)):$(PKG)/
STAT_TARGET = $(subst .,_,$(STAT_DOMAIN)):$(PKG)/

publish: redo-docs
	@printf "Publishing snapshot manual...\n"
	@cp $(PKG).pdf $(PKG)/$(PKG).pdf
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG) $(SNAP_TARGET)

release: redo-docs
	@printf "Publishing release manual...\n"
	@cp $(PKG).pdf $(PKG)/$(PKG).pdf
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG) $(DOCS_TARGET)

stats:
	@printf "Generating statistics...\n"
	@$(GITSTATS) $(GITSTATS_ARGS) $(TOP) $(GITSTATS_DIR)

stats-upload:
	@printf "Uploading statistics...\n"
	@$(RCLONE) sync $(RCLONE_ARGS) stats $(STAT_TARGET)

CLEAN = $(PKG).info dir $(PKG) $(PKG).html $(PKG).pdf $(GITSTATS_DIR)

clean:
	@printf " Cleaning docs/*...\n"
	@rm -rf $(CLEAN)
