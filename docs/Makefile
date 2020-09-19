-include ../config.mk
include ../default.mk

docs: info html html-dir pdf

info: $(PKG).info dir
html: $(PKG).html
pdf:  $(PKG).pdf

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH) -l ox-extra -l ox-texinfo+.el
ORG_EVAL  = --eval "(ox-extras-activate '(ignore-headlines))"
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

# This target first bumps version strings in the Org source.  The
# necessary tools might be missing so other targets do not depend
# on this target and it has to be run explicitly when appropriate.
#
#   AMEND=t make texi    Update manual to be amended to HEAD.
#   VERSION=N make texi  Update manual for release.
#
.PHONY: texi
texi:
	@$(EMACS) $(ORG_ARGS) $(PKG).org $(ORG_EVAL)
	@printf "\n" >> $(PKG).texi
	@rm -f $(PKG).texi~

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@printf "Generating $@\n"
	@printf "%s" $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

HTML_FIXUP_CSS    = '/<link rel="stylesheet" type="text\/css" href="\/assets\/page.css">/a\
<link rel="icon" href="/assets/magit_alt1.ico">\
\n<link class="s-css-s--style" rel="stylesheet"           title="Default"               href="/assets/themes/default.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Default high contrast" href="/assets/themes/default-high-contrast.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Solarized dark xterm"  href="/assets/themes/solarized-dark-xterm.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Black on white"        href="/assets/themes/black-on-white.css">\
\n<script src="/assets/js/simple-css-switch.js"></script>'
HTML_FIXUP_ONLOAD = 's/<body lang="en">/<body lang="en" onload="simpleCssSwitch()">/'
HTML_FIXUP_MENU   = '/<\/body>/i<div id="s-css-s--menu"><\/div>'

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<
	@sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $@

html-dir: $(PKG).texi
	@printf "Generating $(PKG)/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) $<
	@for f in $$(find $(PKG) -name '*.html') ; do \
	sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $$f ; \
	done

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

DOMAIN         ?= magit.vc
PUBLISH_PATH   ?= /manual/
RELEASE_PATH   ?= /manual/$(VERSION)/

S3_BUCKET      ?= s3://$(DOMAIN)
PUBLISH_TARGET  = $(S3_BUCKET)$(PUBLISH_PATH)
RELEASE_TARGET  = $(S3_BUCKET)$(RELEASE_PATH)

CFRONT_DIST    ?= E2LUHBKU1FBV02
CFRONT_PATHS    = $(PKG).html $(PKG).pdf $(PKG)/*

comma := ,
empty :=
space := $(empty) $(empty)

publish: html html-dir pdf
	@aws s3 cp $(PKG).html $(PUBLISH_TARGET)
	@aws s3 cp $(PKG).pdf  $(PUBLISH_TARGET)
	@aws s3 sync $(PKG)    $(PUBLISH_TARGET)$(PKG)/
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation --distribution-id $(CFRONT_DIST) --paths \
	"$(subst $(space),$(comma),$(addprefix $(PUBLISH_PATH),$(CFRONT_PATHS)))" > /dev/null

release: html html-dir pdf
	@aws s3 cp $(PKG).html $(RELEASE_TARGET)
	@aws s3 cp $(PKG).pdf  $(RELEASE_TARGET)
	@aws s3 sync $(PKG)    $(RELEASE_TARGET)$(PKG)/
	@aws s3 cp $(PUBLISH_TARGET)dir.html $(RELEASE_TARGET)dir.html
	@aws s3 cp $(PUBLISH_TARGET)dir/index.html $(RELEASE_TARGET)dir/index.html
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation --distribution-id $(CFRONT_DIST) --paths \
	"$(subst $(space),$(comma),$(addprefix $(RELEASE_PATH),$(CFRONT_PATHS)))" > /dev/null

CLEAN = $(PKG).info dir $(PKG) $(PKG).html $(PKG).pdf

clean:
	@rm -rf $(CLEAN)
