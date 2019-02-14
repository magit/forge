PKG = forge

ELS  += $(PKG)-db.el
ELS  += $(PKG)-core.el
ELS  += $(PKG).el
ELS  += $(PKG)-repo.el
ELS  += $(PKG)-post.el
ELS  += $(PKG)-topic.el
ELS  += $(PKG)-issue.el
ELS  += $(PKG)-pullreq.el
ELS  += $(PKG)-notify.el
ELS  += $(PKG)-revnote.el
ELS  += $(PKG)-github.el
ELS  += $(PKG)-gitlab.el
ELS  += $(PKG)-gitea.el
ELS  += $(PKG)-gogs.el
ELS  += $(PKG)-bitbucket.el
ELS  += $(PKG)-semi.el
ELS  += $(PKG)-commands.el
ELS  += $(PKG)-list.el
ELCS  = $(ELS:.el=.elc)
PACKAGE_LISP = $(addprefix lisp/,$(ELS))

EMACS      ?= emacs
EMACS_ARGS ?=

CURL       ?= curl -fsSkL --retry 9 --retry-delay 9

EMAKE_SHA1 ?= e2db14acc21fdf9e0c76377c5165149562c1ed88

EMAKE_WORKDIR  ?= .emake
EMAKE_LOGLEVEL ?= INFO

EMAKE_ENV += PACKAGE_FILE="$(PKG)-pkg.el"
EMAKE_ENV += PACKAGE_LISP="$(PACKAGE_LISP)"
EMAKE_ENV += PACKAGE_ARCHIVES="melpa"
EMAKE_ENV += EMAKE_WORKDIR="$(EMAKE_WORKDIR)"
EMAKE_ENV += EMAKE_LOGLEVEL="$(EMAKE_LOGLEVEL)"

EMAKE_BASE ?= $(EMAKE_ENV) $(EMACS) -Q $(EMACS_ARGS) \
	-L '$(abspath lisp)' \
	-l '$(EMAKE_WORKDIR)/emake.el' \
	--eval '(setq enable-dir-local-variables nil)'
EMAKE ?= $(EMAKE_BASE) --eval "(emake (pop argv))"

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../dash
ORG_LOAD_PATH += -L ../../org/lisp
ORG_LOAD_PATH += -L ../../org/contrib/lisp
ORG_LOAD_PATH += -L ../../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css
