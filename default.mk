TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = forge

ELS  += $(PKG)-db.el
ELS  += $(PKG)-core.el
ELS  += $(PKG).el
ELS  += $(PKG)-repo.el
ELS  += $(PKG)-post.el
ELS  += $(PKG)-topic.el
ELS  += $(PKG)-discussion.el
ELS  += $(PKG)-issue.el
ELS  += $(PKG)-pullreq.el
ELS  += $(PKG)-revnote.el
ELS  += $(PKG)-notify.el
ELS  += $(PKG)-client.el
ELS  += $(PKG)-github.el
ELS  += $(PKG)-gitlab.el
ELS  += $(PKG)-forgejo.el
ELS  += $(PKG)-gitea.el
ELS  += $(PKG)-gogs.el
ELS  += $(PKG)-bitbucket.el
ELS  += $(PKG)-semi.el
ELS  += $(PKG)-commands.el
ELS  += $(PKG)-tablist.el
ELS  += $(PKG)-topics.el
ELS  += $(PKG)-repos.el
ELCS  = $(ELS:.el=.elc)

DEPS  = closql
DEPS += compat
DEPS += cond-let
DEPS += emacsql
DEPS += ghub/lisp
DEPS += llama
DEPS += magit/lisp
DEPS += markdown-mode
DEPS += seq
DEPS += transient/lisp
DEPS += treepy
DEPS += with-editor/lisp
DEPS += yaml
# Optional
DEPS += sqlite3
DEPS += vertico

DOMAIN      ?= magit.vc
CFRONT_DIST ?= E2LUHBKU1FBV02

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)
REVDESC := $(shell test -e $(TOP).git && git describe --tags)

EMACS      ?= emacs
EMACS_ARGS ?= --eval "(progn \
  (put 'if-let 'byte-obsolete-info nil) \
  (put 'when-let 'byte-obsolete-info nil))"

LOAD_PATH  ?= $(addprefix -L ../../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH += -L ../../org/lisp
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

GITSTATS      ?= gitstats
GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css -c max_authors=999
