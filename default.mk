TOP := $(dir $(lastword $(MAKEFILE_LIST)))

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

DEPS  = closql
DEPS += dash
DEPS += emacsql
DEPS += ghub
DEPS += magit/lisp
DEPS += markdown-mode
DEPS += transient/lisp
DEPS += treepy
DEPS += with-editor
DEPS += yaml
# Optional
DEPS += emacsql-libsqlite3
DEPS += sqlite3

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../dash
ORG_LOAD_PATH += -L ../../org/lisp
ORG_LOAD_PATH += -L ../../org-contrib/lisp
ORG_LOAD_PATH += -L ../../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

STATS_DIR ?= $(TOP)docs/stats
