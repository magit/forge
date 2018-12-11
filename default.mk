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
ELS  += $(PKG)-commands.el
ELS  += $(PKG)-list.el
ELCS  = $(ELS:.el=.elc)

DEPS  = closql
DEPS += dash
DEPS += hydra # for lv.el
DEPS += emacsql
DEPS += ghub
DEPS += graphql
DEPS += magit/lisp
DEPS += magit-popup
DEPS += markdown-mode
DEPS += transient/lisp
DEPS += treepy
DEPS += with-editor

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../../,$(DEPS))
LOAD_PATH  += -L .
