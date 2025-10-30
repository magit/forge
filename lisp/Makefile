-include ../config.mk
include ../default.mk

lisp: $(ELCS) autoloads check-declare

autoloads: $(PKG)-autoloads.el

$(PKG)-db.elc:
$(PKG)-core.elc:       $(PKG)-db.elc
$(PKG).elc:            $(PKG)-core.elc
$(PKG)-repo.elc:       $(PKG).elc
$(PKG)-post.elc:       $(PKG).elc
$(PKG)-topic.elc:      $(PKG)-post.elc
$(PKG)-discussion.elc: $(PKG)-topic.elc
$(PKG)-issue.elc:      $(PKG)-topic.elc
$(PKG)-pullreq.elc:    $(PKG)-topic.elc
$(PKG)-revnote.elc:    $(PKG)-topic.elc
$(PKG)-notify.elc:     $(PKG).elc
$(PKG)-client.elc:     $(PKG).elc
$(PKG)-github.elc:     $(PKG)-client.elc $(PKG)-discussion.elc \
                       $(PKG)-topic.elc $(PKG)-pullreq.elc
$(PKG)-gitlab.elc:     $(PKG)-client.elc \
                       $(PKG)-topic.elc $(PKG)-pullreq.elc
$(PKG)-forgejo.elc:    $(PKG).elc
$(PKG)-gitea.elc:      $(PKG).elc
$(PKG)-gogs.elc:       $(PKG).elc
$(PKG)-bitbucket.elc:  $(PKG).elc
$(PKG)-semi.elc:       $(PKG).elc
$(PKG)-commands.elc:   $(PKG).elc
$(PKG)-list.elc:       $(PKG).elc
$(PKG)-topics.elc:     $(PKG)-tablist.elc
$(PKG)-repos.elc:      $(PKG)-tablist.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	--eval "(check-declare-directory default-directory)"

CLEAN = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning lisp/*...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch --eval "\
(let ((inhibit-message t))\
  (loaddefs-generate\
   default-directory \"$@\" nil\
   (prin1-to-string\
    '(add-to-list 'load-path\
                  (or (and #$$ (directory-file-name (file-name-directory #$$)))\
                      (car load-path)))))\
   nil t)"
