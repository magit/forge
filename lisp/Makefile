-include ../config.mk
include ../default.mk

lisp: $(ELCS) loaddefs

loaddefs: $(PKG)-autoloads.el

$(PKG)-db.elc:
$(PKG)-core.elc:      $(PKG)-db.elc
$(PKG).elc:           $(PKG)-core.elc
$(PKG)-repo.elc:      $(PKG).elc
$(PKG)-post.elc:      $(PKG).elc
$(PKG)-topic.elc:     $(PKG)-post.elc
$(PKG)-issue.elc:     $(PKG)-topic.elc
$(PKG)-pullreq.elc:   $(PKG)-topic.elc
$(PKG)-revnote.elc:   $(PKG)-topic.elc
$(PKG)-notify.elc:    $(PKG).elc
$(PKG)-github.elc:    $(PKG)-topic.elc $(PKG)-pullreq.elc
$(PKG)-gitlab.elc:    $(PKG)-topic.elc $(PKG)-pullreq.elc
$(PKG)-gitea.elc:     $(PKG).elc
$(PKG)-gogs.elc:      $(PKG).elc
$(PKG)-bitbucket.elc: $(PKG).elc
$(PKG)-semi.elc:      $(PKG).elc
$(PKG)-commands.elc:  $(PKG).elc
$(PKG)-list.elc:      $(PKG).elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

CLEAN = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning lisp/*...\n"
	@rm -rf $(CLEAN)

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"
