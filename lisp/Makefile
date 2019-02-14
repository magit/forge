-include ../config.mk
include ../default.mk

lisp: loaddefs $(ELS)
	@cd .. && $(MAKE) -f emake.mk compile

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

loaddefs: $(PKG)-autoloads.el
$(PKG)-autoloads.el: $(ELS)
	@cd .. && $(MAKE) -f emake.mk loaddefs
