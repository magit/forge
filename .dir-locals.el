((emacs-lisp-mode
  (indent-tabs-mode . nil))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode))
 (makefile-gmake-mode
  (outline-regexp . "##"))
 ("CHANGELOG"
  (nil
   (fill-column . 80)
   (mode . display-fill-column-indicator)))
 )
