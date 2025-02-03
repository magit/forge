((emacs-lisp-mode
  (indent-tabs-mode . nil)
  (read-symbol-shorthands
   ("partial" . "llama--left-apply-partially")
   ("rpartial" . "llama--right-apply-partially")))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode))
 (makefile-gmake-mode
  (outline-regexp . "##"))
 ("CHANGELOG"
  (nil
   (fill-column . 80)
   (mode . display-fill-column-indicator)))
 )
