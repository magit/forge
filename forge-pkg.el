(define-package "forge" "0"
  "Access Git forges from Magit."
  '((emacs          "25.1")
    (closql         "1.0.0")
    (dash           "2.14.1")
    (emacsql-sqlite "3.0.0")
    ;; This comes with important bug fixes and a workaround for
    ;; an Emacs bug.
    (ghub           "20190319")
    (let-alist      "1.0.5")
    ;; Forge currently isn't compatible with any releases Magit
    ;; version.  When removing this kludge, then do not forget to
    ;; also remove the related kludge from Forge's Melpa recipe.
    (magit          "20190408")
    (markdown-mode  "2.3")
    (transient      "0.1.0"))
  :url "https://github.com/magit/forge")
