;;; forge.el --- Access Git forges from Magit     -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/forge
;; Keywords: git tools vc

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Work with Git forges, such as Github and Gitlab, from the comfort
;; of Magit and the rest of Emacs.

;; The schema of the database has not been finalized yet.  Until that
;; has happened it will occationally have to be discarded.  For now
;; the database does not contain any information that cannot simply
;; be fetched again.

;;; Code:

(require 'magit)

(require 'forge-db)
(require 'forge-core)

(provide 'forge)

(require 'forge-repo)
(require 'forge-post)
(require 'forge-topic)
(require 'forge-issue)
(require 'forge-pullreq)
(require 'forge-revnote)
(require 'forge-notify)

(require 'forge-github)
(require 'forge-gitlab)
(require 'forge-gitea)
(require 'forge-gogs)
(require 'forge-bitbucket)
(require 'forge-semi)

(require 'forge-commands)
(require 'forge-list)

;;; Add Sections

(magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
(magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)

;;; Add Bindings

(define-key magit-mode-map "'" 'forge-dispatch)

(define-key magit-commit-section-map [remap magit-browse-thing] 'forge-browse-dwim)
(define-key magit-remote-section-map [remap magit-browse-thing] 'forge-browse-remote)
(define-key magit-branch-section-map [remap magit-browse-thing] 'forge-browse-branch)

(define-key magit-commit-section-map (kbd "C-c C-v") 'forge-visit-topic)
(define-key magit-branch-section-map (kbd "C-c C-v") 'forge-visit-topic)

(transient-append-suffix 'magit-dispatch "%"
  '("'" "Forge" forge-dispatch ?%))

(transient-append-suffix 'magit-fetch "m"
  '("y" "forge topics" forge-pull))
(transient-append-suffix 'magit-fetch "y"
  '("Y" "forge notifications" forge-pull-notifications))

(transient-append-suffix 'magit-pull "m"
  '("y" "forge topics" forge-pull))
(transient-append-suffix 'magit-pull "y"
  '("Y" "forge notifications" forge-pull-notifications))

(transient-append-suffix 'magit-branch "w"
  '("y" "pull-request" forge-checkout-pullreq))
(transient-append-suffix 'magit-branch "W"
  '("Y" "from pull-request" forge-branch-pullreq))

(transient-append-suffix 'magit-worktree "c"
  '("y" "pull-request worktree" forge-checkout-worktree))

;;; forge.el ends here
