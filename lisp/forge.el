;;; forge.el --- Access Git forges from Magit     -*- lexical-binding: t -*-

;; Copyright (C) 2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

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

(magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
(magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)

(define-key magit-mode-map "'" 'forge-dispatch)

(define-key magit-commit-section-map [remap magit-browse-thing] 'forge-browse-dwim)
(define-key magit-remote-section-map [remap magit-browse-thing] 'forge-browse-remote)
(define-key magit-branch-section-map [remap magit-browse-thing] 'forge-browse-branch)

(define-key magit-commit-section-map (kbd "C-c C-v") 'forge-visit-topic)
(define-key magit-branch-section-map (kbd "C-c C-v") 'forge-visit-topic)

(require 'magit-fetch)

(when (boundp 'magit-fetch-popup)
  (magit-define-popup-action 'magit-pull-and-fetch-popup
    ?Y "forge notifications" 'forge-pull-notifications)
  (magit-define-popup-action 'magit-pull-popup
    ?Y "forge notifications" 'forge-pull-notifications)

  (magit-define-popup-action 'magit-pull-and-fetch-popup
    ?y "forge topics" 'forge-pull)
  (magit-define-popup-action 'magit-pull-popup
    ?y "forge fopics" 'forge-pull)

  (magit-define-popup-action 'magit-branch-popup
    ?y "Checkout pull-request" 'forge-checkout-pullreq)

  (magit-define-popup-action 'magit-branch-popup
    ?Y "Create from pull-request" 'forge-branch-pullreq)

  (magit-define-popup-action 'magit-worktree-popup
    ?p "Create new worktree from pull-request" 'forge-checkout-worktree ?c)
  )

;;; forge.el ends here
