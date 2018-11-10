;;; forge-gitlab.el --- Gitlab support            -*- lexical-binding: t -*-

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

;;; Code:

(require 'glab)

(require 'forge)
(require 'forge-issue)
(require 'forge-pullreq)

;;; Variables

(defvar forge-gitlab-token-scopes '(api)
  "The Gitlab API scopes needed by Magit.

`api' is the only required scope.  It gives read and write access
to everything.  The Gitlab API provides more fine-grained scopes
for read-only access, but when any write access is required, then
it is all or nothing.")

;;; Class

;; The `forge-gitlab-repository' class is defined in ghub-repo.el.

;;; _
(provide 'forge-gitlab)
;;; forge-gitlab.el ends here
