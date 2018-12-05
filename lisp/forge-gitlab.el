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

(defclass forge-gitlab-repository (forge-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#note_%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/merge_requests")
   (pullreq-url-format        :initform "https://%h/%o/%n/merge_requests/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/merge_requests/%i#note_%I")
   (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format         :initform "https://%h/%o/%n/commits/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/merge_requests/new")
   (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))

;;; _
(provide 'forge-gitlab)
;;; forge-gitlab.el ends here
