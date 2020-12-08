;;; forge-bitbucket.el --- Bitbucket support      -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;;; Code:

(require 'buck)
(require 'forge)

;;; Class

(defclass forge-bitbucket-repository (forge-noapi-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   ;; The anchor for the issue itself is .../%i#issue-%i
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#comment-%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/pull-requests")
   (pullreq-url-format        :initform "https://%h/%o/%n/pull-requests/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/pull-requests/%i#comment-%I")
   (commit-url-format         :initform "https://%h/%o/%n/commits/%r")
   (branch-url-format         :initform "https://%h/%o/%n/branch/%r")
   (file-url-format           :initform "https://%h/%o/%n/src/%r/%f#lines-%l")
   (remote-url-format         :initform "https://%h/%o/%n/src")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/pull-requests/new")))

;;; _

(cl-defmethod forge--file-url ((repo forge-bitbucket-repository) rev file start end)
  (let* ((start-fragment (number-to-string start))
         (location-fragment (if end (concat start-fragment ":" (number-to-string end)) start-fragment)))
    (forge--format repo 'file-url-format
                   `((?r . ,rev) (?f . ,file) (?l . ,location-fragment)))))

(provide 'forge-bitbucket)
;;; forge-bitbucket.el ends here
