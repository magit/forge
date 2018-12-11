;;; forge-semi.el --- Support for semi-forges     -*- lexical-binding: t -*-

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

(require 'forge)

(defclass forge-gitweb-repository (forge-noapi-repository)
  ((commit-url-format :initform "https://%h/gitweb/?p=%P.git;a=commitdiff;h=%r")
   (branch-url-format :initform "https://%h/gitweb/?p=%P.git;a=log;h=refs/heads/%r")
   (remote-url-format :initform "https://%h/gitweb/?p=%P.git;a=summary"))
  "Gitweb from https://git-scm.com/docs/gitweb.")

;;; _
(provide 'forge-semi)
;;; forge-semi.el ends here
