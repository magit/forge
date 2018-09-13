;;; forge-revnote.el --- forge revnote support    -*- lexical-binding: t -*-

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
(require 'forge-post)
(require 'forge-topic)

;;; Class

(defclass forge-revnote (forge-topic)
  ((closql-table         :initform revnote)
   (closql-primary-key   :initform id)
   ;; (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform repository)
   (closql-foreign-table :initform repository)
   (closql-class-prefix  :initform "forge-")
   (id                   :initarg :id)
   (repository           :initarg :repository)
   (commit               :initarg :commit)
   (file                 :initarg :file)
   (line                 :initarg :line)
   (author               :initarg :author)
   (body                 :initarg :body)))

;;; _
(provide 'forge-revnote)
;;; forge-revnote.el ends here
