;;; forge-issue.el --- Issue support               -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Jonas Bernoulli

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

(require 'forge)
(require 'forge-post)
(require 'forge-topic)

;;; Classes

(defclass forge-issue (forge-topic)
  ((closql-table         :initform issue)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform repository)
   (closql-foreign-table :initform repository)
   (closql-class-prefix  :initform "forge-")
   (id                   :initarg :id)
   (repository           :initarg :repository)
   (number               :initarg :number)
   (state                :initarg :state)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (unread-p             :initarg :unread-p :initform nil)
   (locked-p             :initarg :locked-p)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees            :closql-table (issue-assignee assignee))
   (project-cards) ; projectsCards
   (edits) ; userContentEdits
   (labels               :closql-table (issue-label label))
   (participants)
   (posts                :closql-class forge-issue-post)
   (reactions)
   (timeline)
   ))

(defclass forge-issue-post (forge-post)
  ((closql-table         :initform issue-post)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform issue)
   (closql-foreign-table :initform issue)
   (closql-class-prefix  :initform "forge-issue-")
   (id                   :initarg :id)
   (issue                :initarg :issue)
   (number               :initarg :number)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ))

;;; Query

(cl-defmethod forge-get-topic ((post forge-issue-post))
  (forge-get-issue post))

(cl-defmethod forge-get-issue ((repo forge-repository) number)
  (closql-get (forge-db)
              (forge--object-id 'forge-issue repo number)
              'forge-issue))

(cl-defmethod forge-get-issue ((number integer))
  (when-let ((repo (forge-get-repository t)))
    (forge-get-issue repo number)))

(cl-defmethod forge-get-issue ((post forge-issue-post))
  (closql-get (forge-db)
              (oref post issue)
              'forge-issue))

(cl-defmethod forge-ls-issues ((repo forge-repository) &optional type)
  (forge-ls-topics repo 'forge-issue type))

;;; Utilities

(defun forge-read-issue (prompt &optional type)
  (when (eq type t)
    (setq type (if current-prefix-arg nil 'open)))
  (let* ((default (forge-issue-at-point))
         (repo    (forge-get-repository (or default t)))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (forge-ls-issues repo type))
         (choice  (magit-completing-read
                   prompt
                   (mapcar format choices)
                   nil nil nil nil
                   (and default (funcall format default))))
         (number  (and (string-match "\\([0-9]+\\)" choice)
                       (string-to-number (match-string 1 choice)))))
    (and number
         (forge-get-issue repo number))))

(cl-defmethod forge-get-url ((issue forge-issue))
  (forge--format issue 'issue-url-format))

;;; Sections

(defun forge-issue-at-point ()
  (or (magit-section-value-if 'issue)
      (when-let ((post (magit-section-value-if 'post)))
        (if (forge-issue-p post)
            post
          (forge-get-issue post)))))

(defvar forge-issues-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-issues)
    (define-key map [remap magit-visit-thing]  'forge-list-issues)
    (define-key map (kbd "C-c C-n")            'forge-create-issue)
    map))

(defvar forge-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-issue)
    (define-key map [remap magit-visit-thing]  'forge-visit-issue)
    map))

(defun forge-insert-issues ()
  (when-let ((repo (forge-get-repository nil)))
    (when (and (not (oref repo sparse-p))
               (or (not (slot-boundp repo 'issues-p)) ; temporary KLUDGE
                   (oref repo issues-p)))
      (forge-insert-topics "Issues"
                           (forge-ls-recent-topics repo 'issue)
                           (forge--topic-type-prefix repo 'issue)))))

;;; _
(provide 'forge-issue)
;;; forge-issue.el ends here
