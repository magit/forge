;;; forge-issue.el --- Issue support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'forge)
(require 'forge-post)
(require 'forge-topic)

;;; Classes

(defclass forge-issue (forge-topic)
  ((closql-table         :initform 'issue)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform 'repository)
   (closql-class-prefix  :initform "forge-")
   (id                   :initarg :id)
   (repository           :initarg :repository)
   (number               :initarg :number)
   (state                :initarg :state)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated :initform nil)
   (closed               :initarg :closed)
   (status               :initarg :status :initform nil)
   (locked-p             :initarg :locked-p)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees            :closql-tables (issue-assignee assignee))
   (project-cards) ; projectsCards
   (edits) ; userContentEdits
   (labels               :closql-tables (issue-label label))
   (participants)
   (posts                :closql-class forge-issue-post)
   (reactions)
   (timeline)
   (marks                :closql-tables (issue-mark mark))
   (note                 :initarg :note :initform nil)
   (their-id             :initarg :their-id)
   (slug                 :initarg :slug)
   (saved-p              :initarg :saved-p :initform nil)
   ))

(cl-defmethod closql-dref ((obj forge-issue) (_(eql assignees)))
  (forge-sql-cdr
   [:select assignee:* :from assignee
    :join issue-assignee :on (= issue-assignee:id assignee:id)
    :where (= issue-assignee:issue $s1)
    :order-by [(asc login)]]
   (closql--oref obj 'id)))

(cl-defmethod closql-dref ((obj forge-issue) (_(eql labels)))
  (forge-sql-cdr
   [:select label:* :from label
    :join issue-label :on (= issue-label:id label:id)
    :where (= issue-label:issue $s1)
    :order-by [(asc name)]]
   (closql--oref obj 'id)))

(cl-defmethod closql-dref ((obj forge-issue) (_(eql marks)))
  (forge-sql-cdr
   [:select mark:* :from mark
    :join issue-mark :on (= issue-mark:id mark:id)
    :where (= issue-mark:issue $s1)
    :order-by [(asc name)]]
   (closql--oref obj 'id)))

(defclass forge-issue-post (forge-post)
  ((closql-table         :initform 'issue-post)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform 'issue)
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
;;;; Get

(cl-defmethod forge-get-repository ((post forge-issue-post))
  (forge-get-repository (forge-get-issue post)))

(cl-defmethod forge-get-topic ((post forge-issue-post))
  (forge-get-issue post))

(cl-defmethod forge-get-issue ((issue forge-issue))
  issue)

(cl-defmethod forge-get-issue ((repo forge-repository) number)
  (closql-get (forge-db)
              (forge--object-id 'forge-issue repo number)
              'forge-issue))

(cl-defmethod forge-get-issue ((number integer))
  (and-let* ((repo (forge-get-repository :tracked)))
    (forge-get-issue repo number)))

(cl-defmethod forge-get-issue ((id string))
  (closql-get (forge-db) id 'forge-issue))

(cl-defmethod forge-get-issue ((post forge-issue-post))
  (closql-get (forge-db)
              (oref post issue)
              'forge-issue))

;;;; Current

(defun forge-current-issue (&optional demand)
  "Return the issue at point or being visited.
If there is no such issue and DEMAND is non-nil, then signal
an error."
  (or (forge-issue-at-point)
      (and (forge-issue-p forge-buffer-topic)
           forge-buffer-topic)
      (and demand (user-error "No current issue"))))

(defun forge-issue-at-point (&optional demand)
  "Return the issue at point.
If there is no such issue and DEMAND is non-nil, then signal
an error."
  (or (thing-at-point 'forge-issue)
      (magit-section-value-if 'issue)
      (and demand (user-error "No issue at point"))))

(put 'forge-issue 'thing-at-point #'forge-thingatpt--issue)
(defun forge-thingatpt--issue ()
  (and-let* ((repo (forge--repo-for-thingatpt)))
    (and (thing-at-point-looking-at "#\\([0-9]+\\)\\_>")
         (forge-get-issue repo (string-to-number (match-string 1))))))

;;; Read

(defun forge-read-issue (prompt)
  "Read an active issue with completion using PROMPT.

Open, unread and pending issues are considered active.
Default to the current issue, even if it isn't active.

\\<forge-read-topic-minibuffer-map>While completion is in \
progress, \\[forge-read-topic-lift-limit] lifts the limit, extending
the completion candidates to include all issues.

If `forge-limit-topic-choices' is nil, then all candidates
can be selected from the start."
  (forge--read-topic prompt
                     #'forge-current-issue
                     (forge--topics-spec :type 'issue :active t)
                     (forge--topics-spec :type 'issue :active nil :state nil)))

(defun forge-read-open-issue (prompt)
  "Read an open issue with completion using PROMPT."
  (let* ((current (forge-current-issue))
         (repo    (forge-get-repository (or current :tracked)))
         (default (and current (forge--format-topic-line current)))
         (alist   (forge--topic-collection
                   (forge--list-topics
                    (forge--topics-spec :type 'issue :state 'open)
                    repo)))
         (choices (mapcar #'car alist))
         (choice  (magit-completing-read prompt choices nil t nil nil default)))
    (cdr (assoc choice alist))))

;;; Insert

(defvar-keymap forge-issues-section-map
  :parent forge-common-map
  "<remap> <magit-browse-thing>" #'forge-browse-issues
  "<remap> <magit-visit-thing>"  #'forge-list-issues
  "<remap> <forge--list-menu>"   #'forge-topics-menu
  "<remap> <forge--item-menu>"   #'forge-topic-menu
  "C-c C-n"                      #'forge-create-issue)

(defvar-keymap forge-issue-section-map
  :parent forge-common-map
  "<remap> <magit-visit-thing>"  #'forge-visit-this-topic
  "<remap> <forge--list-menu>"   #'forge-topics-menu
  "<remap> <forge--item-menu>"   #'forge-topic-menu)

(cl-defun forge-insert-issues (&optional (spec nil sspec) heading)
  "Insert a list of issues, according to `forge--buffer-topics-spec'.
Optional SPEC can be used to override that filtering specification,
and optional HEADING to change the section heading."
  (when-let (((forge-db t))
             (repo (forge-get-repository :tracked?))
             ((oref repo issues-p))
             (spec (if sspec spec (forge--clone-buffer-topics-spec)))
             ((memq (oref spec type) '(topic issue))))
    (oset spec type 'issue)
    (forge--insert-topics 'issues
                          (or heading "Issues")
                          (forge--list-topics spec repo))))

;;; _
(provide 'forge-issue)
;;; forge-issue.el ends here
