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
   (assignees            :closql-table (issue-assignee assignee))
   (project-cards) ; projectsCards
   (edits) ; userContentEdits
   (labels               :closql-table (issue-label label))
   (participants)
   (posts                :closql-class forge-issue-post)
   (reactions)
   (timeline)
   (marks                :closql-table (issue-mark mark))
   (note                 :initarg :note :initform nil)
   (their-id             :initarg :their-id)
   (slug                 :initarg :slug)
   (saved-p              :initarg :saved-p :initform nil)
   ))

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
      (and (derived-mode-p 'forge-topic-list-mode)
           (and-let* ((id (tabulated-list-get-id))
                      (topic (forge-get-topic id)))
             (and (forge-issue-p topic)
                  topic)))
      (and demand (user-error "No issue at point"))))

(put 'forge-issue 'thing-at-point #'forge-thingatpt--issue)
(defun forge-thingatpt--issue ()
  (and-let* ((repo (forge--repo-for-thingatpt)))
    (and (thing-at-point-looking-at "#\\([0-9]+\\)\\_>")
         (forge-get-issue repo (string-to-number (match-string 1))))))

;;;; List

(defun forge--ls-recent-issues (repo)
  (forge-ls-recent-topics repo 'issue))

(defun forge--ls-issues (repo)
  (forge--select-issues repo
    [:from issue :where (= issue:repository $s1)]))

(defun forge--ls-open-issues (repo)
  (forge--select-issues repo
    [:from issue
     :where (and (= issue:repository $s1)
                 (= issue:state 'open))]))

(defun forge--ls-active-issues (repo)
  (forge--select-issues repo
    [:from issue
     :where (and (= issue:repository $s1)
                 (or (= issue:state 'open)
                     (in issue:status [pending unread])))]))

(defun forge--ls-assigned-issues (repo)
  (forge--select-issues repo
    [:from issue
     :join issue_assignee :on (= issue_assignee:issue issue:id)
     :join assignee       :on (= issue_assignee:id    assignee:id)
     :where (and (= issue:repository $s1)
                 (= assignee:login   $s2)
                 (isnull issue:closed))]
    (ghub--username repo)))

(defun forge--ls-authored-issues (repo)
  (forge--select-issues repo
    [:from [issue]
     :where (and (= issue:repository $s1)
                 (= issue:author     $s2)
                 (isnull issue:closed))]
    (ghub--username repo)))

(defun forge--ls-labeled-issues (repo label)
  (forge--select-issues repo
    [:from issue
     :join issue_label :on (= issue_label:issue issue:id)
     :join label       :on (= issue_label:id    label:id)
     :where (and (= issue:repository $s1)
                 (= label:name       $s2)
                 (isnull issue:closed))]
    label))

(defun forge--ls-owned-issues ()
  (forge--select-issues nil
    [:from [issue repository]
     :where (and (= issue:repository repository:id)
                 (in repository:owner $v1)
                 (not (in repository:name $v2))
                 (isnull issue:closed))
     :order-by [(asc repository:owner)
                (asc repository:name)
                (desc issue:number)]]
    (vconcat (mapcar #'car forge-owned-accounts))
    (vconcat forge-owned-ignored)))

(defun forge--select-issues (repo query &rest args)
  (declare (indent 1))
  (mapcar (let ((db (forge-db)))
            (lambda (row)
              (closql--remake-instance 'forge-issue db row)))
          (apply #'forge-sql
                 (vconcat [:select *]
                          query
                          (and (not (cl-find :order-by query))
                               [:order-by [(desc updated)]]))
                 (if repo
                     (cons (oref repo id) args)
                   args))))

;;; Read

(defun forge-read-issue (prompt)
  "Read an active issue with completion using PROMPT.

Open, unread and pending issues are considered active.
Default to the current issue even if it isn't active.

\\<forge-read-topic-minibuffer-map>While completion is in \
progress, \\[forge-read-topic-lift-limit] lifts the limit, extending
the completion candidates to include all issues.

If `forge-limit-topic-choices' is nil, then all candidates
can be selected from the start."
  (forge--read-topic prompt
                     #'forge-current-issue
                     #'forge--ls-active-issues
                     #'forge--ls-issues))

(defun forge-read-open-issue (prompt)
  "Read an open issue with completion using PROMPT."
  (let* ((current (forge-current-issue))
         (repo    (forge-get-repository (or current :tracked)))
         (default (and current (forge--format-topic-line current)))
         (alist   (forge--topic-collection (forge--ls-open-issues repo)))
         (choices (mapcar #'car alist))
         (choice  (magit-completing-read prompt choices nil t nil nil default)))
    (cdr (assoc choice alist))))

;;; Insert

(defvar-keymap forge-issues-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-issues
  "<remap> <magit-visit-thing>"  #'forge-list-issues
  "C-c C-m"                      #'forge-topics-menu
  "C-c C-n"                      #'forge-create-issue)

(defvar-keymap forge-issue-section-map
  "<remap> <magit-visit-thing>"  #'forge-visit-this-topic
  "C-c C-m"                      #'forge-topic-menu)

(defun forge-insert-issues ()
  "Insert a list of mostly recent and/or open issues.
Also see option `forge-topic-list-limit'."
  (forge--insert-issues "Issues" #'forge--ls-recent-issues))

(defun forge-insert-assigned-issues ()
  "Insert a list of open issues that are assigned to you."
  (forge--insert-issues "Assigned issues" #'forge--ls-assigned-issues))

(defun forge-insert-authored-issues ()
  "Insert a list of open issues that are authored by you."
  (forge--insert-issues "Authored issues" #'forge--ls-authored-issues))

(defun forge--insert-issues (heading getter)
  (when-let ((repo (forge--assert-insert-topics-get-repository t)))
    (forge--insert-topics 'issues heading (funcall getter repo))))

;;; _
(provide 'forge-issue)
;;; forge-issue.el ends here
