;;; forge-pullreq.el --- Pullreq support  -*- lexical-binding:t -*-

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

(defclass forge-pullreq (forge-topic)
  ((closql-table         :initform 'pullreq)
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
   (merged               :initarg :merged)
   (status               :initarg :status :initform nil)
   (locked-p             :initarg :locked-p)
   (editable-p           :initarg :editable-p)
   (cross-repo-p         :initarg :cross-repo-p)
   (base-ref             :initarg :base-ref)
   (base-repo            :initarg :base-repo)
   (head-ref             :initarg :head-ref)
   (head-user            :initarg :head-user)
   (head-repo            :initarg :head-repo)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees            :closql-tables (pullreq-assignee assignee))
   (project-cards) ; projectsCards
   (commits)
   (edits) ; userContentEdits
   (labels               :closql-tables (pullreq-label label))
   (participants)
   (posts                :closql-class forge-pullreq-post)
   (reactions)
   (review-requests      :closql-tables (pullreq-review-request assignee))
   (reviews)
   (timeline)
   (marks                :closql-tables (pullreq-mark mark))
   (note                 :initarg :note :initform nil)
   (base-rev             :initarg :base-rev)
   (head-rev             :initarg :head-rev)
   (draft-p              :initarg :draft-p)
   (their-id             :initarg :their-id)
   (slug                 :initarg :slug)
   (saved-p              :initarg :saved-p :initform nil)
   ))

(cl-defmethod closql-dref ((obj forge-pullreq) (_(eql assignees)))
  (forge-sql-cdr
   [:select assignee:* :from assignee
    :join pullreq-assignee :on (= pullreq-assignee:id assignee:id)
    :where (= pullreq-assignee:pullreq $s1)
    :order-by [(asc login)]]
   (closql--oref obj 'id)))

(cl-defmethod closql-dref ((obj forge-pullreq) (_(eql labels)))
  (forge-sql-cdr
   [:select label:* :from label
    :join pullreq-label :on (= pullreq-label:id label:id)
    :where (= pullreq-label:pullreq $s1)
    :order-by [(asc name)]]
   (closql--oref obj 'id)))

(cl-defmethod closql-dref ((obj forge-pullreq) (_(eql review-requests)))
  (forge-sql-cdr
   [:select assignee:* :from assignee
    :join pullreq-review-request :on (= pullreq-review-request:id assignee:id)
    :where (= pullreq-review-request:pullreq $s1)
    :order-by [(asc login)]]
   (closql--oref obj 'id)))

(cl-defmethod closql-dref ((obj forge-pullreq) (_(eql marks)))
  (forge-sql-cdr
   [:select mark:* :from mark
    :join pullreq-mark :on (= pullreq-mark:id mark:id)
    :where (= pullreq-mark:pullreq $s1)
    :order-by [(asc name)]]
   (closql--oref obj 'id)))

(defclass forge-pullreq-post (forge-post)
  ((closql-table         :initform 'pullreq-post)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform 'pullreq)
   (closql-class-prefix  :initform "forge-pullreq-")
   (id                   :initarg :id)
   (pullreq              :initarg :pullreq)
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

(cl-defmethod forge-get-repository ((post forge-pullreq-post))
  (forge-get-repository (forge-get-pullreq post)))

(cl-defmethod forge-get-topic ((post forge-pullreq-post))
  (forge-get-pullreq post))

(cl-defmethod forge-get-pullreq ((pullreq forge-pullreq))
  pullreq)

(cl-defmethod forge-get-pullreq ((repo forge-repository) number)
  (closql-get (forge-db)
              (forge--object-id 'forge-pullreq repo number)
              'forge-pullreq))

(cl-defmethod forge-get-pullreq ((number integer))
  (and-let* ((repo (forge-get-repository :tracked)))
    (forge-get-pullreq repo number)))

(cl-defmethod forge-get-pullreq ((id string))
  (closql-get (forge-db) id 'forge-pullreq))

(cl-defmethod forge-get-pullreq ((post forge-pullreq-post))
  (closql-get (forge-db)
              (oref post pullreq)
              'forge-pullreq))

(cl-defmethod forge-get-pullreq ((_(eql :branch)) &optional branch)
  (and-let* ((branch (or branch
                         (magit-section-case
                           (branch (oref it value))
                           (commit (magit--painted-branch-at-point)))))
             (branch (cdr (magit-split-branch-name branch)))
             (number (magit-get "branch" branch "pullRequest")))
    (forge-get-pullreq (string-to-number number))))

;;;; Current

(defun forge-current-pullreq (&optional demand)
  "Return the pull-request at point or being visited.
If there is no such pull-request and DEMAND is non-nil, then signal
an error."
  (or (forge-pullreq-at-point)
      (and (forge-pullreq-p forge-buffer-topic)
           forge-buffer-topic)
      (and demand (user-error "No current pull-request"))))

(defun forge-pullreq-at-point (&optional demand)
  "Return the pull-request at point.
If there is no such pull-request and DEMAND is non-nil, then signal
an error."
  (or (thing-at-point 'forge-pullreq)
      (magit-section-value-if 'pullreq)
      (forge-get-pullreq :branch)
      (and demand (user-error "No pull-request at point"))))

(put 'forge-pullreq 'thing-at-point #'forge-thingatpt--pullreq)
(defun forge-thingatpt--pullreq ()
  (and-let* ((repo (forge--repo-for-thingatpt)))
    (and (thing-at-point-looking-at
          (if (forge-gitlab-repository--eieio-childp repo)
              "[#!]\\([0-9]+\\)\\_>"
            "#\\([0-9]+\\)\\_>"))
         (forge-get-pullreq repo (string-to-number (match-string 1))))))

;;; Read

(defun forge-read-pullreq (prompt)
  "Read an active pull-request with completion using PROMPT.

Open, unread and pending pull-requests are considered active.
Default to the current pull-request, even if it isn't active.

\\<forge-read-topic-minibuffer-map>While completion is in \
progress, \\[forge-read-topic-lift-limit] lifts the limit, extending
the completion candidates to include all pull-requests.

If `forge-limit-topic-choices' is nil, then all candidates
can be selected from the start."
  (forge--read-topic prompt
                     #'forge-current-pullreq
                     (forge--topics-spec :type 'pullreq :active t)
                     (forge--topics-spec :type 'pullreq :active nil :state nil)))

;;; Utilities

(defun forge--pullreq-branch-internal (pullreq)
  (let ((branch (oref pullreq head-ref)))
    ;; It is invalid for a branch name to begin with a colon, yet
    ;; that is what Gitlab uses when a pull-request's source branch
    ;; has been deleted.  On Github this is simply nil in the same
    ;; situation.
    (and branch (not (string-prefix-p ":" branch)) branch)))

(defun forge--pullreq-branch-active (pullreq)
  (let* ((number (number-to-string (oref pullreq number)))
         (branch-n (format "pr-%s" number))
         (branch (forge--pullreq-branch-internal pullreq)))
    (or (and (magit-branch-p branch)
             (equal (magit-get "branch" branch "pullRequest") number)
             branch)
        (and (magit-branch-p branch-n)
             (equal (magit-get "branch" branch-n "pullRequest") number)
             branch-n))))

(defun forge--pullreq-ref (pullreq)
  (let ((ref (format "refs/pullreqs/%s" (oref pullreq number))))
    (and (magit-rev-verify ref) ref)))

(defun forge--pullreq-range (pullreq &optional endpoints)
  (and-let* ((head (forge--pullreq-ref pullreq)))
    (concat (forge--get-remote) "/" (oref pullreq base-ref)
            (if endpoints "..." "..")
            head)))

;;; Insert

(defvar-keymap forge-pullreqs-section-map
  :parent forge-common-map
  "<remap> <magit-browse-thing>" #'forge-browse-pullreqs
  "<remap> <magit-visit-thing>"  #'forge-list-pullreqs
  "<remap> <forge--list-menu>"   #'forge-topics-menu
  "<remap> <forge--item-menu>"   #'forge-topic-menu
  "C-c C-n"                      #'forge-create-pullreq)

(defvar-keymap forge-pullreq-section-map
  :parent forge-common-map
  "<remap> <magit-visit-thing>"  #'forge-visit-this-topic
  "<remap> <forge--list-menu>"   #'forge-topics-menu
  "<remap> <forge--item-menu>"   #'forge-topic-menu)

(cl-defun forge-insert-pullreqs (&optional (spec nil sspec) heading)
  "Insert a list of pull-requests, according to `forge--buffer-topics-spec'.
Optional SPEC can be used to override that filtering specification,
and optional HEADING to change the section heading."
  (when-let (((forge-db t))
             (repo (forge-get-repository :tracked?))
             (spec (if sspec spec (forge--clone-buffer-topics-spec)))
             ((memq (oref spec type) '(topic pullreq))))
    (oset spec type 'pullreq)
    (forge--insert-topics 'pullreqs
                          (or heading "Pull requests")
                          (forge--list-topics spec repo))))

(defun forge--insert-pullreq-commits (pullreq &optional all)
  (cl-letf (((symbol-function #'magit-cancel-section) (lambda ())))
    (if all
        ;; Numeric pr ref, pr branch (if it exists) and api
        ;; pr range may be out of sync.  Just show them all.
        (magit-insert-section-body
          (magit--insert-log nil
            (delq nil (list (concat "^" (or (oref pullreq base-rev)
                                            (concat (forge--get-remote) "/"
                                                    (oref pullreq base-ref))))
                            (forge--pullreq-ref pullreq)
                            (forge--pullreq-branch-active pullreq)
                            (and-let* ((branch (oref pullreq head-ref)))
                              (and (magit-local-branch-p branch) branch))))
            (seq-uniq (cons "--graph" magit-buffer-log-args))))
      (when-let ((range (forge--pullreq-range pullreq)))
        (magit-insert-section-body
          (magit--insert-log nil range magit-buffer-log-args)
          (magit-make-margin-overlay nil t))))))

;;; _
(provide 'forge-pullreq)
;;; forge-pullreq.el ends here
