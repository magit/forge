;;; forge-pullreq.el --- Pullreq support          -*- lexical-binding: t -*-

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

;;; Classes

(defclass forge-pullreq (forge-topic)
  ((closql-table         :initform pullreq)
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
   (merged               :initarg :merged)
   (unread-p             :initarg :unread-p :initform nil)
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
   (assignees            :closql-table (pullreq-assignee assignee))
   (project-cards) ; projectsCards
   (commits)
   (edits) ; userContentEdits
   (labels               :closql-table (pullreq-label label))
   (participants)
   (posts                :closql-class forge-pullreq-post)
   (reactions)
   (review-requests)
   (reviews)
   (timeline)
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; activeLockReason, additions, authorAssociation, (baseRefName), baseRefOid,
   ;; bodyHTML, bodyText, canBeRebased, changedFiles, closed, createdViaEmail,
   ;; databaseId, deletions, editor, (headRefName), headRefOid, mergeCommit,
   ;; mergeStateStatus, mergeable, merged, mergedBy, permalink,
   ;; potentialMergeCommit,, reactionGroups, resourcePath, revertResourcePath,
   ;; revertUrl, url, viewer{*}
   ))

(defclass forge-pullreq-post (forge-post)
  ((closql-table         :initform pullreq-post)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform pullreq)
   (closql-foreign-table :initform pullreq)
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
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; pullRequest (same as issue),
   ;; repository (use .pullreq.project),
   ;; authorAssociation, bodyHTML, bodyText, createdViaEmail,
   ;; editor, id, reactionGroups, resourcePath, url, viewer{*}
   ))

;;; Query

(cl-defmethod forge-get-topic ((post forge-pullreq-post))
  (forge-get-pullreq post))

(cl-defmethod forge-get-pullreq ((repo forge-repository) number)
 (closql-get (forge-db)
             (forge--object-id 'forge-pullreq repo number)
             'forge-pullreq))

(cl-defmethod forge-get-pullreq ((number integer))
  (when-let ((repo (forge-get-repository t)))
    (forge-get-pullreq repo number)))

(cl-defmethod forge-get-pullreq ((post forge-pullreq-post))
  (closql-get (forge-db)
              (oref post pullreq)
              'forge-pullreq))

(cl-defmethod forge-ls-pullreqs ((repo forge-repository))
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-pullreq (forge-db) row))
          (forge-sql [:select * :from pullreq
                      :where (= repository $s1)
                      :order-by [(desc number)]]
                     (oref repo id))))

(cl-defmethod forge-get-repository ((post forge-pullreq-post))
  (forge-get-repository (forge-get-pullreq post)))

;;; Utilities

(defun forge-read-pullreq (prompt)
  (let* ((default (forge-pullreq-at-point))
         (repo    (forge-get-repository (or default t)))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (oref repo pullreqs))
         (choice  (magit-completing-read prompt
                                         (mapcar format choices)
                                         nil nil nil nil
                                         (and default
                                              (funcall format default))))
         (number  (and (string-match "\\([0-9]+\\)" choice)
                       (string-to-number (match-string 1 choice)))))
    (and number
         (forge-get-pullreq repo number))))

(defun forge-read-pullreq-or-number (prompt)
  (if (forge--childp (forge-get-repository t) 'forge-unusedapi-repository)
      (let* ((num (read-number (concat prompt ": ")))
             (ref (forge--pullreq-ref-1 num)))
        (unless (magit-ref-exists-p ref)
          (user-error "Reference `%s' doesn't exist.  Maybe pull first?" ref))
        num)
    (forge-read-pullreq prompt)))

(defun forge--pullreq-branch (pullreq &optional assert-new)
  (with-slots (head-ref number cross-repo-p editable-p) pullreq
    (let ((branch head-ref))
      (when (and cross-repo-p
                 (or (not editable-p)
                     (magit-branch-p branch)))
        (setq branch (format "pr-%s" number)))
      (when (and assert-new (magit-branch-p branch))
        (user-error "Branch `%s' already exists" branch))
      branch)))

(defun forge--pullreq-ref-1 (number)
  (let ((ref (format "refs/pullreqs/%s" number)))
    (and (magit-rev-verify ref) ref)))

(defun forge--pullreq-ref (pullreq)
  (let ((branch (forge--pullreq-branch pullreq)))
    (or (and branch (magit-rev-verify branch) branch)
        (forge--pullreq-ref-1 (oref pullreq number)))))

;;; Sections

(defun forge-pullreq-at-point ()
  (or (magit-section-value-if 'pullreq)
      (when-let ((post (magit-section-value-if 'post)))
        (if (forge-pullreq-p post)
            post
          (forge-get-pullreq post)))))

(defvar forge-pullreqs-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-pullreqs)
    (define-key map [remap magit-visit-thing]  'forge-list-pullreqs)
    (define-key map (kbd "C-c C-n")            'forge-create-pullreq)
    map))

(defvar forge-pullreq-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-pullreq)
    (define-key map [remap magit-visit-thing]  'forge-visit-pullreq)
    map))

(defun forge-insert-pullreqs ()
  (when-let ((repo (forge-get-repository nil))
             (- (not (oref repo sparse-p)))
             (pullreqs (forge-list-recent-topics repo 'pullreq)))
    (magit-insert-section (pullreqs nil t)
      (magit-insert-heading
        (format "%s (%s)"
                (propertize "Pull requests" 'face 'magit-section-heading)
                (length pullreqs)))
      (magit-insert-section-body
        (let ((width (length (number-to-string (oref (car pullreqs) number))))
              (prefix (forge--topic-type-prefix (car pullreqs))))
          (dolist (pullreq pullreqs)
            (forge-insert-pullreq pullreq width prefix)))
        (insert ?\n)))))

(defun forge-insert-pullreq (pullreq &optional width prefix)
  (with-slots (number title unread-p closed merged) pullreq
    (magit-insert-section (pullreq pullreq t)
      (magit-insert-heading
       (format (if width
                   (format "%%-%is %%s\n" (1+ width))
                 "%s %s\n")
               (propertize (format "%s%s" (or prefix "#") number)
                           'face (if merged
                                     'forge-topic-merged
                                   'forge-topic-unmerged))
               (magit-log-propertize-keywords
                nil (propertize title 'face
                                (cond (unread-p 'forge-topic-unread)
                                      (closed   'forge-topic-closed)
                                      (t        'forge-topic-open))))))
      (when-let ((ref (forge--pullreq-ref pullreq)))
        (magit-insert-section-body
          (cl-letf (((symbol-function #'magit-cancel-section) (lambda ())))
            (magit-insert-log (format "%s..%s" (oref pullreq base-ref) ref)
                              magit-log-section-arguments)))))))

;;; _
(provide 'forge-pullreq)
;;; forge-pullreq.el ends here
