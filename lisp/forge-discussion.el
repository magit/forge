;;; forge-discussion.el --- Discussion support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

(defclass forge-discussion (forge-topic)
  ((closql-table         :initform 'discussion)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform 'repository)
   (closql-class-prefix  :initform "forge-")
   (id                   :initarg :id)
   (fid                  :initarg :fid)
   (number               :initarg :number)
   (repository           :initarg :repository)
   (answer               :initarg :answer)
   (state                :initarg :state)
   (state-reason         :initarg :state-reason)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (unread-p             :initarg :unread-p :initform nil)
   (done-p               :initarg :done-p   :initform nil)
   (locked-p             :initarg :locked-p :initform nil)
   (body                 :initarg :body)
   (note                 :initarg :note     :initform nil)
   (edits) ; userContentEdits
   (labels               :closql-table (discussion-label label))
   (posts                :closql-class forge-discussion-post)
   (reactions)
   (timeline)
   (marks                :closql-table (discussion-mark mark))
   ))

(defclass forge-discussion-post (forge-post)
  ((closql-table         :initform 'discussion-post)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform 'discussion)
   (closql-class-prefix  :initform "forge-discussion-")
   (id                   :initarg :id)
   (fid                  :initarg :fid)
   (number               :initarg :number)
   (discussion           :initarg :discussion)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   (replies              :closql-class forge-discussion-reply)
   ))

(defclass forge-discussion-reply (forge-post)
  ((closql-table         :initform 'discussion-reply)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform 'post)
   (closql-class-prefix  :initform "forge-discussion-")
   (id                   :initarg :id)
   (fid                  :initarg :fid)
   (number               :initarg :number)
   (post                 :initarg :post)
   (discussion           :initarg :discussion)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ))

;;; Query
;;;; Get

(cl-defmethod forge-get-repository ((post forge-discussion-post))
  (forge-get-repository (forge-get-discussion post)))

(cl-defmethod forge-get-topic ((post forge-discussion-post))
  (forge-get-discussion post))

(cl-defmethod forge-get-discussion ((discussion forge-discussion))
  discussion)

(cl-defmethod forge-get-discussion ((repo forge-repository) number)
  (closql-get (forge-db)
              (forge--object-id 'forge-discussion repo number)
              'forge-discussion))

(cl-defmethod forge-get-discussion ((number integer))
  (and-let* ((repo (forge-get-repository t)))
    (forge-get-discussion repo number)))

(cl-defmethod forge-get-discussion ((id string))
  (closql-get (forge-db) id 'forge-discussion))

(cl-defmethod forge-get-discussion ((post forge-discussion-post))
  (closql-get (forge-db)
              (oref post discussion)
              'forge-discussion))

;; (cl-defmethod forge-get-discussion ((post forge-discussion-reply))
;;   (closql-get (forge-db)
;;               (oref post discussion)
;;               'forge-discussion))

;; (cl-defmethod forge-get-discussion-post ((reply forge-discussion-reply))
;;   (closql-get (forge-db)
;;               (oref reply post)
;;               'forge-discussion-post))

;;;; Current

(defun forge-current-discussion (&optional demand)
  "Return the discussion at point or being visited.
If there is no such discussion and DEMAND is non-nil, then signal
an error."
  (or (forge-discussion-at-point)
      (and (derived-mode-p 'forge-topic-mode)
           (forge-discussion-p forge-buffer-topic)
           forge-buffer-topic)
      (and demand (user-error "No current discussion"))))

(defun forge-discussion-at-point (&optional demand)
  "Return the discussion at point.
If there is no such discussion and DEMAND is non-nil, then signal
an error."
  (or (thing-at-point 'forge-discussion)
      (magit-section-value-if 'discussion)
      (and (derived-mode-p 'forge-topic-list-mode)
           (let ((topic (forge-get-topic (tabulated-list-get-id))))
             (and (forge-discussion-p topic)
                  topic)))
      (and demand (user-error "No discussion at point"))))

(put 'forge-discussion 'thing-at-point #'forge-thingatpt--discussion)
(defun forge-thingatpt--discussion ()
  (and-let* ((repo (forge--repo-for-thingatpt)))
    (and (thing-at-point-looking-at "#\\([0-9]+\\)\\_>")
         (forge-get-discussion repo (string-to-number (match-string 1))))))

;;;; List

(cl-defmethod forge-ls-discussions
  ((repo forge-repository) &optional type select)
  (forge-ls-topics repo 'forge-discussion type select))

(defun forge--ls-recent-discussions (repo)
  (forge-ls-recent-topics repo 'dicussion))

(defun forge--ls-assigned-discussions (repo)
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-discussion (forge-db) row))
          (forge-sql
           [:select $i1 :from [discussion discussion_assignee assignee]
            :where (and (= discussion_assignee:discussion discussion:id)
                        (= discussion_assignee:id    assignee:id)
                        (= discussion:repository     $s2)
                        (= assignee:login       $s3)
                        (isnull discussion:closed))
            :order-by [(desc updated)]]
           (vconcat (closql--table-columns (forge-db) 'discussion t))
           (oref repo id)
           (ghub--username repo))))

(defun forge--ls-authored-discussions (repo)
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-discussion (forge-db) row))
          (forge-sql
           [:select $i1 :from [discussion]
            :where (and (= discussion:repository $s2)
                        (= discussion:author     $s3)
                        (isnull discussion:closed))
            :order-by [(desc updated)]]
           (vconcat (closql--table-columns (forge-db) 'discussion t))
           (oref repo id)
           (ghub--username repo))))

;;; Read

(defun forge-read-discussion (prompt &optional type)
  "Read an discussion with completion using PROMPT.
TYPE can be `open', `closed', or nil to select from all discussions.
TYPE can also be t to select from open discussions, or all discussions
if a prefix argument is in effect."
  (when (eq type t)
    (setq type (if current-prefix-arg nil 'open)))
  (let* ((default (forge-current-discussion))
         (repo    (forge-get-repository (or default t)))
         (choices (mapcar #'forge--format-topic-choice
                          (forge-ls-discussions repo type))))
    (cdr (assoc (magit-completing-read
                 prompt choices nil nil nil nil
                 (and default
                      (setq default (forge--format-topic-choice default))
                      (member default choices)
                      (car default)))
                choices))))

;;; Insert

(defvar-keymap forge-discussions-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-discussions
  "<remap> <magit-visit-thing>"  #'forge-list-discussions
  "C-c C-n"                      #'forge-create-discussion)

(defvar-keymap forge-discussion-section-map
  "<remap> <magit-visit-thing>"  #'forge-visit-this-topic)

(defun forge-insert-discussions ()
  "Insert a list of mostly recent and/or open discussions.
Also see option `forge-topic-list-limit'."
  (forge--insert-discussions "Discussions"
                             #'forge--ls-recent-discussions))

(defun forge-insert-assigned-discussions ()
  "Insert a list of open discussions that are assigned to you."
  (forge--insert-discussions "Assigned discussions"
                             #'forge--ls-assigned-discussions))

(defun forge-insert-authored-discussions ()
  "Insert a list of open discussions that are authored to you."
  (forge--insert-discussions "Authored discussions"
                             #'forge--ls-authored-discussions))

(defun forge--insert-discussions (heading getter)
  (when-let ((repo (forge--assert-insert-topics-get-repository t)))
    (forge--insert-topics 'discussions heading (funcall getter repo))))

;;; _
(provide 'forge-discussion)
;;; forge-discussion.el ends here
