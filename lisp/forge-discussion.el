;;; forge-discussion.el --- Discussion support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

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
   (repository           :initarg :repository)
   (number               :initarg :number)
   (answer               :initarg :answer)
   (state                :initarg :state)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (status               :initarg :status :initform nil)
   (locked-p             :initarg :locked-p :initform nil)
   (body                 :initarg :body)
   (project-cards) ; projectsCards
   (edits) ; userContentEdits
   (labels               :closql-tables (discussion-label label))
   (participants)
   (posts                :closql-class forge-discussion-post)
   (reactions)
   (timeline)
   (marks                :closql-tables (discussion-mark mark))
   (note                 :initarg :note     :initform nil)
   (their-id             :initarg :their-id)
   (slug                 :initarg :slug)
   (saved-p              :initarg :saved-p :initform nil)
   ))

(defclass forge-discussion-post (forge-post)
  ((closql-table         :initform 'discussion-post)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform 'discussion)
   (closql-class-prefix  :initform "forge-discussion-")
   (id                   :initarg :id)
   (their-id             :initarg :their-id)
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
   (their-id             :initarg :their-id)
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

(cl-defmethod forge-get-discussion ((disc forge-discussion))
  disc)

(cl-defmethod forge-get-discussion ((repo forge-repository) number)
  (closql-get (forge-db)
              (forge--object-id 'forge-discussion repo number)
              'forge-discussion))

(cl-defmethod forge-get-discussion ((number integer))
  (and-let* ((repo (forge-get-repository :tracked nil 'notatpt)))
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
      (and (forge-discussion-p forge-buffer-topic)
           forge-buffer-topic)
      (and demand (user-error "No current discussion"))))

(defun forge-discussion-at-point (&optional demand)
  "Return the discussion at point.
If there is no such discussion and DEMAND is non-nil, then signal
an error."
  (or (thing-at-point 'forge-discussion)
      (magit-section-value-if 'discussion)
      (and demand (user-error "No discussion at point"))))

(put 'forge-discussion 'thing-at-point #'forge-thingatpt--discussion)
(defun forge-thingatpt--discussion ()
  (and-let* (((thing-at-point-looking-at "#\\([0-9]+\\)\\_>"))
             (number (string-to-number (match-string 1)))
             (repo (forge--repo-for-thingatpt)))
    (forge-get-discussion repo number)))

;;; Read

(defun forge-read-discussion (prompt)
  "Read an active discussion with completion using PROMPT.

Open, unread and pending discussions are considered active.
Default to the current discussion, even if it isn't active.

\\<forge-read-topic-minibuffer-map>While completion is in \
progress, \\[forge-read-topic-lift-limit] lifts the limit, extending
the completion candidates to include all discussions.

If `forge-limit-topic-choices' is nil, then all candidates
can be selected from the start."
  (forge--read-topic prompt
                     #'forge-current-discussion
                     (forge--topics-spec :type 'discussion :active t)
                     (forge--topics-spec :type 'discussion :active nil :state nil)))

;;; Insert

(defvar-keymap forge-discussions-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-discussions
  "<remap> <magit-visit-thing>"  #'forge-list-discussions
  "<remap> <forge--list-menu>"   #'forge-topics-menu
  "<remap> <forge--item-menu>"   #'forge-topic-menu
  "C-c C-n"                      #'forge-create-discussion)

(defvar-keymap forge-discussion-section-map
  :parent forge-common-map
  "<remap> <magit-visit-thing>"  #'forge-visit-this-topic
  "<remap> <forge--list-menu>"   #'forge-topics-menu
  "<remap> <forge--item-menu>"   #'forge-topic-menu)

(cl-defun forge-insert-discussions (&optional (spec nil sspec) heading)
  "Insert a list of discussions, according to `forge--buffer-topics-spec'.
Optional SPEC can be used to override that filtering specification,
and optional HEADING to change the section heading."
  (when-let (((forge-db t))
             (repo (forge-get-repository :tracked?))
             ((oref repo discussions-p))
             (spec (if sspec spec (forge--clone-buffer-topics-spec)))
             ((memq (oref spec type) '(topic discussion))))
    (oset spec type 'discussion)
    (forge--insert-topics 'discussions
                          (or heading "Discussions")
                          (forge--list-topics spec repo))))

;;; _
(provide 'forge-discussion)
;;; forge-discussion.el ends here
