;;; forge-topic.el --- Topics support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

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

(require 'bookmark)
(require 'bug-reference)
(require 'eieio-custom)
(require 'markdown-mode)
(require 'parse-time)
(require 'yaml)

(require 'forge)
(require 'forge-post)

(defvar bug-reference-auto-setup-functions)

(define-obsolete-face-alias 'forge-topic-slug-completed
                            'forge-topic-slug-realized "Forge 0.5.0")

(define-obsolete-face-alias 'forge-topic-slug-unplanned
                            'forge-topic-slug-expunged "Forge 0.5.0")

(define-obsolete-face-alias 'forge-issue-unplanned
                            'forge-issue-expunged "Forge 0.5.0")

;;; Options

(defcustom forge-limit-topic-choices t
  "Whether to initially limit completion candidates to active topics."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'boolean)

(defcustom forge-post-heading-format "%a %C\n"
  "Format for post headings in topic view.

The following %-sequences are supported:

`%a' The forge nickname of the author.
`%c' The absolute creation date.
`%C' The relative creation date."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'string)

(defcustom forge-post-fill-region t
  "Whether to call `fill-region' before displaying forge posts."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'boolean)

(defcustom forge-topic-wash-title-hook
  (list #'magit-highlight-bracket-keywords)
  "Functions used to highlight parts of each individual topic title.

These functions are called in order, in a buffer that containing the
topic title.  They should set text properties as they see fit, usually
just `font-lock-face'.  Before each function is called, point is at the
beginning of the buffer."
  :package-version '(forge . "0.4.7")
  :group 'forge
  :type 'hook
  :options (list #'magit-highlight-bracket-keywords))

(defcustom forge-topic-repository-slug-width 28
  "Width of repository slugs (i.e., \"OWNER/NAME\")."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'natnum)

(defcustom forge-bug-reference-hooks
  '(find-file-hook
    forge-post-mode-hook
    git-commit-setup-hook
    magit-mode-hook)
  "Hooks to which `forge-bug-reference-setup' is added.
This variable has to be customized before `forge' is loaded."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :options '(find-file-hook
             forge-post-mode-hook
             git-commit-setup-hook
             magit-mode-hook)
  :type '(list :convert-widget custom-hook-convert-widget))

(defvar forge-format-avatar-function nil
  "Function used to insert avatars in certain locations.
This is experimental and intended for users who wish to
implement such a function themselves.  See #447.")

;;; Faces
;;;; Common

(defface forge-dimmed '((t :foreground "#93a1a1"))
  "Parent face or faces used for text that shouldn't stand out.

This face is not directly, instead several faces inherit from it
either directly or via an intermediate face.  This face should
only specify the `:foreground' attribute, which is why this face
does not inherit from `magit-dimmed'."
  :group 'forge-faces)

(defface forge-topic-header-line
  '((t :inherit magit-header-line :foreground reset))
  "Face for the `header-line' in `forge-topic-mode' buffers."
  :group 'forge-faces)

(defface forge-discussion-answer-heading
  '((t :inherit magit-diff-added))
  "Face for headings of discussion replies marked as the answer."
  :group 'magit-faces)

;;;; Topic and Notification Slugs

(defface forge-topic-slug-open
  '((t :inherit forge-dimmed))
  "Face uses for slugs of open topics."
  :group 'forge-faces)

(defface forge-topic-slug-realized
  '((t :inherit forge-dimmed))
  "Face used for slugs of realized topics.
Realized topics include:
- completed issues and
- merged pull-requests."
  :group 'forge-faces)

(defface forge-topic-slug-expunged
  '((t :inherit forge-dimmed :strike-through t))
  "Face used for slugs of expunged topics.
Expunged topics include:
- issues closes as unplanned,
- issues closed as duplicates, and
- pull-requests closed without merging."
  :group 'forge-faces)

(defface forge-topic-slug-saved
  '((t :foreground "orange"))
  "Face used for slugs of topics with saved notifications."
  :group 'forge-faces)

(defface forge-topic-slug-unread
  '((t :weight bold))
  "Face used for slugs of topics with unread notifications."
  :group 'forge-faces)

;;;; Topic and Notification Summaries
;;;;; Notifications

(defface forge-topic-unread
  '((t :weight bold
       :box (:line-width (-1 . -1) :style nil)))
  "Face used for summaries of entities with unread notifications.
This face is always used together with, and takes preference over,
a `forge-{issue,pullreq}-STATE' face and should not specify any
attribute that is specified by any of those faces.  Likewise those
faces should not set `:weight' or `:slant'."
  :group 'forge-faces)

(defface forge-topic-pending
  '((t :weight bold))
  "Face used for summaries of entities with open notifications.
This face is always used together with, and takes preference over,
a `forge-{issue,pullreq}-STATE' face and should not specify any
attribute that is specified by any of those faces.  Likewise those
faces should not set `:weight' or `:slant'."
  :group 'forge-faces)

(defface forge-topic-done
  '((t))
  "Face used for summaries of entities with no unread or open notification.
This face is always used together with, and takes preference over,
a `forge-{issue,pullreq}-STATE' face and should not specify any
attribute that is specified by any of those faces.  Likewise those
faces should not set `:weight' or `:slant'."
  :group 'forge-faces)

;;;;; Discussions

(defface forge-discussion-open
  '((t :slant italic))
  "Face used for summaries of open discussions."
  :group 'forge-faces)

(defface forge-discussion-completed
  '((t :inherit forge-dimmed :slant italic))
  "Face used for summaries of discussions closed as completed."
  :group 'forge-faces)

(defface forge-discussion-expunged
  '((t :inherit forge-dimmed :slant italic :strike-through t))
  "Face used for summaries of expunged discussions.
Expunged discussions include:
- discussions closes as unplanned, and
- discussions closed as duplicates."
  :group 'forge-faces)

;;;;; Issues

(defface forge-issue-open
  '((t))
  "Face used for summaries of open issues."
  :group 'forge-faces)

(defface forge-issue-completed
  '((t :inherit forge-dimmed))
  "Face used for summaries of issues closed as completed."
  :group 'forge-faces)

(defface forge-issue-expunged
  '((t :inherit forge-dimmed :strike-through t))
  "Face used for summaries of expunged issues.
Expunged issues include:
- issues closes as unplanned, and
- issues closed as duplicates."
  :group 'forge-faces)

;;;;; Pull-Requests

(defface forge-pullreq-open
  '((t :foreground "LimeGreen"))
  "Face used for summaries of open pull-requests."
  :group 'forge-faces)

(defface forge-pullreq-merged
  '((t :foreground "MediumPurple"))
  "Face used for summaries of merged pull-requests."
  :group 'forge-faces)

(defface forge-pullreq-rejected
  '((t :foreground "MediumPurple" :strike-through t))
  "Face used for summaries of closed pull-requests, that weren't merged."
  :group 'forge-faces)

(defface forge-pullreq-draft
  '((t :inherit highlight))
  "Face used for summaries of draft pull-requests.
A face attribute should be used that is not already used by any
`forge-topic-STATUS' or `forge-{issue,pullreq}-STATE' face."
  :group 'forge-faces)

;;;; Labels

(defface forge-topic-label
  '((t :inherit secondary-selection
       :box (:line-width (-1 . -1) :style released-button)))
  "Face used for topic labels, marks and milestones."
  :group 'forge-faces)

;;;; Post Details

(defface forge-post-author
  '((t :inherit bold))
  "Face used for post author in topic view."
  :group 'forge-faces)

(defface forge-post-date
  '((t :inherit italic))
  "Face used for post date in topic view."
  :group 'forge-faces)

;;; Class

(defclass forge-topic (forge-post) () :abstract t)

(cl-defmethod forge--object-id ((class (subclass forge-topic)) repo number)
  "Return the id for a CLASS object in REPO identified by id NUMBER."
  (base64-encode-string
   (encode-coding-string
    (format "%s:%s%s"
            (base64-decode-string (oref repo id))
            (substring (symbol-name class)
                       (length (oref-default class closql-class-prefix)))
            number)
    'utf-8)
   t))

(cl-defmethod forge--object-id ((prefix string) number-or-id)
  (and number-or-id
       (base64-encode-string
        (encode-coding-string
         (format "%s:%s"
                 (base64-decode-string prefix)
                 (if (numberp number-or-id)
                     number-or-id
                   ;; Currently every ID is base64 encoded.  Unfortunately
                   ;; we cannot use the IDs of Gitlab labels (see comment
                   ;; in the respective `forge--update-labels' method),
                   ;; and have to use their names, which are not encoded.
                   (or (ignore-errors (base64-decode-string number-or-id))
                       number-or-id)))
         'utf-8)
        t)))

(cl-defmethod forge-topic-mark-read ((topic forge-topic))
  (when (eq (oref topic status) 'unread)
    (oset topic status 'pending)))

(cl-defmethod forge--set-topic-marks ((_repo forge-repository) topic marks)
  (oset topic marks
        (forge-sql-car [:select id :from mark :where (in name $v1)]
                       (vconcat marks)))
  (forge-refresh-buffer))

;;; Query
;;;; Get

(cl-defmethod forge-get-parent ((topic forge-topic))
  (forge-get-repository topic))

(cl-defmethod forge-get-repository ((topic forge-topic))
  (closql-get (forge-db)
              (oref topic repository)
              'forge-repository))

(cl-defmethod forge-get-topic ((topic forge-topic))
  topic)

(cl-defmethod forge-get-topic ((repo forge-repository) number-or-id)
  (cond ((stringp number-or-id)
         (or (forge-get-discussion number-or-id)
             (forge-get-issue      number-or-id)
             (forge-get-pullreq    number-or-id)))
        ((< number-or-id 0)
         (forge-get-pullreq repo (abs number-or-id)))
        ((forge-get-discussion repo number-or-id))
        ((forge-get-issue      repo number-or-id))
        ((forge-get-pullreq    repo number-or-id))))

(cl-defmethod forge-get-topic ((number integer))
  (if (< number 0)
      (forge-get-pullreq (abs number))
    (or (forge-get-discussion number)
        (forge-get-issue      number)
        (forge-get-pullreq    number))))

(cl-defmethod forge-get-topic ((id string))
  (or (forge-get-discussion id)
      (forge-get-issue      id)
      (forge-get-pullreq    id)))

;;;; Current

(defun forge-current-topic (&optional demand)
  "Return the topic at point or being visited.
If there is no such topic and DEMAND is non-nil, then signal
an error."
  (or (forge-topic-at-point)
      forge-buffer-topic
      (and demand (user-error "No current topic"))))

(defun forge-topic-at-point (&optional demand)
  "Return the topic at point.
If there is no such topic and DEMAND is non-nil, then signal
an error."
  (or (thing-at-point 'forge-topic)
      (magit-section-value-if '(discussion issue pullreq))
      (forge-get-pullreq :branch)
      (and demand (user-error "No topic at point"))))

(put 'forge-topic 'thing-at-point #'forge-thingatpt--topic)
(defun forge-thingatpt--topic ()
  (and-let ((_(thing-at-point-looking-at "\\([#!]\\)\\([0-9]+\\)\\_>"))
            (prefix (match-string-no-properties 1))
            (number (string-to-number (match-string-no-properties 2)))
            (repo (forge--repo-for-thingatpt)))
    (cond ((equal prefix "#")
           (forge-get-topic repo number))
          ((forge-gitlab-repository--eieio-childp repo)
           (forge-get-pullreq repo number)))))

(defun forge-region-topics ()
  (magit-region-values '(discussion issue pullreq)))

(defun forge-current-topic-type ()
  (magit-section-case
    ([* discussions] 'discussion)
    ([* issues]      'issue)
    ([* pullreqs]    'pullreq)
    (t (or (and forge--buffer-topics-spec
                (oref forge--buffer-topics-spec type))
           'topic))))

;;;; List

(defvar-local forge--buffer-topics-spec nil)
(put 'forge--buffer-topics-spec 'permanent-local t)

(defun forge--init-buffer-topics-spec ()
  (unless forge--buffer-topics-spec
    (setq forge--buffer-topics-spec
          (clone forge-status-buffer-default-topic-filters))))
(add-hook 'magit-status-mode-hook #'forge--init-buffer-topics-spec)

(defun forge--clone-buffer-topics-spec ()
  (forge--init-buffer-topics-spec)
  (clone forge--buffer-topics-spec))

(defclass forge--topics-spec ()
  ((type        :documentation "\
Limit list based on topic type."
                :initarg :type
                :initform 'topic
                :type (member topic discussion issue pullreq nil)
                :custom (choice
                         (const topic)
                         (const discussion)
                         (const issue)
                         (const pullreq)
                         (const :tag "disable topic sections (nil)" nil)))
   (active      :documentation "\
Limit list to active topics.

A topic is \"active\" if its state (public condition) is open and/or
its status (private condition) is unread or pending.

When this is t, then the value of the `state' and `status' slots are
ignored."
                :initarg :active
                :initform t
                :type boolean
                :custom boolean)
   (state       :documentation "\
Limit list based on topic (public) state.

State is the \"public condition\".  I.e., is the topic still open?"
                :initarg :state
                :initform 'open
                :type (satisfies
                       (lambda (val)
                         (member val '(open
                                       closed
                                       (completed merged)
                                       completed
                                       merged
                                       (unplanned duplicate outdated rejected)
                                       unplanned
                                       duplicate
                                       outdated
                                       rejected
                                       nil))))
                :custom (choice
                         (const open)
                         (const closed)
                         (const (completed merged))
                         (const completed)
                         (const merged)
                         (const (unplanned duplicate outdated rejected))
                         (const unplanned)
                         (const duplicate)
                         (const outdated)
                         (const rejected)
                         (const :tag "all (nil)" nil)))
   (status      :documentation "\
Limit list based on topic (private) status.

Status is the \"private condition\".  I.e., have you decided yet
that *you* are done with the topic, and have others made changes,
which *you* have not seen yet?

`inbox' means \"`unread' or `pending'\"."
                :initarg :status
                :initform nil
                :type (member inbox unread pending done nil)
                :custom (choice
                         (const inbox)
                         (const unread)
                         (const pending)
                         (const done)
                         (const :tag "all (nil)" nil)))
   (updated     :documentation "\
Date when topic was last updated."
                :initarg :updated
                :initform nil
                :type (or string null))
   (category    :documentation "\
Limit list to discussions of given category.
Issues and pull-requests are unaffected."
                :initarg :category
                :initform nil
                :type (or string null)
                :custom (choice
                         (string :tag "name")
                         (const :tag "all (nil)" nil)))
   (milestone   :documentation "\
Limit list to issues and pull-requests assigned to given milestone.
Discussions are unaffected."
                :initarg :milestone
                :initform nil
                :type (or string null)
                :custom (choice
                         (string :tag "name")
                         (const :tag "all (nil)" nil)))
   (labels      :documentation "\
Limit list to topics with at least one of the given labels."
                :initarg :labels
                :initform nil
                :type (list-of string)
                :custom (repeat string))
   (marks       :documentation "\
Limit list to topics with at least one of the given marks.
Marks are like labels, but they are private and local to the
current Forge database."
                :initarg :marks
                :initform nil
                :type (list-of string)
                :custom (repeat string))
   (saved       :documentation "Limit list to saved topics."
                :initarg :saved
                :initform nil
                :type boolean
                :custom boolean)
   (author      :documentation "\
Limit list to topics created by given user."
                :initarg :author
                :initform nil
                :label "Author"
                :type (or string null)
                :custom (choice
                         (string :tag "username")
                         (const :tag "no filter (nil)" nil)))
   (assignee    :documentation "\
Limit list to topics assigned to given user."
                :initarg :assignee
                :initform nil
                :label "Assignee"
                :type (or string null)
                :custom (choice
                         (string :tag "username")
                         (const :tag "no filter (nil)" nil)))
   (reviewer    :documentation "\
Limit list to topics for which a review by the given user was requested."
                :initarg :reviewer
                :initform nil
                :label "Reviewer"
                :type (or string null)
                :custom (choice
                         (string :tag "username")
                         (const :tag "no filter (nil)" nil)))
   (global      :documentation "Whether to list topics for all repositories."
                :initarg :global
                :initform nil
                :type boolean)
   (order       :documentation "Order in which topics are listed."
                :initarg :order
                :initform 'newest
                :type (member newest oldest recently-updated anciently-updated)
                :custom (choice (const newest)
                                (const oldest)
                                (const recently-updated)
                                (const anciently-updated)))
   (limit       :documentation "Number of topics to list at most."
                :initarg :limit
                :initform 200
                :type (or integer null)
                :custom (choice natnum (const :tag "no limit" nil)))
   (grouped     :documentation "Whether to group topics by repository."
                :initarg :grouped
                :initform nil
                :type boolean
                :custom boolean)))

(defun forge--cast-topics-spec-state (spec)
  (when-let ((cast (pcase (list (oref spec type) (oref spec state))
                     (`(topic ,(or 'unplanned 'duplicate 'rejected))
                      '(unplanned duplicate rejected))
                     ('(issue rejected)
                      '(unplanned duplicate rejected))
                     (`(pullreq ,(or 'unplanned 'duplicate))
                      '(unplanned duplicate rejected))
                     (`(topic ,(or 'completed 'merged))
                      '(completed merged))
                     ('(issue merged)
                      '(completed merged))
                     ('(pullreq completed)
                      '(completed merged)))))
    (oset spec state cast)))

(cl-defun forge--list-topics
    (&optional (spec forge--buffer-topics-spec)
               (repo (forge-get-repository :tracked?))
               (type (oref spec type)))
  (when (oref spec reviewer)
    (setq type 'pullreq))
  (if (eq type 'topic)
      (pcase-let ((`(,pred ,slot) (pcase (oref spec order)
                                    ('newest             '(> number))
                                    ('oldest             '(< number))
                                    ('recently-updated   '(string> updated))
                                    ('anciently-updated  '(string< updated)))))
        (cl-sort (nconc (forge--list-topics-1 spec repo 'discussion)
                        (forge--list-topics-1 spec repo 'issue)
                        (forge--list-topics-1 spec repo 'pullreq))
                 pred :key (##eieio-oref % slot)))
    (forge--list-topics-1 spec repo type)))

(defun forge--list-topics-1 (spec repo type)
  (mapcar (partial #'closql--remake-instance
                   (pcase type
                     ('discussion 'forge-discussion)
                     ('issue      'forge-issue)
                     ('pullreq    'forge-pullreq))
                   (forge-db))
          (forge-sql (forge--list-topics-2 spec repo type))))

(defun forge--list-topics-2 (spec repo type)
  (pcase-let (((eieio active state status category milestone labels marks
                      saved author assignee reviewer global order limit)
               spec))
    (cond (active
           (setq state 'open)
           (setq status '(unread pending)))
          ((eq status 'inbox)
           (setq status '(unread pending))))
    (when (eq state 'closed)
      (setq state '( completed merged unplanned duplicate outdated rejected)))
    `[:select :distinct topic:*
      :from [(as ,type topic)]
      ,@(pcase type
          ((and 'discussion (guard category))
           `[:join discussion-category :on (= discussion-category:name ,category)])
          ((and (or 'issue 'pullreq) (guard milestone))
           `[:join milestone :on (= milestone:title ,milestone)]))
      ,@(pcase (and labels type)
          ('discussion
           [:join discussion-label :on (= discussion-label:discussion  topic:id)
            :join            label :on (= label:id          discussion-label:id)])
          ('issue
           [:join      issue-label :on (= issue-label:issue            topic:id)
            :join            label :on (= label:id               issue-label:id)])
          ('pullreq
           [:join    pullreq-label :on (= pullreq-label:pullreq        topic:id)
            :join            label :on (= label:id             pullreq-label:id)]))
      ,@(pcase (and marks type)
          ('discussion
           [:join discussion-mark :on (= discussion-mark:discussion  topic:id)
            :join            mark :on (= mark:id           discussion-mark:id)])
          ('issue
           [:join      issue-mark :on (= issue-mark:issue            topic:id)
            :join            mark :on (= mark:id                issue-mark:id)])
          ('pullreq
           [:join    pullreq-mark :on (= pullreq-mark:pullreq        topic:id)
            :join            mark :on (= mark:id              pullreq-mark:id)]))
      ,@(pcase (and assignee type)
          ('issue
           [:join      issue-assignee :on (= issue-assignee:issue            topic:id)
            :join            assignee :on (= assignee:id            issue-assignee:id)])
          ('pullreq
           [:join    pullreq-assignee :on (= pullreq-assignee:pullreq        topic:id)
            :join            assignee :on (= assignee:id          pullreq-assignee:id)]))
      ,@(and reviewer
             [:join (as pullreq-review-request r) :on (= r:pullreq  topic:id)
              :join assignee                      :on (= assignee:id    r:id)])
      :where
      (and
       ,@(and (not global) repo `((= topic:repository ,(oref repo id))))
       ,@(cond
          ((and active state status)
           `((or (in topic:state  ,(vconcat (ensure-list state)))
                 (in topic:status ,(vconcat (ensure-list status))))))
          (`(,@(and state  `((in topic:state  ,(vconcat (ensure-list state)))))
             ,@(and status `((in topic:status ,(vconcat (ensure-list status))))))))
       ,@(pcase type
          ((and 'discussion (guard category))
           '((= topic:category discussion-category:id)))
          ((and (or 'issue 'pullreq) (guard milestone))
           '((= topic:milestone milestone:id))))
       ,@(and labels    `((or ,@(mapcar (##`(= label:name ,%)) labels))))
       ,@(and marks     `((or ,@(mapcar (##`(=  mark:name ,%))  marks))))
       ,@(and saved     '((= topic:saved-p  't)))
       ,@(and author    `((= topic:author   ,author)))
       ,@(and assignee (memq type '(issue pullreq))
              `((= assignee:login ,assignee)))
       ,@(and reviewer (eq type 'pullreq)
              `((= assignee:login ,reviewer))))
      :order-by [,(pcase order
                    ('newest            '(desc topic:number))
                    ('oldest            '(asc  topic:number))
                    ('recently-updated  '(desc topic:updated))
                    ('anciently-updated '(asc  topic:updated)))]
      ,@(and limit `(:limit ,limit))]))

;;; Read

(defun forge-read-topic (prompt)
  "Read an active topic with completion using PROMPT.

Open, unread and pending topics are considered active.
Default to the current topic, even if it isn't active.

\\<forge-read-topic-minibuffer-map>While completion is in \
progress, \\[forge-read-topic-lift-limit] lifts the limit, extending
the completion candidates to include all topics.

If `forge-limit-topic-choices' is nil, then all candidates
can be selected from the start."
  (forge--read-topic prompt
                     #'forge-current-topic
                     (forge--topics-spec :type 'topic :active t)
                     (forge--topics-spec :type 'topic :active nil :state nil)))

(defun forge--read-topic (prompt current active all)
  (let* ((current (funcall current))
         (repo    (forge-get-repository (or current :tracked)))
         (default (and current (forge--format-topic-line current)))
         (alist   (forge--topic-collection
                   (forge--list-topics (if forge-limit-topic-choices active all)
                                       repo)))
         (choices (mapcar #'car alist))
         (choices (cond ((and forge-limit-topic-choices
                              default
                              (not (member default choices)))
                         (push (cons default (oref current id)) alist)
                         (cons default choices))
                        (choices)))
         (choice
          (if forge-limit-topic-choices
              (minibuffer-with-setup-hook
                  (lambda ()
                    (use-local-map (make-composed-keymap
                                    forge-read-topic-minibuffer-map
                                    (current-local-map))))
                (magit-completing-read
                 (substitute-command-keys
                  (format "%s \\<%s> (\\[%s] for all)" prompt
                          'forge-read-topic-minibuffer-map
                          'forge-read-topic-lift-limit))
                 (completion-table-dynamic
                  (let (all-choices)
                    (lambda (_string)
                      (cond
                       (all-choices)
                       (forge-limit-topic-choices choices)
                       (t
                        (forge--replace-minibuffer-prompt (concat prompt ": "))
                        (setq alist (forge--topic-collection
                                     (forge--list-topics all repo)))
                        (setq all-choices (mapcar #'car alist)))))))
                 nil t nil nil default))
            (magit-completing-read prompt choices nil t nil nil default))))
    (cdr (assoc choice alist))))

(defun forge--topic-collection (topics)
  (mapcar (##cons (forge--format-topic-line %)
                  (oref % id))
          topics))

(defvar-keymap forge-read-topic-minibuffer-map
  "+" #'forge-read-topic-lift-limit)

(defun forge-read-topic-lift-limit ()
  "No longer limit completion candidates to active topics."
  (interactive)
  (when (and (minibufferp)
             forge-limit-topic-choices)
    (setq-local forge-limit-topic-choices nil)
    (when (and (bound-and-true-p vertico-mode)
               (boundp 'vertico--input)
               (fboundp 'vertico--exhibit))
      (setq vertico--input t)
      (vertico--exhibit))))

(defun forge--replace-minibuffer-prompt (prompt)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (end (length prompt)))
      ;; (insert-and-inherit prompt) would discard all faces already
      ;; present in PROMPT, so instead we do it like `read_minibuf'.
      (put-text-property 0 end 'front-sticky t prompt)
      (put-text-property 0 end 'rear-nonsticky t prompt)
      (put-text-property 0 end 'field t prompt)
      (let ((props minibuffer-prompt-properties))
        (while props
          (let ((key (pop props))
                (val (pop props)))
            (if (eq key 'face)
                (add-face-text-property 0 end val t prompt)
              (put-text-property 0 end key val prompt)))))
      (insert prompt)
      (delete-region (point) (minibuffer-prompt-end)))))

(defun forge-topic-completion-at-point ()
  (let ((bol (line-beginning-position))
        repo)
    (and (looking-back "[!#][0-9]*" bol)
         (or (not bug-reference-prog-mode)
             (nth 8 (syntax-ppss))) ; inside comment or string
         (setq repo (forge-get-repository :tracked))
         (looking-back (if (forge--childp repo 'forge-gitlab-repository)
                           "\\(?3:[!#]\\)\\(?2:[0-9]*\\)"
                         "#\\(?2:[0-9]*\\)")
                       bol)
         (list (match-beginning 2)
               (match-end 0)
               (mapcar (lambda (row)
                         (propertize (number-to-string (car row))
                                     :title (format " %s" (cadr row))))
                       (if (forge--childp repo 'forge-gitlab-repository)
                           (forge-sql [:select [number title]
                                       :from $i1
                                       :where (= repository $s2)
                                       :order-by [(desc updated)]]
                                      (if (equal (match-string 3) "#")
                                          'issue
                                        'pullreq)
                                      (oref repo id))
                         (forge-sql [:select [number title updated]
                                     :from discussion
                                     :where (= repository $s1)
                                     :union
                                     :select [number title updated]
                                     :from issue
                                     :where (= repository $s1)
                                     :union
                                     :select [number title updated]
                                     :from pullreq
                                     :where (= repository $s1)
                                     :order-by [(desc updated)]]
                                    (oref repo id))))
               :annotation-function (##get-text-property 0 :title %)))))

(defun forge-read-topic-title (topic)
  (read-string "Title: " (oref topic title)))

(defun forge-read-topic-milestone (&optional topic)
  (magit-completing-read
   "Milestone"
   (cons ""
         (mapcar #'caddr
                 (oref (forge-get-repository (or topic :tracked)) milestones)))
   nil t
   (and topic (forge--format-topic-milestone topic))))

(defun forge-read-topic-labels (&optional obj)
  (let ((crm-separator ","))
    (magit-completing-read-multiple
     "Labels: "
     (forge--format-labels (forge-get-repository (or obj :tracked)))
     nil t
     (and (cl-typep obj 'forge-topic)
          (forge--format-labels obj crm-separator)))))

(defun forge-read-topic-marks (&optional obj)
  (let ((crm-separator ","))
    (magit-completing-read-multiple
     "Marks: " (forge--format-marks) nil t
     (and (cl-typep obj 'forge-topic)
          (forge--format-marks obj crm-separator)))))

(defun forge-read-topic-assignees (&optional topic)
  (let* ((repo (forge-get-repository (or topic :tracked)))
         (value (and topic (oref topic assignees)))
         (choices (mapcar #'cadr (oref repo assignees)))
         (crm-separator ","))
    (magit-completing-read-multiple
     "Assignees: " choices nil
     (if (forge--childp repo 'forge-gitlab-repository)
         t ; Selecting something else would fail later on.
       'confirm)
     (mapconcat #'cadr value ","))))

(defun forge-read-topic-review-requests (&optional topic)
  (let* ((repo (forge-get-repository (or topic :tracked)))
         (value (and topic (oref topic review-requests)))
         (choices (nconc (mapcar #'cadr (oref repo assignees))
                         (oref repo teams)))
         (crm-separator ","))
    (magit-completing-read-multiple
     "Request review from: " choices nil
     'confirm
     (mapconcat #'cadr value ","))))

;;; Format

(cl-defmethod forge--format ((topic forge-topic) slot &optional spec)
  (forge--format (forge-get-repository topic) slot
                 `(,@spec (?i . ,(oref topic number)))))

(defun forge--format-topic-line (topic &optional width)
  (concat
   (and (or (and (derived-mode-p 'forge-notifications-mode)
                 (eq forge-notifications-display-style 'flat))
            (and (derived-mode-p 'forge-topics-mode)
                 (oref forge--buffer-topics-spec global)
                 (not (oref forge--buffer-topics-spec grouped))))
        (concat (truncate-string-to-width
                 (oref (forge-get-repository topic) slug)
                 forge-topic-repository-slug-width
                 nil ?\s t)
                " "))
   (string-pad (forge--format-topic-slug topic) (or width 5))
   " "
   (forge--format-topic-title topic)))

(defun forge--format-topic-slug (topic)
  (with-slots (slug state status saved-p) topic
    (magit--propertize-face
     slug
     `(,@(and saved-p               '(forge-topic-slug-saved))
       ,@(and (eq status 'unread)   '(forge-topic-slug-unread))
       ,(pcase state
          ('open                    'forge-topic-slug-open)
          ((or 'completed 'merged)  'forge-topic-slug-completed)
          ((or 'unplanned 'outdated 'duplicate 'rejected)
           'forge-topic-slug-expunged))))))

(defun forge--format-topic-refs (topic)
  (pcase-let
      (((eieio cross-repo-p base-repo base-ref head-repo head-ref) topic)
       (separator (magit--propertize-face ":" 'magit-dimmed))
       (deleted (magit--propertize-face "(deleted)" 'magit-dimmed)))
    (concat (if cross-repo-p
                (concat base-repo separator base-ref)
              base-ref)
            (magit--propertize-face "..." 'magit-dimmed)
            (if cross-repo-p
                (if (and head-repo head-ref)
                    (concat head-repo separator head-ref)
                  deleted)
              (or head-ref deleted)))))

(defun forge--format-topic-draft (topic)
  (if (oref topic draft-p)
      (magit--propertize-face "yes" 'bold)
    (magit--propertize-face "no" 'magit-dimmed)))

(defun forge--format-topic-saved (topic)
  (if (oref topic saved-p)
      (magit--propertize-face "yes" 'bold)
    (magit--propertize-face "no" 'magit-dimmed)))

(defun forge--format-topic-title (topic)
  (with-temp-buffer
    (save-excursion
      (with-slots (title status state) topic
        (insert
         (magit--propertize-face
          title
          `(,@(and (forge-pullreq-p topic)
                   (oref topic draft-p)
                   '(forge-pullreq-draft))
            ,(pcase status
               ('unread  'forge-topic-unread)
               ('pending 'forge-topic-pending)
               ('done    'forge-topic-done))
            ,(pcase (list (eieio-object-class topic) state)
               (`(forge-discussion  open)       'forge-discussion-open)
               (`(forge-discussion  completed)  'forge-discussion-completed)
               (`(forge-discussion  outdated)   'forge-discussion-expunged)
               (`(forge-discussion  duplicate)  'forge-discussion-expunged)
               (`(forge-issue       open)       'forge-issue-open)
               (`(forge-issue       completed)  'forge-issue-completed)
               (`(forge-issue       unplanned)  'forge-issue-expunged)
               (`(forge-issue       duplicate)  'forge-issue-expunged)
               (`(forge-pullreq     open)       'forge-pullreq-open)
               (`(forge-pullreq     merged)     'forge-pullreq-merged)
               (`(forge-pullreq     rejected)   'forge-pullreq-rejected)))))))
    (run-hook-wrapped 'forge-topic-wash-title-hook
                      (##prog1 nil (save-excursion (funcall %))))
    (buffer-string)))

(defun forge--format-topic-category (topic)
  (and-let* ((id (oref topic category))
             (str (forge-sql1 [:select [name]
                               :from discussion-category
                               :where (= id $s1)]
                              id)))
    (magit--propertize-face str 'forge-topic-label)))

(defun forge--format-topic-milestone (topic)
  (and-let* ((id (oref topic milestone))
             (str (forge-sql1 [:select [title]
                               :from milestone
                               :where (= id $s1)]
                              id)))
    (magit--propertize-face str 'forge-topic-label)))

(defun forge--format-labels (&optional arg concat)
  (and-let*
      ((local t)
       (labels (cond
                ((eieio-object-p arg)
                 (oref arg labels))
                ((forge-buffer-repository)
                 (forge-sql-cdr `[:select label:* :from label :where
                                  ,(if arg
                                       '(and (= repository $s1)
                                             (in name $v2))
                                     '(= repository $s1))
                                  :order-by [(asc name)]]
                                forge-buffer-repository
                                (vconcat arg)))
                (t
                 (setq local nil)
                 (forge-sql `[:select :distinct name :from label
                             ,@(and arg '(:where (in name $v1)))
                              :order-by [(asc name)]]
                            (vconcat arg)))))
       (format (if local
                   (pcase-lambda (`(,_id ,name ,color ,_description))
                     (let* ((background (forge--sanitize-color color))
                            (foreground (readable-foreground-color background)))
                       (magit--propertize-face
                        name `(( :background ,background
                                 :foreground ,foreground)
                               forge-topic-label))))
                 (pcase-lambda (`(,name))
                   (magit--propertize-face name 'forge-topic-label)))))
    (if concat
        (mapconcat format labels (if (stringp concat) concat " "))
      (mapcar format labels))))

(defun forge--format-marks (&optional arg concat)
  (and-let ((marks (if (forge-topic--eieio-childp arg)
                       (oref arg marks)
                     ;; Unlike labels, marks are not repo-specific.
                     (when (forge-repository-p arg) (setq arg nil))
                     (forge-sql-cdr `[:select * :from mark
                                      ,@(and arg '(:where (in name $v1)))
                                      :order-by [(asc name)]]
                                    (vconcat arg))))
            (format (pcase-lambda (`(,_id ,name ,face ,_description))
                      (magit--propertize-face
                       name (list face 'forge-topic-label)))))
    (if concat
        (mapconcat format marks (if (stringp concat) concat " "))
      (mapcar format marks))))

(defun forge--format-topic-state (topic)
  (with-slots (state) topic
    (magit--propertize-face
     (symbol-name state)
     (pcase (list (if (forge-issue-p topic) 'issue 'pullreq) state)
       ('(discussion  open)       'forge-discussion-open)
       ('(discussion  completed)  'forge-discussion-completed)
       ('(discussion  outdated)   'forge-discussion-expunged)
       ('(discussion  duplicate)  'forge-discussion-expunged)
       ('(issue       open)       'forge-issue-open)
       ('(issue       completed)  'forge-issue-completed)
       ('(issue       unplanned)  'forge-issue-expunged)
       ('(issue       duplicate)  'forge-issue-expunged)
       ('(pullreq     open)       'forge-pullreq-open)
       ('(pullreq     merged)     'forge-pullreq-merged)
       ('(pullreq     closed)     'forge-pullreq-rejected)))))

(defun forge--format-topic-status (topic)
  (with-slots (status) topic
    (magit--propertize-face
     (symbol-name status)
     (pcase status
       ('unread  'forge-topic-unread)
       ('pending 'forge-topic-pending)
       ('done    'forge-topic-done)))))

(defun forge--format-topic-assignees (arg)
  (and-let ((assignees
             (cond ((eieio-object-p arg)
                    (oref arg assignees))
                   ((forge-buffer-repository)
                    (forge-sql-cdr [:select * :from assignee
                                    :where
                                    (and (= repository $s1)
                                         (in login $v2))
                                    :order-by [(asc login)]]
                                   forge-buffer-repository
                                   (vconcat arg))))))
    (mapconcat #'forge--format-person assignees ", ")))

(defun forge--format-topic-review-requests (topic)
  (and$ (oref topic review-requests)
        (mapconcat #'forge--format-person $ ", ")))

(defun forge--format-person (person)
  (pcase-let* ((`(,_id ,login ,name) person)
               (avatar (forge--format-avatar login)))
    (propertize (if name
                    (format "%s%s (@%s)" avatar name login)
                  (format "%s@%s" avatar login))
                'face 'transient-value)))

(defun forge--format-avatar (person)
  (if forge-format-avatar-function
      (funcall forge-format-avatar-function person)
    ""))

(defun forge--format-boolean (slot name &optional obj)
  ;; Booleans are formatted differently in transients and headers.
  ;; Use this to format the (complete) description of suffix commands.
  (let ((obj (or obj (forge-current-topic))))
    (if (and obj (slot-exists-p obj slot))
        (format (propertize "[%s]" 'face 'transient-delimiter)
                (propertize name 'face
                            (if (eieio-oref obj slot)
                                'transient-value
                              'transient-inactive-value)))
      (format "[%s]" name))))

;;; Insert

(defun forge-insert-topics (type heading prepare)
  "Insert a list of topics, according to PREPARE.

This function is not intended to be added to section hooks directly.
Instead create a function, which calls this function, and add that
wrapper to the mode's section hook.

PREPARE is a function which takes one arguments the repository object,
and must return a filter object of type `forge--topics-spec' or nil.
Insert no topics if PREPARE returns nil, or if the current repository
isn't tracked or Forge hasn't been fully setup yet (in the latter two
cases don't even call PREPARE).

The filter object can be created either using `forge--topics-spec' or
by `clone'ing the object returned by `forge--init-buffer-topics-spec',
to share some settings with other topic lists in the same buffer.
See `forge--topics-spec' for the valid slots and their values.

HEADING is used as the heading of the list section and TYPE is used as
its type.  TYPE should be a symbol of the form `SUBSET-KIND', where KIND
is one of `topics', `issues' or `pullreqs', and SUBSET should describe
what subset of KIND is being listed.

For example, to insert a list of issues assigned to you use something
like:

  (defun my-forge-insert-assigned-issues ()
    \"Insert a list of issues that are assigned to me.\"
    (forge-insert-topics \\='assigned-issues \"Assigned issues\"
      (lambda (repo)
        (and-let* ((me (ghub--username repo)))
          (forge--topics-spec :type \\='issue :active t
                              :assignee me)))))

  (magit-add-section-hook \\='magit-status-sections-hook
                          #\\='my-forge-insert-assigned-issues
                          #\\='forge-insert-issues)

Grep Forge for more examples.

Alternatively you can use `forge-topics-setup-buffer' to list a set
of topics in a dedicated buffer."
  (declare (indent defun))
  (when-let* ((_(forge-db t))
              (repo (forge-get-repository :tracked?))
              (spec (funcall prepare repo)))
    (forge--insert-topics type heading (forge--list-topics spec repo))))

(defun forge--insert-topics (type heading topics)
  (when topics
    (let ((width (apply #'max (mapcar (##length (oref % slug)) topics))))
      (magit-insert-section ((eval type) heading t)
        (magit-insert-heading
          (concat (magit--propertize-face (concat heading " ")
                                          'magit-section-heading)
                  (magit--propertize-face (format "(%s)" (length topics))
                                          'magit-section-child-count)))
        (magit-insert-section-body
          (dolist (topic topics)
            (forge--insert-topic topic width))
          (insert ?\n)
          (magit-make-margin-overlay nil t))))))

(defun forge--insert-topic (topic &optional width)
  (magit-insert-section ((eval (oref topic closql-table)) topic t)
    (insert (forge--format-topic-line topic (or width 5)))
    (forge--insert-topic-marks topic t)
    (forge--insert-topic-labels topic t)
    (insert "\n")
    (magit-log-format-author-margin
     (oref topic author)
     (format-time-string "%s" (parse-iso8601-time-string (oref topic created))))
    (when (and (slot-exists-p topic 'merged)
               (not (oref topic merged)))
      (magit-insert-heading)
      (magit-insert-section-body
        (forge--insert-pullreq-commits topic)))))

(defun forge--insert-topic-labels (topic &optional separate)
  (and-let ((labels (oref topic labels)))
    (prog1 t
      (pcase-dolist (`(,_id ,name ,color ,description) labels)
        (let* ((background (forge--sanitize-color color))
               (foreground (readable-foreground-color background)))
          (if separate (insert " ") (setq separate t))
          (insert name)
          (let ((o (make-overlay (- (point) (length name)) (point))))
            (overlay-put o 'priority 2)
            (overlay-put o 'evaporate t)
            (overlay-put o 'font-lock-face
                         `(( :background ,background
                             :foreground ,foreground)
                           forge-topic-label))
            (when description
              (overlay-put o 'help-echo description))))))))

(defun forge--insert-topic-marks (topic &optional separate)
  (and-let ((marks (oref topic marks)))
    (prog1 t
      (pcase-dolist (`(,_id ,name ,face ,description) marks)
        (if separate (insert " ") (setq separate t))
        (insert name)
        (let ((o (make-overlay (- (point) (length name)) (point))))
          (overlay-put o 'priority 2)
          (overlay-put o 'evaporate t)
          (overlay-put o 'font-lock-face (list face 'forge-topic-label))
          (when description
            (overlay-put o 'help-echo description)))))))

;;; Modes

(defvar-keymap forge-common-map
  :doc "Parent keymap of many of Forge's keymaps.
Keymaps that use this keymap as the/a parent keymap, remap the
place-holder commands `forge--list-menu' and/or `forge--item-menu'
to the appropriate menu command.  To change the keys bound to menu
commands in all Forge keymaps, one only has to change them here."
  "C-c C-c"    #'forge--list-menu
  "C-c RET"    #'forge--item-menu
  "C-<return>" #'forge--item-menu)

(defun forge--list-menu ()
  "Place-holder menu command.  See `forge-common-map'."
  (interactive)
  (message "No list menu available here"))
(put 'forge--list-menu 'completion-predicate #'ignore)

(defun forge--item-menu ()
  "Place-holder menu command.  See `forge-common-map'."
  (interactive)
  (message "No item menu available here"))
(put 'forge--item-menu 'completion-predicate #'ignore)

(defvar-keymap forge-post-section-map
  "<remap> <magit-edit-thing>"   #'forge-edit-post
  "C-c C-k"                      #'forge-delete-comment)

(defvar-keymap forge-topic-mode-map
  :parent (make-composed-keymap forge-common-map magit-mode-map)
  "<remap> <magit-visit-thing>"  #'markdown-follow-link-at-point
  "<mouse-2>"                    #'markdown-follow-link-at-point
  "<remap> <forge--item-menu>"   #'forge-topic-menu
  "<remap> <forge--list-menu>"   #'forge-topic-menu
  "C-c C-n"                      #'forge-create-post
  "C-c C-r"                      #'forge-create-post)

(define-derived-mode forge-topic-mode magit-mode "Topic"
  "Parent major mode of `forge-{issue,pullreq}-mode'.
This mode itself is never used directly."
  :interactive nil
  (face-remap-add-relative 'header-line 'forge-topic-header-line)
  (setq-local markdown-translate-filename-function
              #'forge--markdown-translate-filename-function))

(defvar-keymap forge-discussion-mode-map :parent forge-topic-mode-map)
(define-derived-mode forge-discussion-mode forge-topic-mode "Discussion"
  "Mode for looking at a Forge discussion.")
(defalias 'forge-discussion-setup-buffer   #'forge-topic-setup-buffer)
(defalias 'forge-discussion-refresh-buffer #'forge-topic-refresh-buffer)
(defvar forge-discussion-headers-hook
  '(forge-insert-topic-state
    forge-insert-topic-status
    forge-insert-topic-category
    forge-insert-topic-labels
    forge-insert-topic-marks))

(defvar-keymap forge-issue-mode-map :parent forge-topic-mode-map)
(define-derived-mode forge-issue-mode forge-topic-mode "Issue"
  "Major mode for looking at a Forge issue."
  :interactive nil)
(defalias 'forge-issue-setup-buffer   #'forge-topic-setup-buffer)
(defalias 'forge-issue-refresh-buffer #'forge-topic-refresh-buffer)
(defvar forge-issue-headers-hook
  '(forge-insert-topic-state
    forge-insert-topic-status
    forge-insert-topic-milestone
    forge-insert-topic-labels
    forge-insert-topic-marks
    forge-insert-topic-assignees))

(defvar-keymap forge-pullreq-mode-map :parent forge-topic-mode-map)
(define-derived-mode forge-pullreq-mode forge-topic-mode "Pull-request"
  "Major mode for looking at a Forge pull-request."
  :interactive nil)
(defalias 'forge-pullreq-setup-buffer   #'forge-topic-setup-buffer)
(defalias 'forge-pullreq-refresh-buffer #'forge-topic-refresh-buffer)
(defvar forge-pullreq-headers-hook
  '(forge-insert-topic-state
    forge-insert-topic-draft
    forge-insert-topic-status
    forge-insert-topic-saved
    forge-insert-topic-refs
    forge-insert-topic-milestone
    forge-insert-topic-labels
    forge-insert-topic-marks
    forge-insert-topic-assignees
    forge-insert-topic-review-requests))

(defvar-local forge-buffer-topic nil)

(defun forge-topic-setup-buffer (topic)
  (let* ((repo (forge-get-repository topic))
         (name (format "*forge: %s %s*" (oref repo slug) (oref topic slug)))
         (magit-generate-buffer-name-function (lambda (_mode _value) name))
         (mode (pcase-exhaustive (eieio-object-class topic)
                 ('forge-discussion #'forge-discussion-mode)
                 ('forge-issue      #'forge-issue-mode)
                 ('forge-pullreq    #'forge-pullreq-mode)))
         (buffer (magit-setup-buffer mode t
                   :buffer name
                   :directory (or (forge-get-worktree repo) "/")
                   (forge-buffer-topic topic))))
    (forge-topic-mark-read topic)
    buffer))

(defun forge-topic-refresh-buffer ()
  (let ((topic (closql-reload forge-buffer-topic)))
    (setq forge-buffer-topic topic)
    (magit-set-header-line-format (forge--format-topic-line topic))
    (magit-insert-section (topicbuf)
      (magit-insert-headers
       (pcase major-mode
         ('forge-discussion-mode 'forge-discussion-headers-hook)
         ('forge-issue-mode      'forge-issue-headers-hook)
         ('forge-pullreq-mode    'forge-pullreq-headers-hook)))
      (when (forge-pullreq-p topic)
        (magit-insert-section (pullreq topic)
          (magit-insert-heading "Commits")
          (forge--insert-pullreq-commits topic t)))
      (when-let ((note (oref topic note)))
        (magit-insert-section (note)
          (magit-insert-heading "Note")
          (insert (forge--fontify-markdown note) "\n\n")))
      (forge-insert-post topic nil)
      (dolist (post (oref topic posts))
        (forge-insert-post post topic))
      (when (and (display-images-p)
                 (fboundp 'markdown-display-inline-images))
        (let ((markdown-display-remote-images t))
          (markdown-display-inline-images))))))

(defun forge-insert-post (post topic)
  (magit-insert-section (post post)
    (forge-insert-post-heading post topic)
    (forge-insert-post-content post)
    (when (forge-discussion-p topic)
      (dolist (reply (oref post replies))
        (magit-insert-section (post reply)
          (forge-insert-post-heading reply topic)
          (forge-insert-post-content reply))))))

(defun forge-insert-post-heading (post topic)
  (oset magit-insert-section--current
        heading-highlight-face
        'magit-diff-hunk-heading-highlight)
  (let* ((author  (oref post author))
         (created (oref post created))
         (heading
          (format-spec
           forge-post-heading-format
           `((?a . ,(propertize (concat (forge--format-avatar author)
                                        (or author "(ghost)"))
                                'font-lock-face 'forge-post-author))
             (?c . ,(propertize created 'font-lock-face 'forge-post-date))
             (?C . ,(propertize (apply #'format "%s %s ago"
                                       (magit--age
                                        (float-time
                                         (date-to-time created))))
                                'font-lock-face 'forge-post-date))))))
    (when (forge-discussion-reply-p post)
      (setq heading (concat "    " heading)))
    (font-lock-append-text-property
     0 (length heading)
     'font-lock-face (cond-let*
                       ([_(forge-discussion-p topic)]
                        [answer (oref topic answer)]
                        [_(equal (oref post their-id)
                                 (forge--their-id answer))]
                        'forge-discussion-answer-heading)
                       ((forge-discussion-reply-p post)
                        '(magit-dimmed magit-diff-hunk-heading))
                       ('magit-diff-hunk-heading))
     heading)
    (magit-insert-heading heading)))

(defun forge-insert-post-content (post)
  (insert (forge--fontify-markdown
           (oref post body)
           (and (forge-discussion-reply-p post) 4)))
  (insert "\n\n"))

(cl-defmethod magit-buffer-value (&context (major-mode forge-topic-mode))
  (oref forge-buffer-topic slug))

;;; Bookmarks

(cl-defmethod magit-bookmark-name
  (&context (major-mode forge-topic-mode))
  (concat (oref (forge-get-repository forge-buffer-topic) slug)
          (oref forge-buffer-topic slug)))

(cl-defmethod magit-bookmark-get-value
  (bookmark &context (major-mode forge-topic-mode))
  (bookmark-prop-set bookmark 'forge-topic (oref forge-buffer-topic id)))

(cl-defmethod magit-bookmark-get-buffer-create
  (bookmark (_mode (derived-mode forge-topic-mode)))
  (let ((magit-display-buffer-function #'identity)
        (magit-display-buffer-noselect t))
    (forge-topic-setup-buffer
     (forge-get-topic (bookmark-prop-get bookmark 'forge-topic)))))

(put 'forge-discussion-mode 'magit-bookmark-variables t)
(put 'forge-issue-mode      'magit-bookmark-variables t)
(put 'forge-pullreq-mode    'magit-bookmark-variables t)

;;; Headers

(cl-defmacro forge--define-topic-header
    (name &key insert format (command nil command?))
  (declare (indent defun))
  (let ((fun (intern (format "forge-insert-topic-%s" name)))
        (map (intern (format "forge-topic-%s-section-map" name)))
        (cmd (intern (format "forge-topic-set-%s" name))))
    `(progn
       (cl-defun ,fun (&optional (topic forge-buffer-topic))
         (magit-insert-section (,(intern (format "topic-%s" name)))
           (insert ,(capitalize (string-pad (format "%s: " name) 11)))
           ,(cond
             (insert
              `(unless (funcall ,insert topic)
                 (insert (magit--propertize-face "none" 'magit-dimmed))))
             (format
              `(insert (or (funcall ,format topic)
                           (magit--propertize-face "none" 'magit-dimmed)))))
           (insert ?\n)))
       ,@(and (if command? command t)
              `((defvar-keymap ,map "<remap> <magit-edit-thing>"
                               ,(or command `(function ,cmd)))
                (put ',map 'definition-name ',name)))
       (put ',fun 'definition-name ',name))))

(forge--define-topic-header refs
  :command nil
  :format #'forge--format-topic-refs)

(forge--define-topic-header draft
  :command #'forge-topic-toggle-draft
  :format #'forge--format-topic-draft)

(forge--define-topic-header saved
  :command #'forge-topic-toggle-saved
  :format #'forge--format-topic-saved)

(forge--define-topic-header state
  :command #'forge-topic-state-menu
  :format #'forge--format-topic-state)

(forge--define-topic-header status
  :command #'forge-topic-status-menu
  :format #'forge--format-topic-status)

(forge--define-topic-header category
  :format #'forge--format-topic-category)

(forge--define-topic-header milestone
  :format #'forge--format-topic-milestone)

(forge--define-topic-header labels
  :insert #'forge--insert-topic-labels)

(forge--define-topic-header marks
  :insert #'forge--insert-topic-marks)

(forge--define-topic-header assignees
  :format #'forge--format-topic-assignees)

(forge--define-topic-header review-requests
  :format #'forge--format-topic-review-requests)

;;; Commands
;;;; Groups

(defvar forge--show-topic-legend t)

(transient-define-group forge--lists-group
  ["List"
   ("l r" "repositories"  forge-list-repositories)
   ("l n" "notifications" forge-list-notifications)
   ("l g" "global topics" forge-list-global-topics)
   ("l t" "topics"        forge-list-topics)
   ""])

(transient-define-group forge--topic-menus-group
  ["Menu"
   ("m s" "edit"      forge-topic-menu)
   ("m f" "filter"    forge-topics-menu)
   ("m f" "filter"    forge-notifications-menu)
   ("m f" "filter"    forge-repositories-menu)
   ("m d" "dispatch"  forge-dispatch)
   ("m c" "configure" forge-configure)
   """"])

(transient-define-group forge--topic-set-state-group
  [:description (##if forge--show-topic-legend "Set public state" "Set state")
   ("o" forge-topic-state-set-open)
   ("c" forge-chatter-state-set-completed)
   ("U" forge-issue-state-set-unplanned)
   ("O" forge-discussion-state-set-outdated)
   ("D" forge-chatter-state-set-duplicate)
   ("M" forge-pullreq-state-set-merged)
   ("R" forge-pullreq-state-set-rejected)])

(transient-define-group forge--topic-set-status-group
  [:description (##if forge--show-topic-legend "Set private status" "Set status")
   ("u" forge-topic-status-set-unread)
   ("p" forge-topic-status-set-pending)
   ("d" forge-topic-status-set-done)])

(transient-define-group forge--topic-legend-group
  ["Legend" :if-non-nil forge--show-topic-legend
   (:info* (##propertize "open discussion"      'face 'forge-discussion-open))
   (:info* (##propertize "completed discussion" 'face 'forge-discussion-completed))
   (:info* (##propertize "expunged discussion"  'face 'forge-discussion-expunged))]
  ["" :if-non-nil forge--show-topic-legend
   (:info* (##propertize "open issue"       'face 'forge-issue-open))
   (:info* (##propertize "completed issue"  'face 'forge-issue-completed))
   (:info* (##propertize "expunged issue"   'face 'forge-issue-expunged))]
  ["" :if-non-nil forge--show-topic-legend
   (:info* (##propertize "open pullreq"     'face 'forge-pullreq-open))
   (:info* (##propertize "merged pullreq"   'face 'forge-pullreq-merged))
   (:info* (##propertize "rejected pullreq" 'face 'forge-pullreq-rejected))]
  ["" :if-non-nil forge--show-topic-legend
   (:info* (##propertize "unread"           'face 'forge-topic-unread))
   (:info* (##propertize "pending"          'face 'forge-topic-pending))
   (:info* (##propertize "done"             'face 'forge-topic-done))]
  ["" :if-non-nil forge--show-topic-legend
   (:info* (##propertize "draft"            'face 'forge-pullreq-draft))])

(transient-define-suffix forge-toggle-topic-legend ()
  "Toggle whether to show legend for faces used in topic menus and lists."
  :description (##if forge--show-topic-legend "hide legend" "show legend")
  :transient t
  (interactive)
  (customize-set-variable 'forge--show-topic-legend
                          (not forge--show-topic-legend)))

(defconst forge--topic-menus-column-widths '(21 21 21 21))

;;;; Menus

;;;###autoload(autoload 'forge-topic-menu "forge-topic" nil t)
(transient-define-prefix forge-topic-menu ()
  "Edit the topic at point."
  :transient-suffix t
  :transient-non-suffix #'transient--do-call
  :transient-switch-frame nil
  :refresh-suffixes t
  :environment #'forge--menu-environment
  :column-widths forge--topic-menus-column-widths
  [:hide always ("q" forge-menu-quit-list)]
  [forge--topic-menus-group
   forge--topic-set-state-group
   forge--topic-set-status-group
   ["Actions"
    ("/f" forge-pull-this-topic)
    ("/b" forge-browse-this-topic)
    ("/r" "respond" forge-create-post)
    ("/c" forge-checkout-this-pullreq)
    ("/A" forge-approve-pullreq)
    ("/R" forge-request-changes)]]
  [forge--lists-group
   ["Set                                         "
    ("-c" forge-topic-set-category)
    ("-m" forge-topic-set-milestone)
    ("-l" forge-topic-set-labels)
    ("-x" forge-topic-set-marks)
    ("-a" forge-topic-set-assignees)
    ("-r" forge-topic-set-review-requests)
    ("-n" forge-edit-topic-note)
    ("-t" forge-topic-set-title)]
   ["Set"
    ("-s" forge-topic-toggle-saved)
    ("-d" forge-topic-toggle-draft)
    ("-A" forge-discussion-set-answer)
    """Display"
    ("-H" forge-toggle-topic-legend)]]
  [forge--topic-legend-group])

(transient-augment-suffix forge-topic-menu
  :transient #'transient--do-replace
  :inapt-if (lambda () (or (derived-mode-p 'forge-repository-list-mode)
                      (eq (oref transient--prefix command) 'forge-topic-menu)))
  :inapt-face (lambda () (if (derived-mode-p 'forge-repository-list-mode)
                        'transient-inapt-suffix
                      'forge-suffix-active)))

;;;###autoload(autoload 'forge-topic-state-menu "forge-topic" nil t)
(transient-define-prefix forge-topic-state-menu ()
  "Set state of the current topic."
  :environment #'forge--menu-environment
  [forge--topic-set-state-group])

;;;###autoload(autoload 'forge-topic-status-menu "forge-topic" nil t)
(transient-define-prefix forge-topic-status-menu ()
  "Set status of the current topic."
  :environment #'forge--menu-environment
  [forge--topic-set-status-group])

(defun forge--menu-environment (fn)
  (let ((magit--refresh-cache (list (cons 0 0))))
    (funcall fn)))

;;;; State

(defclass forge--topic-set-state-command (transient-suffix)
  ((state :initarg :state)
   (getter :initarg :getter)
   (definition
    :initform (lambda ()
                (interactive)
                (with-slots (getter state) (transient-suffix-object)
                  (let ((topic (funcall getter t)))
                    (forge--set-topic-state (forge-get-repository topic)
                                            topic state)))))
   (description
    :initform (lambda (obj)
                (symbol-name (oref obj state))))
   (inapt-if
    :initform (lambda ()
                (or (forge-region-topics)
                    (with-slots (getter state) (transient-suffix-object)
                      (if-let ((topic (funcall getter)))
                          ;; Once a pull-request is merged,
                          ;; its state cannot be changed anymore.
                          (memq (oref topic state) (list state 'merged))
                        t)))))
   (inapt-face
    :initform (lambda (obj)
                (with-slots (getter state) (transient-suffix-object)
                  (if (and-let ((_(not (forge-region-topics)))
                                (topic (funcall getter)))
                        (eq (oref topic state) state))
                      'forge-suffix-active
                    'transient-inapt-suffix))))))

(transient-define-suffix forge-topic-state-set-open ()
  "Set the state of the current topic to `open'."
  :class 'forge--topic-set-state-command
  :state 'open
  :getter #'forge-current-topic)

(transient-define-suffix forge-chatter-state-set-completed ()
  "Set the state of the current discussion or issue to `completed'."
  :class 'forge--topic-set-state-command
  :state 'completed
  :getter #'forge-current-chatter
  :if #'forge-current-chatter)

(transient-define-suffix forge-issue-state-set-unplanned ()
  "Set the state of the current issue to `unplanned'."
  :class 'forge--topic-set-state-command
  :state 'unplanned
  :getter #'forge-current-issue
  :if #'forge-current-issue)

(transient-define-suffix forge-chatter-state-set-duplicate ()
  "Set the state of the current discussion or issue to `duplicate'."
  :class 'forge--topic-set-state-command
  :state 'duplicate
  :getter #'forge-current-chatter
  :if #'forge-current-chatter
  (interactive)
  (with-slots (getter state) (transient-suffix-object)
    (let ((topic (funcall getter t)))
      (if (forge-issue-p topic)
          (message
           "The API does not yet support closing an issue as a duplicate")
        (forge--set-topic-state (forge-get-repository topic)
                                topic state)))))

(transient-define-suffix forge-discussion-state-set-outdated ()
  "Set the state of the current discussion to `outdated'."
  :class 'forge--topic-set-state-command
  :state 'outdated
  :getter #'forge-current-discussion
  :if #'forge-current-discussion)

(transient-define-suffix forge-pullreq-state-set-merged ()
  "Merge the current pull-request into its target.
Prompt the user to either use the API to perform the merge or use Git.
I recommend you only use the API if your organization enforces that
inferior process."
  :class 'forge--topic-set-state-command
  :state 'merged
  :getter #'forge-current-pullreq
  :if #'forge-current-pullreq
  :transient nil
  (interactive)
  (let ((pullreq (forge-current-pullreq)))
    (if (magit-read-char-case (format "Merge #%s " (oref pullreq number)) t
          (?g "using [g]it (recommended)" t)
          (?a "using [a]pi" nil))
        (let ((branch (or (forge--pullreq-branch-active pullreq)
                          (forge--branch-pullreq pullreq))))
          (if-let ((upstream (magit-get-local-upstream-branch branch)))
              (if (zerop (magit-call-git "checkout" upstream))
                  (magit--merge-absorb
                   branch (magit-merge-arguments)
                   ;; Users might be surprised that we
                   ;; aren't done yet, so drop a hint.
                   "Inspect the result, and if satisfied push")
                (user-error "Could not checkout %S" upstream))
            (user-error "No upstream configured for %S" branch)))
      (forge-merge pullreq (forge-select-merge-method)))))

(transient-define-suffix forge-pullreq-state-set-rejected ()
  "Set the state of the current pull-request to `rejected'."
  :class 'forge--topic-set-state-command
  :state 'rejected
  :getter #'forge-current-pullreq
  :if #'forge-current-pullreq)

;;;; Status

(defclass forge--topic-set-status-command (transient-suffix)
  ((status :initarg :status)
   (definition
    :initform (lambda ()
                (interactive)
                (with-slots (status) (transient-suffix-object)
                  (if-let ((topics (forge-region-topics)))
                      (dolist (topic topics)
                        (oset topic status status))
                    (oset (forge-current-topic t) status status)))
                (forge-refresh-buffer)))
   (description
    :initform (lambda (obj)
                (symbol-name (oref obj status))))
   (inapt-if
    :initform (lambda ()
                (and (not (forge-region-topics))
                     (if-let ((topic (forge-current-topic)))
                         (eq (oref topic status)
                             (oref (transient-suffix-object) status))
                       t))))
   (inapt-face
    :initform (lambda ()
                (if (forge-current-topic)
                    'forge-suffix-active
                  'transient-inapt-suffix)))))

(transient-define-suffix forge-topic-status-set-unread ()
  "Set the notification status of the current topic to `unread'."
  :class 'forge--topic-set-status-command :status 'unread)

(transient-define-suffix forge-topic-status-set-pending ()
  "Set the notification status of the current topic to `pending'."
  :class 'forge--topic-set-status-command :status 'pending)

(transient-define-suffix forge-topic-status-set-done ()
  "Set the notification status of the current topic to `done'."
  :class 'forge--topic-set-status-command :status 'done)

;;;; Set

(defclass forge--topic-set-slot-command (transient-suffix)
  ((slot :initarg :slot)
   (setter)
   (reader :initarg :reader)
   (formatter :initarg :formatter)
   (definition
    :initform (lambda (value)
                (interactive
                 (list (funcall (oref (transient-suffix-object) reader)
                                (forge-current-topic t))))
                (let ((topic (forge-current-topic t)))
                  (funcall (oref (transient-suffix-object) setter)
                           (forge-get-repository topic)
                           topic value))))
   (description
    :initform (lambda (obj)
                (with-slots (slot inapt-if-not) obj
                  (if-let* ((topic (if inapt-if-not
                                       (funcall inapt-if-not)
                                     (forge-current-topic)))
                            (value (funcall (oref obj formatter) topic)))
                      (format "%s %s" slot value)
                    (format "%s" slot)))))))

(cl-defmethod initialize-instance :after
  ((obj forge--topic-set-slot-command) &optional _slots)
  (with-slots (slot) obj
    (let ((name (symbol-name slot)))
      (cond ((slot-boundp obj 'reader))
            ((string-suffix-p "-p" name)
             (setq name (substring name 0 -2))
             (oset obj reader (##not (eieio-oref % slot))))
            ((oset obj reader (intern (format "forge-read-topic-%s" name)))))
      (oset obj setter (intern (format "forge--set-topic-%s" name)))
      (unless (slot-boundp obj 'formatter)
        (oset obj formatter (intern (format "forge--format-topic-%s" name)))))))

(transient-define-suffix forge-topic-set-title (title)
  "Edit the TITLE of the current topic."
  :class 'forge--topic-set-slot-command :slot 'title
  :inapt-if-not #'forge-current-topic
  :formatter (lambda (topic)
               (propertize (truncate-string-to-width
                            (forge--format-topic-title topic) 34 nil ?\s t)
                           'face 'font-lock-string-face)))

(transient-define-suffix forge-topic-set-category (category)
  "Edit the CATEGORY of the current discussion."
  :class 'forge--topic-set-slot-command :slot 'category
  :inapt-if-not #'forge-current-discussion)

(transient-define-suffix forge-topic-set-milestone (milestone)
  "Edit what MILESTONE the current topic belongs to."
  :class 'forge--topic-set-slot-command :slot 'milestone
  :inapt-if-not (##or (forge-current-issue) (forge-current-pullreq)))

(transient-define-suffix forge-topic-set-labels (labels)
  "Edit the LABELS of the current topic."
  :class 'forge--topic-set-slot-command :slot 'labels
  :inapt-if-not #'forge-current-topic
  :formatter (##forge--format-labels % t))

(transient-define-suffix forge-topic-set-marks (marks)
  "Edit the MARKS of the current topic."
  :class 'forge--topic-set-slot-command :slot 'marks
  :inapt-if-not #'forge-current-topic
  :formatter (##forge--format-marks % t))

(transient-define-suffix forge-topic-set-assignees (assignees)
  "Edit the ASSIGNEES of the current topic."
  :class 'forge--topic-set-slot-command :slot 'assignees
  :inapt-if-not (##or (forge-current-issue) (forge-current-pullreq)))

(transient-define-suffix forge-topic-set-review-requests (review-requests)
  "Edit the REVIEW-REQUESTS of the current pull-request."
  :class 'forge--topic-set-slot-command :slot 'review-requests
  :inapt-if-not #'forge-current-pullreq)

(transient-define-suffix forge-topic-toggle-draft (draft)
  "Toggle whether the current pull-request is a draft."
  :class 'forge--topic-set-slot-command :slot 'draft-p
  :inapt-if-not #'forge-current-pullreq
  :description (##forge--format-boolean 'draft-p "draft"))

(transient-define-suffix forge-topic-toggle-saved ()
  "Toggle whether this topic is marked as saved."
  :class 'forge--topic-set-slot-command :slot 'saved-p
  :inapt-if-not #'forge-current-topic
  :description (##forge--format-boolean 'saved-p "saved")
  ;; Set only locally because Github's API does not support this.
  (interactive)
  (let ((topic (forge-current-topic t)))
    (oset topic saved-p (not (oref topic saved-p))))
  (forge-refresh-buffer))

(transient-define-suffix forge-discussion-set-answer (post)
  "Mark the post at point as the answer to the current question.
When point is on the answer, then unmark it and mark no other."
  :class 'forge--topic-set-slot-command :slot 'answer
  :inapt-if-not (lambda ()
                  (and-let* ((discussion (forge-current-discussion))
                             (category (oref discussion category)))
                    (forge-sql1 [:select answerable-p
                                 :from discussion-category
                                 :where (= id $s1)]
                                category)))
  :description (##forge--format-boolean 'answer "answered")
  :reader #'forge--select-discussion-answer)

;;; Color Utilities

(defun forge--sanitize-color (color)
  (cond ((color-values color) color)
        ;; Discard alpha information.
        ((string-match-p "\\`#.\\{4\\}\\'" color) (substring color 0 3))
        ((string-match-p "\\`#.\\{8\\}\\'" color) (substring color 0 6))
        (t "#000000"))) ; Use fallback instead of invalid color.

;;; Markdown Utilities

(defun forge--fontify-markdown (text &optional indent)
  (with-temp-buffer
    (delay-mode-hooks
      (gfm-mode))
    (insert text)
    (font-lock-ensure)
    (when forge-post-fill-region
      (when indent
        (setq fill-column (- fill-column indent)))
      (fill-region (point-min) (point-max)))
    (when indent
      (indent-rigidly (point-min) (point-max) indent))
    (let* ((string (buffer-string))
           (beg 0)
           (end (length string)))
      (while (< beg end)
        (let ((pos (next-single-property-change beg 'face string end))
              (val (get-text-property beg 'face string)))
          (put-text-property beg pos 'font-lock-face val string)
          (remove-list-of-text-properties beg pos '(face) string)
          (setq beg pos)))
      string)))

(defun forge--markdown-translate-filename-function (file)
  (if (string-match-p "\\`https?://" file)
      file
    (let ((host (oref (forge-get-repository :tracked) forge))) ;aka webhost
      (concat (if (member host ghub-insecure-hosts) "http://" "https://")
              host
              (and (not (string-prefix-p "/" file)) "/")
              file))))

;;; Templates

(defun forge--topic-template (repo class)
  (unless repo
    (setq repo (forge-get-repository :tracked)))
  (let* ((templates (forge--topic-templates repo class))
         (template
          (if (cdr templates)
              (let ((c (magit-completing-read
                        (pcase class
                          ('forge-issue   "Select issue template")
                          ('forge-pullreq "Select pull-request template"))
                        (mapcar (##alist-get 'prompt %) templates)
                        nil t)))
                (seq-find (##equal (alist-get 'prompt %) c) templates))
            (car templates))))
    (if-let ((url (alist-get 'url template)))
        (if (string-match (forge--format repo "\
\\`https://%h/[^/]+/[^/]+/discussions\\(?:/categories/\\(.+\\)\\)?")
                          url)
            `((type . forge-discussion)
              (category . ,(or (match-string 1 url)
                               (forge-read-topic-category
                                nil "Category for new discussion"))))
          `((type . redirect) ,@template))
      `((type . ,class) ,@template))))

(defun forge--topic-templates (repo class)
  (mapcan (lambda (file)
            (with-temp-buffer
              (magit-git-insert "cat-file" "-p" file)
              (if (equal (file-name-nondirectory file) "config.yml")
                  (forge--topic-parse-template-config)
                (list (forge--topic-parse-template (file-name-base file))))))
          (forge--topic-template-files repo class)))

(cl-defgeneric forge--topic-template-files (repo class))

(defun forge--topic-template-files-1 (repo suffix &rest paths)
  (setq suffix (ensure-list suffix))
  (let ((branch (forge--get-default-branch repo)))
    (seq-keep (if suffix
                  (##and (member (file-name-extension %) suffix)
                         (concat branch ":" %))
                (##concat branch ":" %))
              (magit-git-items "ls-tree" "-z"
                               "--full-tree" "--name-only"
                               (and suffix "-r")
                               branch "--" paths))))

(defun forge--topic-parse-template-config ()
  (let-alist (yaml-parse-string (buffer-str)
                                :object-type 'alist
                                :sequence-type 'list)
    (nconc
     (and (not (eq .blank_issues_enabled :false)) ;unset means true
          `(((prompt . ,(concat (propertize "Blank issue" 'face 'bold)
                                " — Create a new issue from scratch")))))
     (mapcar (lambda (link)
               `(,@link
                 (prompt . ,(let-alist link
                              (concat (propertize .name 'face 'bold)
                                      " — " .about)))))
             .contact_links))))

(defun forge--topic-parse-template (name)
  (goto-char (point-min))
  (skip-chars-forward "\s\t\n\r")
  (if-let ((beg (and (looking-at "^---[\s\t]*$")
                     (point)))
           (end (and (zerop (forward-line))
                     (re-search-forward "^---[\s\t]*$" nil t)
                     (match-beginning 0)))
           (repoid (oref (forge-get-repository :tracked) id)))
      (let-alist (yaml-parse-string (buffer-str beg end)
                                    :object-type 'alist
                                    :sequence-type 'list
                                    :null-object nil
                                    :false-object nil)
        (when (stringp .name)
          (setq name .name))
        (setq name (propertize .name 'face 'bold))
        `((prompt    . ,(if .about (format "%s — %s" name .about) name))
          (title     . ,(and .title
                             (stringp .title)
                             (string-trim .title)))
          (text      . ,(string-trim (buffer-str (point))))
          ;; Prevent ad hock creation or previously unknown labels.
          (labels    . ,(cl-intersection
                         (ensure-list .labels)
                         (forge-sql-car [:select name :from label
                                         :where (= repository $s1)]
                                        repoid)
                         :test #'equal))
          ;; Server errors on invalid assignees.
          (assignees . ,(cl-intersection
                         (ensure-list .assignees)
                         (forge-sql-car [:select login :from assignee
                                         :where (= repository $s1)]
                                        repoid)
                         :test #'equal))
          (draft     . ,(and (booleanp .draft) .draft))))
    `((prompt . ,(propertize name 'face 'bold))
      (text   . ,(string-trim (buffer-str))))))

;;; Bug-Reference

(defvar forge-bug-reference-remote-files t
  "Whether forge may enable `bug-reference-mode' in remote files.
See also `forge-bug-reference-setup'.")

(defun forge-bug-reference-setup ()
  "Setup `bug-reference' in the current buffer.
If forge data has been fetched for the current repository, then
enable `bug-reference-mode' or `bug-reference-prog-mode' and
modify `bug-reference-bug-regexp' if appropriate."
  (unless (or bug-reference-url-format
              (not (forge-db t))
              (and buffer-file-name
                   (not forge-bug-reference-remote-files)
                   (file-remote-p buffer-file-name))
              ;; TODO Allow use in these modes again.
              (derived-mode-p 'forge-topics-mode 'forge-notifications-mode))
    (magit--with-safe-default-directory nil
      (when-let ((repo (forge-get-repository :tracked?)))
        (when (derived-mode-p 'magit-status-mode
                              'forge-notifications-mode)
          (setq-local
           bug-reference-auto-setup-functions
           (let ((hook bug-reference-auto-setup-functions))
             (list (lambda ()
                     ;; HOOK is not allowed to be a lexical var:
                     ;; (run-hook-with-args-until-success 'hook)
                     (catch 'success
                       (dolist (f hook)
                         (when (funcall f)
                           (setq bug-reference-bug-regexp
                                 (concat "." bug-reference-bug-regexp))
                           (throw 'success t)))))))))
        (if (derived-mode-p 'prog-mode)
            (bug-reference-prog-mode 1)
          (bug-reference-mode 1))
        (add-hook 'completion-at-point-functions
                  #'forge-topic-completion-at-point nil t)))))

(unless noninteractive
  (dolist (hook forge-bug-reference-hooks)
    (add-hook hook #'forge-bug-reference-setup t)))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"          . "cond-let--and$")
;;   ("and-let"       . "cond-let--and-let")
;;   ("if-let"        . "cond-let--if-let")
;;   ("when-let"      . "cond-let--when-let")
;;   ("buffer-string" . "buffer-string")
;;   ("buffer-str"    . "forge--buffer-substring-no-properties")
;;   ("partial"       . "llama--left-apply-partially"))
;; End:
(provide 'forge-topic)
;;; forge-topic.el ends here
