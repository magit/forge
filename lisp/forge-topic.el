;;; forge-topic.el --- Topics support  -*- lexical-binding:t -*-

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

(require 'bug-reference)
(require 'markdown-mode)
(require 'parse-time)
(require 'yaml)

(require 'forge)
(require 'forge-post)

(defvar bug-reference-auto-setup-functions)

;;; Options

(defcustom forge-topic-list-order '(updated . string>)
  "Order of topics listed in the status buffer.

The value has the form (SLOT . PREDICATE), where SLOT is a
slot of issue or pullreq objects, and PREDICATE is a function
used to order the topics by that slot.  Reasonable values
include (number . >) and (updated . string>)."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type '(cons (symbol   :tag "Slot")
               (function :tag "Predicate")))

(defcustom forge-topic-list-limit '(60 . 5)
  "Limit the number of topics listed in the status buffer.

All unread topics are always shown.  If the value of this option
has the form (OPEN . CLOSED), then the integer OPEN specifies the
maximal number of topics and CLOSED specifies the maximal number
of closed topics.  IF CLOSED is negative then show no closed
topics until the command `forge-toggle-closed-visibility' changes
the sign.

The value can also be an integer, in which case it limits the
number of closed topics only."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type '(choice (number :tag "Maximal number of closed topics")
                 (cons (number :tag "Maximal number of open topics")
                       (number :tag "Maximal number of closed topics"))))

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

(defvar-local forge-display-in-status-buffer t
  "Whether to display topics in the current Magit status buffer.")
(put 'forge-display-in-status-buffer 'permanent-local t)

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
  `((t :inherit magit-header-line
       ,@(and (>= emacs-major-version 29) '(:foreground reset))))
  "Face for the `header-line' in `forge-topic-mode' buffers."
  :group 'forge-faces)

;;;; Topic and Notification Slugs

(defface forge-topic-slug-open
  '((t :inherit forge-dimmed))
  "Face uses for slugs of open topics."
  :group 'forge-faces)

(defface forge-topic-slug-completed
  '((t :inherit forge-dimmed))
  "Face used for slugs of completed topics."
  :group 'forge-faces)

(defface forge-topic-slug-unplanned
  '((t :inherit forge-dimmed :strike-through t))
  "Face used for slugs of unplanned topics.
E.g., for issues closes as \"unplanned\" and pull-requests that
were closed without being merged."
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
  `((t :weight bold
       :box ( :line-width ,(if (>= emacs-major-version 28) (cons -1 -1) -1)
              :style nil)))
  "Face used for summaries of entities with unread notifications.
This face is always used together with, and takes preference
over, a `forge[-fancy]-{issue,pullreq}-STATE' face and should not
specify any attribute that is specified by any of those faces.
Likewise those faces should not set `:weight' or `:slant'."
  :group 'forge-faces)

(defface forge-topic-pending
  '((t :weight bold))
  "Face used for summaries of entities with open notifications.
This face is always used together with, and takes preference
over, a `forge[-fancy]-{issue,pullreq}-STATE' face and should not
specify any attribute that is specified by any of those faces.
Likewise those faces should not set `:weight' or `:slant'."
  :group 'forge-faces)

(defface forge-topic-done
  '((t :slant italic))
  "Face used for summaries of entities with no unread or open notification.
This face is always used together with, and takes preference
over, a `forge[-fancy]-{issue,pullreq}-STATE' face and should not
specify any attribute that is specified by any of those faces.
Likewise those faces should not set `:weight' or `:slant'."
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

(defface forge-issue-unplanned
  '((t :inherit forge-dimmed :strike-through t))
  "Face used for summaries of issues closed as unplanned."
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

;;;; Labels

(defface forge-topic-label
  `((t :box ( :line-width ,(if (>= emacs-major-version 28) (cons -1 -1) -1)
              :style released-button)))
  "Face used for topic labels."
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
   t))

(cl-defmethod forge-topic-mark-read ((topic forge-topic))
  (when (eq (oref topic status) 'unread)
    (oset topic status 'pending)))

(cl-defmethod forge--set-topic-marks ((_repo forge-repository) topic marks)
  (oset topic marks
        (mapcar #'car (forge-sql [:select id :from mark :where (in name $v1)]
                                 (vconcat marks))))
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
  (if (numberp number-or-id)
      (if (< number-or-id 0)
          (forge-get-pullreq repo (abs number-or-id))
        (or (forge-get-issue repo number-or-id)
            (forge-get-pullreq repo number-or-id)))
    (or (forge-get-issue number-or-id)
        (forge-get-pullreq number-or-id))))

(cl-defmethod forge-get-topic ((number integer))
  (if (< number 0)
      (forge-get-pullreq (abs number))
    (or (forge-get-issue number)
        (forge-get-pullreq number))))

(cl-defmethod forge-get-topic ((id string))
  (or (forge-get-issue id)
      (forge-get-pullreq id)))

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
an error.  If NOT-THINGATPT is non-nil, then don't use
`thing-at-point'."
  (or (thing-at-point 'forge-topic)
      (magit-section-value-if '(issue pullreq))
      (forge-get-pullreq :branch)
      (and (derived-mode-p 'forge-topic-list-mode)
           (and-let* ((id (tabulated-list-get-id)))
             (forge-get-topic id)))
      (and demand (user-error "No topic at point"))))

(put 'forge-topic 'thing-at-point #'forge-thingatpt--topic)
(defun forge-thingatpt--topic ()
  (and-let* ((repo (forge--repo-for-thingatpt)))
    (and (thing-at-point-looking-at
          (if (forge-gitlab-repository--eieio-childp repo)
              "\\(?2:[#!]\\)\\(?1:[0-9]+\\)\\_>"
            "#\\([0-9]+\\)\\_>"))
         (funcall (if (equal (match-string 2) "!")
                      #'forge-get-pullreq
                    #'forge-get-topic)
                  repo (string-to-number (match-string-no-properties 1))))))

(defun forge-region-topics ()
  (cond
   ((derived-mode-p 'forge-notifications-mode)
    (magit-region-values '(issue pullreq)))
   ((and (derived-mode-p 'forge-topic-list-mode)
         (region-active-p))
    (let ((beg (region-beginning))
          (end (region-end))
          (topics nil))
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (when-let* ((id (tabulated-list-get-id)))
            (push (forge-get-topic id) topics))
          (forward-line 1))
        (nreverse topics))))))

;;;; List

(defun forge-ls-recent-topics (repo table)
  (let* ((id (oref repo id))
         (limit forge-topic-list-limit)
         (open-limit   (if (consp limit) (car limit) limit))
         (closed-limit (if (consp limit) (cdr limit) limit))
         (topics (forge-sql [:select * :from $i1
                             :where (and (= repository $s2)
                                         (= status 'unread))]
                            table id)))
    (mapc (lambda (row)
            (cl-pushnew row topics :test #'equal))
          (if (consp limit)
              (forge-sql [:select * :from $i1
                          :where (and (= repository $s2)
                                      (isnull closed))
                          :order-by [(desc updated)]
                          :limit $s3]
                         table id open-limit)
            (forge-sql [:select * :from $i1
                        :where (and (= repository $s2)
                                    (isnull closed))]
                       table id)))
    (when (> closed-limit 0)
      (mapc (lambda (row)
              (cl-pushnew row topics :test #'equal))
            (forge-sql [:select * :from $i1
                        :where (and (= repository $s2)
                                    (notnull closed))
                        :order-by [(desc updated)]
                        :limit $s3]
                       table id closed-limit)))
    (cl-sort (mapcar (let ((class (if (eq table 'pullreq)
                                      'forge-pullreq
                                    'forge-issue)))
                       (lambda (row)
                         (closql--remake-instance class (forge-db) row)))
                     topics)
             (cdr forge-topic-list-order)
             :key (lambda (it) (eieio-oref it (car forge-topic-list-order))))))

(defun forge--ls-topics (repo)
  (cl-sort (nconc (forge--ls-issues repo)
                  (forge--ls-pullreqs repo))
           #'> :key (-cut oref <> number)))

(defun forge--ls-active-topics (repo)
  (cl-sort (nconc (forge--ls-active-issues repo)
                  (forge--ls-active-pullreqs repo))
           #'> :key (-cut oref <> number)))

;;; Read

(defun forge-read-topic (prompt)
  "Read an active topic with completion using PROMPT.

Open, unread and pending topics are considered active.
Default to the current topic even if it isn't active.

\\<forge-read-topic-minibuffer-map>While completion is in \
progress, \\[forge-read-topic-lift-limit] lifts the limit, extending
the completion candidates to include all topics.

If `forge-limit-topic-choices' is nil, then all candidates
can be selected from the start."
  (forge--read-topic prompt
                     #'forge-current-topic
                     #'forge--ls-active-topics
                     #'forge--ls-topics))

(defun forge--read-topic (prompt current active all)
  (let* ((current (funcall current))
         (repo    (forge-get-repository (or current :tracked)))
         (default (and current (forge--format-topic-line current)))
         (alist   (forge--topic-collection
                   (funcall (if forge-limit-topic-choices active all)
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
                        (setq alist (forge--topic-collection (funcall all repo)))
                        (setq all-choices (mapcar #'car alist)))))))
                 nil t nil nil default))
            (magit-completing-read prompt choices nil t nil nil default))))
    (cdr (assoc choice alist))))

(defun forge--topic-collection (topics)
  (mapcar (lambda (topic)
            (cons (forge--format-topic-line topic)
                  (oref topic id)))
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
                                     :from pullreq
                                     :where (= repository $s1)
                                     :union
                                     :select [number title updated]
                                     :from issue
                                     :where (= repository $s1)
                                     :order-by [(desc updated)]]
                                    (oref repo id))))
               :annotation-function (lambda (c) (get-text-property 0 :title c))))))

(defun forge-read-topic-title (topic)
  (read-string "Title: " (oref topic title)))

(defun forge-read-topic-milestone (&optional topic)
  (forge--completing-read
   "Milestone"
   (mapcar #'caddr (oref (forge-get-repository (or topic :tracked)) milestones))
   nil t
   (and topic (forge--format-topic-milestone topic))))

(defun forge-read-topic-label (&optional prompt repository)
  (magit-completing-read (or prompt "Label")
                         (forge--format-topic-label-choices
                          (or repository (forge-get-repository :tracked)))
                         nil t))

(defun forge-read-topic-labels (&optional topic)
  (let* ((repo (forge-get-repository (or topic :tracked)))
         (crm-separator ","))
    (magit-completing-read-multiple
     "Labels: "
     (mapcar #'cadr (oref repo labels))
     nil t
     (and topic (mapconcat #'car (closql--iref topic 'labels) ",")))))

(defun forge-read-topic-marks (&optional topic)
  (let ((marks (mapcar #'car (forge-sql [:select name :from mark])))
        (crm-separator ","))
    (magit-completing-read-multiple
     "Marks: " marks nil t
     (and topic (mapconcat #'car (closql--iref topic 'marks) ",")))))

(defun forge-read-topic-assignees (&optional topic)
  (let* ((repo (forge-get-repository (or topic :tracked)))
         (value (and topic (closql--iref topic 'assignees)))
         (choices (mapcar #'cadr (oref repo assignees)))
         (crm-separator ","))
    (magit-completing-read-multiple
     "Assignees: " choices nil
     (if (forge--childp repo 'forge-gitlab-repository)
         t ; Selecting something else would fail later on.
       'confirm)
     (mapconcat #'car value ","))))

(defun forge-read-topic-review-requests (&optional topic)
  (let* ((repo (forge-get-repository (or topic :tracked)))
         (value (and topic (closql--iref topic 'review-requests)))
         (choices (mapcar #'cadr (oref repo assignees)))
         (crm-separator ","))
    (magit-completing-read-multiple
     "Request review from: " choices nil
     'confirm
     (mapconcat #'car value ","))))

(defun forge--completing-read ( prompt collection &optional
                                predicate require-match initial-input
                                hist def)
  ;; NOTE Only required until `magit-completing-read' has been
  ;; updated to allow empty input if require-match is t.
  (let ((reply (funcall magit-completing-read-function
                        (concat prompt ": ")
                        (if (and def (not (member def collection)))
                            (cons def collection)
                          collection)
                        predicate
                        require-match initial-input hist def)))
    (if (equal reply "")
        (if (and require-match (not (eq require-match t)))
            (user-error "Nothing selected")
          nil)
      reply)))

;;; Format

(cl-defmethod forge--format ((topic forge-topic) slot &optional spec)
  (forge--format (forge-get-repository topic) slot
                 `(,@spec (?i . ,(oref topic number)))))

(defun forge--format-topic-line (topic &optional width)
  (concat
   (and (derived-mode-p 'forge-notifications-mode)
        (eq forge-notifications-display-style 'flat)
        (concat (truncate-string-to-width
                 (oref (forge-get-repository topic) slug)
                 forge-notifications-repo-slug-width
                 nil ?\s t)
                " "))
   ;; MAYBE bring this back once we support discussions.
   ;; (cond (no-indicator nil)
   ;;       ((forge-issue-p   topic) (magit--propertize-face "I " 'magit-dimmed))
   ;;       ((forge-pullreq-p topic) (magit--propertize-face "P " 'magit-dimmed))
   ;;       (t                       (magit--propertize-face "* " 'error)))
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
          ('open                     'forge-topic-slug-open)
          ((or 'completed 'merged)   'forge-topic-slug-completed)
          ((or 'unplanned 'rejected) 'forge-topic-slug-unplanned))))))

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
  (with-slots (title status state) topic
    (magit-log-propertize-keywords
     nil
     (magit--propertize-face
      title
      `(,(pcase status
           ('unread  'forge-topic-unread)
           ('pending 'forge-topic-pending)
           ('done    'forge-topic-done))
        ,(pcase (list (eieio-object-class topic) state)
           (`(forge-issue   open)      'forge-issue-open)
           (`(forge-issue   completed) 'forge-issue-completed)
           (`(forge-issue   unplanned) 'forge-issue-unplanned)
           (`(forge-pullreq open)      'forge-pullreq-open)
           (`(forge-pullreq merged)    'forge-pullreq-merged)
           (`(forge-pullreq rejected)  'forge-pullreq-rejected)))))))

(defun forge--format-topic-title+labels (topic)
  (concat (forge--format-topic-title  topic) " "
          (forge--format-topic-labels topic)))

(defun forge--format-topic-milestone (topic)
  (and-let* ((id (oref topic milestone)))
    (caar (forge-sql [:select [title] :from milestone :where (= id $s1)] id))))

(defun forge--format-topic-labels (topic)
  (and-let* ((labels (closql--iref topic 'labels)))
    (mapconcat (pcase-lambda (`(,name ,color ,_description))
                 (let* ((background (forge--sanitize-color color))
                        (foreground (forge--contrast-color background)))
                   (magit--propertize-face
                    name `(( :background ,background
                             :foreground ,foreground)
                           forge-tablist-topic-label))))
               labels " ")))

(defun forge--format-topic-label-choices (repo)
  (mapcar (pcase-lambda (`(,_id ,name ,color ,_description))
            (let* ((background (forge--sanitize-color color))
                   (foreground (forge--contrast-color background)))
              (magit--propertize-face
               name `( :background ,background
                       :foreground ,foreground))))
          (oref repo labels)))

(defun forge--format-topic-marks (topic)
  (and-let* ((marks (closql--iref topic 'marks)))
    (mapconcat (pcase-lambda (`(,name ,face ,_description))
                 (magit--propertize-face
                  name (list 'forge-tablist-topic-label face)))
               marks " ")))

(defun forge--format-topic-state (topic)
  (with-slots (state) topic
    (magit--propertize-face
     (symbol-name state)
     (pcase (list (if (forge-issue-p topic) 'issue 'pullreq) state)
       ('(issue   open)      'forge-issue-open)
       ('(issue   closed)    'forge-issue-completed)
       ('(issue   completed) 'forge-issue-completed)
       ('(issue   unplanned) 'forge-issue-unplanned)
       ('(pullreq open)      'forge-pullreq-open)
       ('(pullreq merged)    'forge-pullreq-merged)
       ('(pullreq closed)    'forge-pullreq-rejected)))))

(defun forge--format-topic-status (topic)
  (with-slots (status) topic
    (magit--propertize-face
     (symbol-name status)
     (pcase status
       ('unread  'forge-topic-unread)
       ('pending 'forge-topic-pending)
       ('done    'forge-topic-done)))))

(defun forge--format-topic-assignees (topic)
  (and-let* ((assignees (closql--iref topic 'assignees)))
    (mapconcat #'forge--format-person assignees ", ")))

(defun forge--format-topic-review-requests (topic)
  (and-let* ((review-requests (closql--iref topic 'review-requests)))
    (mapconcat #'forge--format-person review-requests ", ")))

(defun forge--format-person (person)
  (pcase-let ((`(,login ,name) person))
    (format "%s%s (@%s)"
            (forge--format-avatar login)
            name login)))

(defun forge--format-avatar (person)
  (if forge-format-avatar-function
      (funcall forge-format-avatar-function person)
    ""))

(defun forge--format-boolean (slot name)
  ;; Booleans are formatted differently in transients and headers.
  ;; Use this to format the (complete) description of suffix commands.
  (let ((topic (forge-current-topic)))
    (if (and topic (slot-exists-p topic slot))
        (format (propertize "[%s]" 'face 'transient-delimiter)
                (propertize name 'face
                            (if (eieio-oref topic slot)
                                'transient-value
                              'transient-inactive-value)))
      (format "[%s]" name))))

;;; Insert

(defun forge--insert-topics (type heading topics)
  (when topics
    (let ((width (apply #'max (--map (length (oref it slug)) topics))))
      (magit-insert-section ((eval type) nil t)
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
     (format-time-string "%s" (parse-iso8601-time-string (oref topic created)))
     t)
    (when (and (slot-exists-p topic 'merged)
               (not (oref topic merged)))
      (magit-insert-heading)
      (magit-insert-section-body
        (forge--insert-pullreq-commits topic)))))

(defun forge--insert-topic-labels (topic &optional separate)
  (and-let* ((labels (closql--iref topic 'labels)))
    (prog1 t
      (pcase-dolist (`(,name ,color ,description) labels)
        (let* ((background (forge--sanitize-color color))
               (foreground (forge--contrast-color background)))
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
  (and-let* ((marks (closql--iref topic 'marks)))
    (prog1 t
      (pcase-dolist (`(,name ,face ,description) marks)
        (if separate (insert " ") (setq separate t))
        (insert name)
        (let ((o (make-overlay (- (point) (length name)) (point))))
          (overlay-put o 'priority 2)
          (overlay-put o 'evaporate t)
          (overlay-put o 'font-lock-face (list face 'forge-topic-label))
          (when description
            (overlay-put o 'help-echo description)))))))

(defun forge--assert-insert-topics-get-repository (&optional issues-p)
  (and (forge-db t)
       (or forge-display-in-status-buffer
           (not (eq major-mode 'magit-status-mode)))
       (and-let* ((repo (forge-get-repository :tracked?)))
         (and (or (not issues-p)
                  (oref repo issues-p))
              repo))))

;;; Modes

(defvar-keymap forge-post-section-map
  "<remap> <magit-edit-thing>"   #'forge-edit-post
  "C-c C-k"                      #'forge-delete-comment)

(defvar-keymap forge-topic-mode-map
  "<remap> <magit-visit-thing>"  #'markdown-follow-link-at-point
  "<mouse-2>"                    #'markdown-follow-link-at-point
  "C-c C-m"                      #'forge-topic-menu
  "C-c C-n"                      #'forge-create-post
  "C-c C-r"                      #'forge-create-post)

(define-derived-mode forge-topic-mode magit-mode "Topic"
  "Parent mode of `forge-{issue,pullreq}-mode'.
This mode itself is never used directly."
  (face-remap-add-relative 'header-line 'forge-topic-header-line)
  (setq-local markdown-translate-filename-function
              #'forge--markdown-translate-filename-function))

(define-derived-mode forge-issue-mode forge-topic-mode "Issue"
  "Mode for looking at a Forge issue.")
(defalias 'forge-issue-setup-buffer   #'forge-topic-setup-buffer)
(defalias 'forge-issue-refresh-buffer #'forge-topic-refresh-buffer)
(defvar forge-issue-headers-hook
  '(forge-insert-topic-state
    forge-insert-topic-status
    forge-insert-topic-milestone
    forge-insert-topic-labels
    forge-insert-topic-marks
    forge-insert-topic-assignees))

(define-derived-mode forge-pullreq-mode forge-topic-mode "Pull-request"
  "Mode for looking at a Forge pull-request.")
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
         (current-repo (forge-get-repository :known?))
         (default-directory (if (forge-repository-equal current-repo repo)
                                default-directory
                              (or (forge-get-worktree repo)
                                  default-directory))))
    (magit-setup-buffer-internal
     (if (forge-issue-p topic) #'forge-issue-mode #'forge-pullreq-mode)
     t `((forge-buffer-topic ,topic)) name)
    (forge-topic-mark-read topic)))

(defun forge-topic-refresh-buffer ()
  (let ((topic (closql-reload forge-buffer-topic)))
    (setq forge-buffer-topic topic)
    (magit-set-header-line-format (forge--format-topic-line topic))
    (magit-insert-section (topicbuf)
      (magit-insert-headers
       (intern (format "%s-headers-hook"
                       (substring (symbol-name major-mode) 0 -5))))
      (when (forge-pullreq-p topic)
        (magit-insert-section (pullreq topic)
          (magit-insert-heading "Commits")
          (forge--insert-pullreq-commits topic t)))
      (when-let ((note (oref topic note)))
        (magit-insert-section (note)
          (magit-insert-heading "Note")
          (insert (forge--fontify-markdown note) "\n\n")))
      (dolist (post (cons topic (oref topic posts)))
        (with-slots (author created body) post
          (magit-insert-section section (post post)
            (oset section heading-highlight-face
                  'magit-diff-hunk-heading-highlight)
            (let ((heading
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
              (font-lock-append-text-property
               0 (length heading)
               'font-lock-face 'magit-diff-hunk-heading heading)
              (magit-insert-heading heading))
            (insert (forge--fontify-markdown body) "\n\n"))))
      (when (and (display-images-p)
                 (fboundp 'markdown-display-inline-images))
        (let ((markdown-display-remote-images t))
          (markdown-display-inline-images))))))

(cl-defmethod magit-buffer-value (&context (major-mode forge-topic-mode))
  (oref forge-buffer-topic slug))

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
                (put ',fun 'definition-name ',name)))
       (put ',map 'definition-name ',name))))

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

(defconst forge--topic-set-state-group
  ["Set state"
   ("s o" forge-topic-state-set-open)
   ("s c" forge-issue-state-set-completed)
   ("s u" forge-issue-state-set-unplanned)
   ("s m" forge-pullreq-state-set-merged)
   ("s r" forge-pullreq-state-set-rejected)
   ""])

(defconst forge--topic-set-status-group
  ["Set status"
   ("s i" forge-topic-status-set-unread)
   ("s p" forge-topic-status-set-pending)
   ("s d" forge-topic-status-set-done)])

(defconst forge--topic-set-slots-group
  ["Set"
   ("e t" forge-topic-set-title)
   ("e m" forge-topic-set-milestone)
   ("e l" forge-topic-set-labels)
   ("e a" forge-topic-set-assignees)
   ("e r" forge-topic-set-review-requests)
   ("e d" forge-topic-toggle-draft)])

(defconst forge--topic-set-local-group
  ["Set local"
   ("l s" forge-topic-toggle-saved)
   ("l m" forge-topic-set-marks)
   ("l n" forge-edit-topic-note)])

;;;; Menus

(defconst forge--topic-menus-column-widths '(19))

;;;###autoload (autoload 'forge-topic-menu "forge-topic" nil t)
(transient-define-prefix forge-topic-menu ()
  "Edit the topic at point."
  :transient-suffix t
  :transient-non-suffix t
  :transient-switch-frame nil
  :refresh-suffixes t
  :column-widths forge--topic-menus-column-widths
  [:hide always
   ("q" forge-menu-quit-list)]
  [["Actions"
    ("f" forge-pull-this-topic)
    ("b" forge-browse-this-topic)
    ("k" forge-delete-comment)
    ("p" forge-create-pullreq-from-issue)
    ("m" "show more actions" forge-dispatch)]]
  [forge--topic-set-state-group
   forge--topic-set-slots-group]
  [forge--topic-set-status-group
   forge--topic-set-local-group]
  (interactive)
  (unless (derived-mode-p 'forge-topic-mode)
    (if-let ((topic (forge-topic-at-point)))
        (forge-topic-setup-buffer topic)
      (user-error "No current topic")))
  (transient-setup 'forge-topic-menu))

;;;###autoload (autoload 'forge-topic-state-menu "forge-topic" nil t)
(transient-define-prefix forge-topic-state-menu ()
  "Set state of the current topic."
  [("o" forge-topic-state-set-open)
   ("c" forge-issue-state-set-completed)
   ("u" forge-issue-state-set-unplanned)
   ("m" forge-pullreq-state-set-merged)
   ("r" forge-pullreq-state-set-rejected)])

;;;###autoload (autoload 'forge-topic-status-menu "forge-topic" nil t)
(transient-define-prefix forge-topic-status-menu ()
  "Set status of the current topic."
  [("i" forge-topic-status-set-unread)
   ("p" forge-topic-status-set-pending)
   ("d" forge-topic-status-set-done)])

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
                  (if (and (not (forge-region-topics))
                           (and-let* ((topic (funcall getter)))
                             (eq (oref topic state) state)))
                      'forge-suffix-active
                    'transient-inapt-suffix))))))

(transient-define-suffix forge-topic-state-set-open ()
  "Set the state of the current topic to `open'."
  :class 'forge--topic-set-state-command
  :state 'open
  :getter #'forge-current-topic)

(transient-define-suffix forge-issue-state-set-completed ()
  "Set the state of the current issue to `completed'."
  :class 'forge--topic-set-state-command
  :state 'completed
  :getter #'forge-current-issue)

(transient-define-suffix forge-issue-state-set-unplanned ()
  "Set the state of the current issue to `unplanned'."
  :class 'forge--topic-set-state-command
  :state 'unplanned
  :getter #'forge-current-issue)

(transient-define-suffix forge-pullreq-state-set-merged ()
  "If the current pull-request is merged, then visualize that."
  :class 'forge--topic-set-state-command
  :state 'merged
  :getter #'forge-current-pullreq
  (interactive)
  (message "Please use a merge command for this"))

(transient-define-suffix forge-pullreq-state-set-rejected ()
  "Set the state of the current pull-request to `rejected'."
  :class 'forge--topic-set-state-command
  :state 'rejected
  :getter #'forge-current-pullreq)

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
   (reader)
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
      (cond ((string-suffix-p "-p" name)
             (setq name (substring name 0 -2))
             (oset obj reader (lambda (topic) (not (eieio-oref topic slot)))))
            ((oset obj reader (intern (format "forge-read-topic-%s" name)))))
      (oset obj setter (intern (format "forge--set-topic-%s" name)))
      (unless (slot-boundp obj 'formatter)
        (oset obj formatter (intern (format "forge--format-topic-%s" name)))))))

(transient-define-suffix forge-topic-set-title (title)
  "Edit the TITLE of the current topic."
  :class 'forge--topic-set-slot-command :slot 'title
  :formatter (lambda (topic)
               (propertize (forge--format-topic-title topic)
                           'face 'font-lock-string-face)))

(transient-define-suffix forge-topic-set-milestone (milestone)
  "Edit what MILESTONE the current topic belongs to."
  :class 'forge--topic-set-slot-command :slot 'milestone)

(transient-define-suffix forge-topic-set-labels (labels)
  "Edit the LABELS of the current topic."
  :class 'forge--topic-set-slot-command :slot 'labels)

(transient-define-suffix forge-topic-set-marks (marks)
  "Edit the MARKS of the current topic."
  :class 'forge--topic-set-slot-command :slot 'marks)

(transient-define-suffix forge-topic-set-assignees (assignees)
  "Edit the ASSIGNEES of the current topic."
  :class 'forge--topic-set-slot-command :slot 'assignees)

(transient-define-suffix forge-topic-set-review-requests (review-requests)
  "Edit the REVIEW-REQUESTS of the current pull-request."
  :class 'forge--topic-set-slot-command :slot 'review-requests
  :inapt-if-not #'forge-current-pullreq)

(transient-define-suffix forge-topic-toggle-draft (draft)
  "Toggle whether the current pull-request is a draft."
  :class 'forge--topic-set-slot-command :slot 'draft-p
  :inapt-if-not #'forge-current-pullreq
  :description (lambda () (forge--format-boolean 'draft-p "draft")))

(transient-define-suffix forge-topic-toggle-saved ()
  "Toggle whether this topic is marked as saved."
  :class 'forge--topic-set-slot-command :slot 'saved-p
  :description (lambda () (forge--format-boolean 'saved-p "saved"))
  ;; Set only locally because Github's API does not support this.
  (interactive)
  (let ((topic (forge-current-topic t)))
    (oset topic saved-p (not (oref topic saved-p))))
  (forge-refresh-buffer))

;;; Color Utilities

(defun forge--sanitize-color (color)
  (cond ((color-values color) color)
        ;; Discard alpha information.
        ((string-match-p "\\`#.\\{4\\}\\'" color) (substring color 0 3))
        ((string-match-p "\\`#.\\{8\\}\\'" color) (substring color 0 6))
        (t "#000000"))) ; Use fallback instead of invalid color.

(defun forge--contrast-color (color)
  "Return black or white depending on the luminance of COLOR."
  (if (> (forge--x-color-luminance color) 0.5) "black" "white"))

;; Copy of `rainbow-x-color-luminance'.
(defun forge--x-color-luminance (color)
  "Calculate the luminance of a color string (e.g., \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let ((values (color-values color)))
    (forge--color-luminance (/ (nth 0 values) 256.0)
                            (/ (nth 1 values) 256.0)
                            (/ (nth 2 values) 256.0))))

;; Copy of `rainbow-color-luminance'.
;; Also see https://en.wikipedia.org/wiki/Relative_luminance.
(defun forge--color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, GREEN and BLUE.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

;;; Markdown Utilities

(defun forge--fontify-markdown (text)
  (with-temp-buffer
    (delay-mode-hooks
      (gfm-mode))
    (insert text)
    (font-lock-ensure)
    (when forge-post-fill-region
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun forge--markdown-translate-filename-function (file)
  (if (string-match-p "\\`https?://" file)
      file
    (let ((host (oref (forge-get-repository :tracked) forge))) ;aka webhost
      (concat (if (member host ghub-insecure-hosts) "http://" "https://")
              host
              (and (not (string-prefix-p "/" file)) "/")
              file))))

;;; Templates

(defun forge--topic-parse-buffer (&optional file)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((alist (save-excursion (forge--topic-parse-yaml))))
        (if alist
            (setf (alist-get 'yaml alist) t)
          (setq alist (save-excursion (forge--topic-parse-plain))))
        (setf (alist-get 'file alist) file)
        (setf (alist-get 'text alist) (magit--buffer-string nil nil ?\n))
        (when (and file (not (alist-get 'prompt alist)))
          (setf (alist-get 'prompt alist)
                (file-name-sans-extension (file-name-nondirectory file))))
        ;; If there is a yaml front-matter, then it is supposed
        ;; to have a `title' field, but this may not be the case.
        (when (and (not file)
                   (not (alist-get 'title alist)))
          (setf (alist-get 'title alist)
                (read-string "Title: ")))
        alist))))

(defun forge--topic-parse-yaml ()
  (let (alist beg end)
    (when (looking-at "^---[\s\t]*$")
      (forward-line)
      (setq beg (point))
      (when (re-search-forward "^---[\s\t]*$" nil t)
        (setq end (match-beginning 0))
        (setq alist (yaml-parse-string
                     (buffer-substring-no-properties beg end)
                     :object-type 'alist
                     :sequence-type 'list
                     ;; FIXME Does not work because of
                     ;; https://github.com/zkry/yaml.el/pull/28.
                     :false-object nil))
        (let-alist alist
          (when (and .name .about)
            (setf (alist-get 'prompt alist)
                  (format "[%s] %s" .name .about)))
          (when (and .labels (atom .labels))
            (setf (alist-get 'labels alist) (list .labels)))
          (when (and .assignees (atom .assignees))
            (setf (alist-get 'assignees alist) (list .assignees))))
        (forward-line)
        (when (and (not (alist-get 'title alist))
                   (looking-at "^\n?#*"))
          (goto-char (match-end 0))
          (setf (alist-get 'title alist)
                (string-trim
                 (magit--buffer-string (point) (line-end-position) t)))
          (forward-line))
        (setf (alist-get 'body alist)
              (string-trim (magit--buffer-string (point) nil ?\n)))))
    alist))

(defun forge--topic-parse-plain ()
  (let (title body)
    (when (looking-at "\\`#*")
      (goto-char (match-end 0)))
    (setq title (magit--buffer-string (point) (line-end-position) t))
    (forward-line)
    (setq body (magit--buffer-string (point) nil ?\n))
    `((title . ,(string-trim title))
      (body  . ,(string-trim body)))))

(defun forge--topic-parse-link-buffer ()
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (mapcar (lambda (alist)
                (cons (cons 'prompt (concat (alist-get 'name alist) " -- "
                                            (alist-get 'about alist)))
                      alist))
              (forge--topic-parse-yaml-links)))))

(defun forge--topic-parse-yaml-links ()
  (alist-get 'contact_links
             (yaml-parse-string (buffer-substring-no-properties
                                 (point-min)
                                 (point-max))
                                :object-type 'alist
                                :sequence-type 'list)))

(cl-defgeneric forge--topic-template-files (repo class)
  "Return a list of topic template files for REPO and a topic of CLASS.")

(cl-defgeneric forge--topic-template (repo class)
  "Return a topic template alist for REPO and a topic of CLASS.
If there are multiple templates, then the user is asked to select
one of them.  It there are no templates, then return a very basic
alist, containing just `text' and `position'.")

(defun forge--topic-templates-data (repo class)
  (let ((branch (oref repo default-branch)))
    (mapcan (lambda (f)
              (with-temp-buffer
                (magit-git-insert "cat-file" "-p" (concat branch ":" f))
                (if (equal (file-name-nondirectory f) "config.yml")
                    (forge--topic-parse-link-buffer)
                  (list (forge--topic-parse-buffer f)))))
            (forge--topic-template-files repo class))))

(cl-defmethod forge--topic-template ((repo forge-repository)
                                     (class (subclass forge-topic)))
  (let ((choices (forge--topic-templates-data repo class)))
    (if (cdr choices)
        (let ((c (magit-completing-read
                  (if (eq class 'forge-pullreq)
                      "Select pull-request template"
                    "Select issue template")
                  (--map (alist-get 'prompt it) choices)
                  nil t)))
          (--first (equal (alist-get 'prompt it) c) choices))
      (car choices))))

;;; Bug-Reference

(when (< emacs-major-version 28)
  (defun bug-reference-fontify (start end)
    "Apply bug reference overlays to region."
    (save-excursion
      (let ((beg-line (progn (goto-char start) (line-beginning-position)))
            (end-line (progn (goto-char end) (line-end-position))))
        ;; Remove old overlays.
        (bug-reference-unfontify beg-line end-line)
        (goto-char beg-line)
        (while (and (< (point) end-line)
                    (re-search-forward bug-reference-bug-regexp end-line 'move))
          (when (and (or (not bug-reference-prog-mode)
                         ;; This tests for both comment and string syntax.
                         (nth 8 (syntax-ppss)))
                     ;; This is the part where this redefinition differs
                     ;; from the original defined in "bug-reference.el".
                     (not (and (derived-mode-p 'magit-status-mode
                                               'forge-notifications-mode)
                               (= (match-beginning 0)
                                  (line-beginning-position))))
                     ;; End of additions.
                     )
            (let ((overlay (make-overlay (match-beginning 0) (match-end 0)
                                         nil t nil)))
              (overlay-put overlay 'category 'bug-reference)
              ;; Don't put a link if format is undefined
              (when bug-reference-url-format
                (overlay-put overlay 'bug-reference-url
                             (if (stringp bug-reference-url-format)
                                 (format bug-reference-url-format
                                         (match-string-no-properties 2))
                               (funcall bug-reference-url-format)))))))))))

(defun forge-bug-reference-setup ()
  "Setup `bug-reference' in the current buffer.
If forge data has been fetched for the current repository, then
enable `bug-reference-mode' or `bug-reference-prog-mode' and
modify `bug-reference-bug-regexp' if appropriate."
  (unless (or bug-reference-url-format
              (not (forge-db t))
              ;; TODO Allow use in this mode again.
              (derived-mode-p 'forge-notifications-mode))
    (magit--with-safe-default-directory nil
      (when-let ((repo (forge-get-repository :tracked?)))
        (if (>= emacs-major-version 28)
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
          (setq-local bug-reference-url-format
                      (if (forge--childp repo 'forge-gitlab-repository)
                          (lambda ()
                            (forge--format repo
                                           (if (equal (match-string 3) "#")
                                               'issue-url-format
                                             'pullreq-url-format)
                                           `((?i . ,(match-string 2)))))
                        (forge--format repo 'issue-url-format '((?i . "%s")))))
          (setq-local bug-reference-bug-regexp
                      (if (forge--childp repo 'forge-gitlab-repository)
                          "\\(?3:[!#]\\)\\(?2:[0-9]+\\)"
                        "#\\(?2:[0-9]+\\)")))
        (if (derived-mode-p 'prog-mode)
            (bug-reference-prog-mode 1)
          (bug-reference-mode 1))
        (add-hook 'completion-at-point-functions
                  #'forge-topic-completion-at-point nil t)))))

(unless noninteractive
  (dolist (hook forge-bug-reference-hooks)
    (add-hook hook #'forge-bug-reference-setup)))

;;; _
(provide 'forge-topic)
;;; forge-topic.el ends here
