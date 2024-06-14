;;; forge-topics.el --- List topics  -*- lexical-binding:t -*-

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

(require 'hl-line)

(require 'forge-topic)
(require 'forge-tablist)

(defvar x-stretch-cursor)

;;; Options

(defcustom forge-list-buffer-default-topic-filters
  (forge--topics-spec :type 'topic :active t :state 'open :order 'newest)
  "Filters initially used to limit topics listed in list buffers.

This option controls which topics are listed when initially creating
a `forge-topics-mode' buffer.  To temporarly change which topics are
listed in a given buffer, instead use \\`N m' (`forge-topics-menu')."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'object)

(defcustom forge-topic-list-mode-hook '(hl-line-mode)
  "Hook run after entering Forge-Topic-List mode."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'hook
  :options '(hl-line-mode))

(defcustom forge-topic-list-columns
  '(("#"     forge--format-topic-slug          5 nil nil)
    ("Title" forge--format-topic-title+labels 35 nil nil))
  "List of columns displayed when listing topics for a single repository.

Each element has the form (HEADER SOURCE WIDTH SORT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  SOURCE is used to get the value, it has to be the
name of a slot of `forge-topic' or a function that takes such an
object as argument.  SORT is a boolean or a function used to sort
by this column.  Supported PROPS include `:right-align' and
`:pad-right'."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type forge--tablist-columns-type)

(defcustom forge-global-topic-list-columns
  '(("Owner" (repository owner)               15 nil nil)
    ("Name"  (repository name)                20 nil nil)
    ("#"     forge--format-topic-slug          5 nil nil)
    ("Title" forge--format-topic-title+labels 35 nil nil))
  "List of columns displayed when listing topics for all repositories.

Each element has the form (HEADER SOURCE WIDTH SORT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  SOURCE is used to get the value, it has to be the
name of a slot of `forge-topic' or a function that takes such an
object as argument.  SORT is a boolean or a function used to sort
by this column.  Supported PROPS include `:right-align' and
`:pad-right'."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type forge--tablist-columns-type)

(defcustom forge-owned-accounts nil
  "An alist of accounts that are owned by you.
This should include your username as well as any organization
that you own.  Used by the commands `forge-list-owned-issues',
`forge-list-owned-pullreqs' and `forge-fork'.

Each element has the form (ACCOUNT . PLIST).  The following
properties are currently being used:

`remote-name' The default name suggested by `forge-fork' for a
  fork created within this account.  If unspecified, then the
  name of the account is used."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (cons (string :tag "Account") plist)))

(defcustom forge-owned-ignored nil
  "A list of repositories that are ignored when listing those owned by you.
This is a list of package names.  Used by the commands
`forge-list-owned-issues' and `forge-list-owned-pullreqs'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (string :tag "Name")))

;;; Faces

(defface forge-suffix-active
  '((t :inherit transient-value :weight bold))
  "Face used for suffixes whose effects is currently active."
  :group 'forge-faces)

(defface forge-suffix-active-and-implied
  '((t :inherit transient-value :weight semibold))
  "Face used for suffixes whose effects is currently active and implied."
  :group 'forge-faces)

(defface forge--suffix-implied
  '((t :inherit transient-value :weight normal))
  "Face used for suffixes whose effects is currently implied."
  :group 'forge-faces)

(defface forge-tablist-hl-line
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :box ( :line-width ,(if (>= emacs-major-version 28) (cons -1 -1) -1)
            :color "grey25"
            :style nil))
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :box ( :line-width ,(if (>= emacs-major-version 28) (cons -1 -1) -1)
            :color "grey75"
            :style nil)))
  "Face uses instead of `hl-line' in Forge's `tabulated-list-mode' buffers.
It is recommended that you stick to using a box for this purpose,
as using the background color would shadow the background colors
used for labels."
  :group 'forge-faces)

(defface forge-tablist-topic-label
  `((t :inherit forge-topic-label))
  "Face used for topic labels in Forge's `tabulated-list-mode' buffers.
This face can be used to control whether a box is added to labels
and how that is styled.  The background colors used for any given
label, cannot be changed independently of the color used in the
forges web interface."
  :group 'forge-faces)

;;; Mode

(defvar-keymap forge-topic-list-mode-map
  :doc "Local keymap for Forge-Topic-List mode buffers."
  :parent (make-composed-keymap forge-common-map tabulated-list-mode-map)
  "RET"                        #'forge-visit-this-topic
  "<return>"                   #'forge-visit-this-topic
  "o"                          #'forge-browse-this-topic
  "<remap> <forge--list-menu>" #'forge-topics-menu
  "<remap> <forge--item-menu>" #'forge-topic-menu)

(defvar forge-topic-list-mode-name
  '((:eval (capitalize
            (concat (if forge--buffer-list-filter
                        (format "%s " forge--buffer-list-filter)
                      "")
                    (if forge--buffer-list-type
                        (format "%ss" forge--buffer-list-type)
                      "topics")))))
  "Information shown in the mode-line for `forge-topic-list-mode'.
Must be set before `forge-list' is loaded.")

(define-derived-mode forge-topic-list-mode tabulated-list-mode
  forge-topic-list-mode-name
  "Major mode for browsing a list of topics."
  (setq-local hl-line-face 'forge-tablist-hl-line)
  (setq-local x-stretch-cursor nil)
  (setq tabulated-list-padding 0)
  (setq tabulated-list-sort-key (cons "#" nil)))

(defun forge-topic-get-buffer (&optional repo create)
  (let ((name (if repo
                  (format "*forge-topics: %s*" (oref repo slug))
                "*forge-topics*")))
    (if create
        (get-buffer-create name)
      (get-buffer name))))

(defun forge-topic-list-setup (&optional repo spec &rest params)
  (let* ((global (or (plist-get params :global)
                     (and spec (oref spec global))))
         (repo (or repo
                   (and (not global)
                        (if-let* ((topic (forge-topic-at-point))
                                  (repo (forge-get-repository topic)))
                            repo
                          (forge-current-repository :tracked?)))))
         (dir (or (and repo (forge-get-worktree repo)) "/"))
         (buf (forge-topic-get-buffer repo))
         (spec (cond (spec (clone spec))
                     ((and (bufferp buf)
                           (buffer-local-value 'forge--buffer-topics-spec buf)))
                     ((clone forge-list-buffer-default-topic-filters)))))
    (while-let ((key (pop params)))
      (eieio-oset spec key (pop params)))
    (unless (oref spec type)
      (oset spec type 'topic))
    (unless (or repo global)
      (error "Cannot determine repository"))
    (with-current-buffer (or buf (setq buf (forge-topic-get-buffer repo t)))
      (setq default-directory dir)
      (setq forge-buffer-repository (and repo (oref repo id)))
      (setq forge--tabulated-list-columns forge-topic-list-columns)
      (setq forge--tabulated-list-query #'forge--list-topics)
      (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
        (forge-topic-list-mode))
      (setq forge--buffer-topics-spec spec)
      (forge--tablist-refresh)
      (add-hook 'tabulated-list-revert-hook #'forge--tablist-refresh nil t)
      (tabulated-list-print)
      (when hl-line-mode
        (hl-line-highlight)))
    (switch-to-buffer buf)))

;;; Commands
;;;; Menu

;;;###autoload (autoload 'forge-topics-menu "forge-topics" nil t)
(transient-define-prefix forge-topics-menu ()
  "Control list of topics displayed in the current buffer."
  :transient-suffix t
  :transient-non-suffix #'transient--do-call
  :transient-switch-frame nil
  :refresh-suffixes t
  :column-widths forge--topic-menus-column-widths
  [:hide always ("q" forge-menu-quit-list)]
  [forge--topic-menus-group
   ["State"
    ("a" forge-topics-filter-active)
    ("o" forge-topics-filter-state-open)
    ("c" forge-topics-filter-state-completed)
    ("x" forge-topics-filter-state-unplanned)]
   ["Status"
    ("i" forge-topics-filter-status-inbox)
    ("u" forge-topics-filter-status-unread)
    ("p" forge-topics-filter-status-pending)
    ("d" forge-topics-filter-status-done)]
   ["Type"
    ("t t" forge-topics-all-types)
    ("t i" forge-topics-filter-issues)
    ("t p" forge-topics-filter-pullreqs)]]
  [forge--lists-group
   ["Filter"
    ("-m" forge-topics-filter-milestone)
    ("-l" forge-topics-filter-labels)
    ("-x" forge-topics-filter-marks)
    ("-c" forge-topics-filter-author)
    ("-a" forge-topics-filter-assignee)
    ("-r" forge-topics-filter-reviewer)
    ("-s" forge-topics-filter-saved)]]
  (interactive)
  (if (derived-mode-p 'forge-topics-mode 'magit-status-mode)
      (transient-setup 'forge-topics-menu)
    (forge-list-topics)))

(transient-augment-suffix forge-topics-menu
  :transient #'transient--do-replace
  :if-not-derived '(forge-notifications-mode forge-repository-list-mode)
  :inapt-if (lambda () (eq (oref transient--prefix command) 'forge-topics-menu))
  :inapt-face 'forge-suffix-active)

(defvar-local forge--quit-keep-topic-menu nil)

(defun forge-menu-quit-list ()
  "From a transient menu, quit the list buffer and the menu.

If quitting the list buffer causes another topic, repository or
notification list buffer to become current in the selected window,
then display the respective menu, otherwise display no menu."
  (interactive)
  (let ((keep-topic-menu forge--quit-keep-topic-menu))
    (when (derived-mode-p 'forge-topic-mode
                          'forge-topic-list-mode
                          'forge-repository-list-mode
                          'forge-notifications-mode)
      (kill-local-variable 'forge--quit-keep-topic-menu)
      (quit-window))
    (cond ((derived-mode-p 'forge-topic-mode)
           (setq transient--exitp 'replace)
           (transient-setup (setq this-command 'forge-topic-menu)))
          ((derived-mode-p 'forge-topic-list-mode)
           (unless keep-topic-menu
             (setq transient--exitp 'replace)
             (transient-setup (setq this-command 'forge-topics-menu))))
          ((derived-mode-p 'forge-repository-list-mode)
           (setq transient--exitp 'replace)
           (transient-setup (setq this-command 'forge-repositories-menu)))
          ((derived-mode-p 'forge-notifications-mode)
           (setq transient--exitp 'replace)
           (transient-setup (setq this-command 'forge-notifications-menu)))
          (t
           (setq transient--exitp t)
           (transient--pre-exit)
           (transient--stack-zap)))))

;;;; List

(defclass forge--topics-list-command (transient-suffix)
  ((type :initarg :type :initform nil)
   (definition
    :initform (lambda (&optional repo)
                (interactive)
                (forge-topic-list-setup
                 repo nil :type (oref (transient-suffix-object) type))
                (transient-setup 'forge-topics-menu)))))

;;;###autoload (autoload 'forge-list-topics "forge-topics" nil t)
(transient-define-suffix forge-list-topics ()
  "List topics of the current repository."
  :class 'forge--topics-list-command :type nil
  :description "topics"
  :inapt-if-mode 'forge-topics-mode
  :inapt-face 'forge-suffix-active
  (declare (interactive-only nil)))

;;;###autoload (autoload 'forge-list-issues "forge-topics" nil t)
(transient-define-suffix forge-list-issues ()
  "List issues of the current repository."
  :class 'forge--topics-list-command :type 'issue
  :description "issues"
  (declare (interactive-only nil)))

;;;###autoload (autoload 'forge-list-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-pullreqs ()
  "List pull-requests of the current repository."
  :class 'forge--topics-list-command :type 'pullreq
  :description "pull-requests"
  (declare (interactive-only nil)))

;;;; Type

(defclass forge--topics-filter-type-command (transient-suffix)
  ((type :initarg :type)
   (definition
    :initform (lambda (&optional repo)
                (interactive)
                (oset forge--buffer-topics-spec type
                      (oref (transient-suffix-object) type))
                (forge-refresh-buffer)))
   (inapt-face :initform 'forge-suffix-active)
   (inapt-if
    :initform (lambda ()
                (eq (oref forge--buffer-topics-spec type)
                    (oref (transient-suffix-object) type))))))

(transient-define-suffix forge-topics-all-types ()
  :class 'forge--topics-filter-type-command :type 'topic
  :description "topics")

(transient-define-suffix forge-topics-filter-issues ()
  "List issues of the current repository."
  :class 'forge--topics-filter-type-command :type 'issue
  :description "issues")

(transient-define-suffix forge-topics-filter-pullreqs ()
  "List pull-requests of the current repository."
  :class 'forge--topics-filter-type-command :type 'pullreq
  :description "pull-requests")

;;;; Active

(transient-define-suffix forge-topics-filter-active ()
  "Limit topic list to active topics."
  :description "active"
  :face (lambda ()
          (and (oref forge--buffer-topics-spec active)
               'forge-suffix-active))
  (interactive)
  (oset forge--buffer-topics-spec active
        (not (oref forge--buffer-topics-spec active)))
  (forge-refresh-buffer))

;;;; State

(defclass forge--topics-filter-state-command (transient-suffix)
  ((state :initarg :state)
   (definition
    :initform (lambda ()
                (interactive)
                (let ((want (oref (transient-suffix-object) state))
                      (spec forge--buffer-topics-spec))
                  (cond ((and (eq want 'open)
                              (oref spec active))
                         (oset spec active nil)
                         (oset spec state want))
                        ((equal (oref spec state) want)
                         (oset spec state nil))
                        ((oset spec state want))))
                (forge-refresh-buffer)))
   (description
    :initform (lambda (suffix)
                (let ((want (oref suffix state))
                      (type (oref forge--buffer-topics-spec type)))
                  (pcase type
                    ((guard (atom want))
                     (symbol-name want))
                    ('topic   (apply #'format "%s/%s" want))
                    ('issue   (symbol-name (car want)))
                    ('pullreq (symbol-name (cadr want)))))))
   (face
    :initform (lambda (suffix)
                (let ((want   (oref suffix state))
                      (have   (oref forge--buffer-topics-spec state))
                      (active (oref forge--buffer-topics-spec active)))
                  (cond ((and (not active)
                              (equal have want))
                         'forge-suffix-active)
                        ((and (or active
                                  (eq have 'open))
                              (eq want 'open))
                         (if (eq have want)
                             'forge-suffix-active-and-implied
                           'forge--suffix-implied))))))))

(transient-define-suffix forge-topics-filter-state-open ()
  "Limit topic list to open topics."
  :class 'forge--topics-filter-state-command :state 'open)

(transient-define-suffix forge-topics-filter-state-completed ()
  "Limit topic list to completed and merged topics."
  :class 'forge--topics-filter-state-command :state '(completed merged))

(transient-define-suffix forge-topics-filter-state-unplanned ()
  "Limit topic list to unplanned and rejected topics."
  :class 'forge--topics-filter-state-command :state '(unplanned rejected))

;;;; Status

(defclass forge--topics-filter-status-command (transient-suffix)
  ((status :initarg :status)
   (definition
    :initform (lambda ()
                (interactive)
                (let* ((want   (oref (transient-suffix-object) status))
                       (spec   forge--buffer-topics-spec)
                       (have   (oref spec status))
                       (active (oref spec active)))
                  (cond (active
                         (oset spec active nil)
                         (oset spec status want))
                        ((eq have want)
                         (oset spec status nil))
                        ((oset spec status want))))
                (forge-refresh-buffer)))
   (description
    :initform (lambda (suffix) (symbol-name (oref suffix status))))
   (face
    :initform (lambda (suffix)
                (let ((want   (oref suffix status))
                      (have   (oref forge--buffer-topics-spec status))
                      (active (oref forge--buffer-topics-spec active)))
                  (cond ((and (not active)
                              (equal have want))
                         'forge-suffix-active)
                        ((and (or active
                                  (eq have 'inbox))
                              (memq want '(inbox unread pending)))
                         (if (eq have want)
                             'forge-suffix-active-and-implied
                           'forge--suffix-implied))))))))

(transient-define-suffix forge-topics-filter-status-inbox ()
  "Limit topic list to unread and pending topics."
  :class 'forge--topics-filter-status-command :status 'inbox)

(transient-define-suffix forge-topics-filter-status-unread ()
  "Limit topic list to unread topics."
  :class 'forge--topics-filter-status-command :status 'unread)

(transient-define-suffix forge-topics-filter-status-pending ()
  "Limit topic list to pending topics."
  :class 'forge--topics-filter-status-command :status 'pending)

(transient-define-suffix forge-topics-filter-status-done ()
  "Limit topic list to done topics."
  :class 'forge--topics-filter-status-command :status 'done)

;;;; Filter

(defclass forge--topics-filter-command (transient-suffix)
  ((slot        :initarg :slot)
   (reader      :initarg :reader)
   (formatter   :initarg :formatter :initform nil)
   (definition
    :initform (lambda ()
                (interactive)
                (with-slots (slot reader) (transient-suffix-object)
                  (eieio-oset forge--buffer-topics-spec slot
                              (if (eieio-oref forge--buffer-topics-spec slot)
                                  nil
                                (funcall reader)))
                  (forge-refresh-buffer))))
   (description
    :initform (lambda (obj)
                (with-slots (slot formatter) obj
                  (let ((value (eieio-oref forge--buffer-topics-spec slot)))
                    (if value
                        (format "%s %s" slot
                                (if formatter
                                    (funcall formatter value)
                                  (propertize (format "%s" value)
                                              'face 'forge-suffix-active)))
                      (format "%s" slot))))))))

(cl-defmethod initialize-instance :after
  ((obj forge--topics-filter-command) &optional _slots)
  (unless (slot-boundp obj 'reader)
    (oset obj reader (intern (format "forge-read-topic-%s" (oref obj slot))))))

(transient-define-suffix forge-topics-filter-milestone ()
  "Read a milestone and limit topic list to topics with that milestone."
  :class 'forge--topics-filter-command
  :slot 'milestone
  :formatter (lambda (m) (propertize m 'face 'forge-topic-label)))

(transient-define-suffix forge-topics-filter-labels ()
  "Read labels and limit topic list to topics with one of these labels."
  :class 'forge--topics-filter-command
  :slot 'labels)

(transient-define-suffix forge-topics-filter-marks ()
  "Read marks and limit topic list to topics with one of these marks."
  :class 'forge--topics-filter-command
  :slot 'marks
  :formatter
  (lambda (ms) (mapconcat (lambda (m) (propertize m 'face 'forge-topic-label)) ms " ")))

(transient-define-suffix forge-topics-filter-saved ()
  "Toggle whether to limit topic list to saved topics."
  :class 'forge--topics-filter-command
  :slot 'saved
  :reader #'always
  :description
  (lambda () (forge--format-boolean 'saved "saved" forge--buffer-topics-spec)))

(transient-define-suffix forge-topics-filter-author ()
  "Read an author and limit topic list to topics created by that author."
  :class 'forge--topics-filter-command
  :slot 'author
  :reader (lambda () (forge--read-filter-by-user "Author")))

(transient-define-suffix forge-topics-filter-assignee ()
  "Read an assignee and limit topic list to topics assignee to that person."
  :class 'forge--topics-filter-command
  :slot 'assignee
  :reader (lambda () (forge--read-filter-by-user "Assignee")))

(transient-define-suffix forge-topics-filter-reviewer ()
  "Read a reviewer and limit topic list to reviews requested from that person."
  :class 'forge--topics-filter-command
  :slot 'reviewer
  :reader (lambda () (forge--read-filter-by-user "Reviewer")))

(defun forge--read-filter-by-user (prompt)
  (let* ((repo (forge-get-repository :tracked))
         (choices (mapcar #'cadr (oref repo assignees))))
    (magit-completing-read prompt choices)))

;;; _
(provide 'forge-topics)
;;; forge-topics.el ends here
