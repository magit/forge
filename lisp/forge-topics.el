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

(defface forge-suffix-active '((t :inherit transient-value))
  "Face used for suffixes whose effects is currently active."
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
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-topic
  "<return>" #'forge-visit-this-topic
  "o"        #'forge-browse-this-topic
  "C-c C-m"  #'forge-topics-menu
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

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

(defun forge-topic-list-setup (type filter fn &optional repo global columns)
  (let* ((repo (or repo
                   (and (not global)
                        (if-let* ((topic (forge-topic-at-point))
                                  (repo (forge-get-repository topic)))
                            repo
                          (forge-get-repository :tracked?)))))
         (dir (or (and repo (forge-get-worktree repo)) "/"))
         (buffer nil))
    (unless (or repo global)
      (error "Cannot determine repository"))
    (with-current-buffer (setq buffer (forge-topic-get-buffer repo t))
      (setq default-directory dir)
      (setq forge-buffer-repository (and repo (oref repo id)))
      (setq forge--tabulated-list-columns (or columns forge-topic-list-columns))
      (setq forge--tabulated-list-query
            (cond ((not (functionp fn))
                   (lambda ()
                     (cl-sort (mapcan (-cut funcall <> repo) fn)
                              #'> :key (-cut oref <> number))))
                  (repo (apply-partially fn repo))
                  (fn)))
      (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
        (forge-topic-list-mode))
      (setq forge--buffer-list-type type)
      (setq forge--buffer-list-filter filter)
      (setq forge--buffer-list-global global)
      (forge--tablist-refresh)
      (add-hook 'tabulated-list-revert-hook #'forge--tablist-refresh nil t)
      (tabulated-list-print)
      (when hl-line-mode
        (hl-line-highlight)))
    (switch-to-buffer buffer)))

;;; Commands
;;;; Menu

;;;###autoload (autoload 'forge-topics-menu "forge-topics" nil t)
(transient-define-prefix forge-topics-menu ()
  "Control list of topics and the topic at point."
  :transient-suffix t
  :transient-non-suffix t
  :transient-switch-frame nil
  :refresh-suffixes t
  :column-widths forge--topic-menus-column-widths
  [:hide always
   ("q"        forge-menu-quit-list)
   ("RET"      forge-topic-menu)
   ("<return>" forge-topic-menu)]
  [["Type"
    (:info "topics"           :face forge-suffix-active)
    ("n"   "notifications..." forge-notifications-menu :transient replace)
    ("r"   "repositories..."  forge-repositories-menu  :transient replace)]
   [:description (lambda ()
                   (if forge--buffer-list-global
                       "Per-repository lists"
                     "Subtype"))
    ("t" "topics"           forge-list-topics)
    ("i" "issues"           forge-list-issues)
    ("p" "pull-requests"    forge-list-pullreqs)
    ""]
   ["Filter"
    :if (lambda () (and (not forge--buffer-list-global)
                   (eq forge--buffer-list-type 'topic)))
    ("l" "labeled"          forge-list-labeled-topics)
    ("c" "created"          forge-list-authored-topics)
    ("a" "assigned"         forge-list-assigned-topics)]
   ["Filter"
    :if (lambda () (and (not forge--buffer-list-global)
                   (eq forge--buffer-list-type 'issue)))
    ("l" "labeled"          forge-list-labeled-issues)
    ("c" "created"          forge-list-authored-issues)
    ("a" "assigned"         forge-list-assigned-issues)]
   ["Filter"
    :if (lambda () (and (not forge--buffer-list-global)
                   (eq forge--buffer-list-type 'pullreq)))
    ("l" "labeled"          forge-list-labeled-pullreqs)
    ("c" "created"          forge-list-authored-pullreqs)
    ("a" "assigned"         forge-list-assigned-pullreqs)
    ("w" "awaiting review"  forge-list-requested-reviews)]]
  [forge--topic-set-state-group
   ["Global lists"
    ("o t" "owned topics"        forge-list-owned-topics)
    ("o i" "owned issues"        forge-list-owned-issues)
    ("o p" "owned pull-requests" forge-list-owned-pullreqs)]
   ["Actions"
    ("f" "fetch all topics"  forge-pull)
    ("m" "show more actions" forge-dispatch)]]
  [forge--topic-set-status-group]
  (interactive)
  (catch 'add-instead
    (unless (derived-mode-p 'forge-topic-list-mode)
      (let ((repo (forge-current-repository)))
        (cond
         ((or (not repo)
              (forge-get-repository repo nil :tracked?)))
         ((y-or-n-p (format "Add %s to database, so its topics can be listed?"
                            (oref repo slug)))
          (forge-add-repository (forge-get-url repo))
          (throw 'add-instead t))
         ((setq repo nil)))
        (if-let ((buffer (forge-topic-get-buffer repo)))
            (switch-to-buffer buffer)
          (if repo
              (forge-list-topics repo)
            (forge-list-owned-topics)))))
    (transient-setup 'forge-topics-menu)))

(defun forge-menu-quit-list ()
  "From a transient menu, quit the list buffer and the menu.

If quitting the list buffer causes another topic, repository or
notification list buffer to become current in the selected window,
then display the respective menu, otherwise display no menu."
  (interactive)
  (when (derived-mode-p 'forge-topic-mode
                        'forge-topic-list-mode
                        'forge-repository-list-mode
                        'forge-notifications-mode)
    (quit-window))
  (cond ((derived-mode-p 'forge-topic-mode)
         (setq transient--exitp 'replace)
         (transient-setup (setq this-command 'forge-topic-menu)))
        ((derived-mode-p 'forge-topic-list-mode)
         (setq transient--exitp 'replace)
         (transient-setup (setq this-command 'forge-topics-menu)))
        ((derived-mode-p 'forge-repository-list-mode)
         (setq transient--exitp 'replace)
         (transient-setup (setq this-command 'forge-repositories-menu)))
        ((derived-mode-p 'forge-notifications-mode)
         (setq transient--exitp 'replace)
         (transient-setup (setq this-command 'forge-notifications-menu)))
        (t
         (setq transient--exitp t)
         (transient--pre-exit)
         (transient--stack-zap))))

;;;; Suffix Class

(defclass forge--topic-list-command (transient-suffix)
  ((type       :initarg :type   :initform nil)
   (filter     :initarg :filter :initform nil)
   (global     :initarg :global :initform nil)
   (inapt-if                    :initform 'forge--topic-list-inapt)
   (inapt-face                  :initform nil)))

(defun forge--topic-list-inapt ()
  (with-slots (type filter global) transient--pending-suffix
    (and (eq type   forge--buffer-list-type)
         (eq filter forge--buffer-list-filter)
         (eq global forge--buffer-list-global))))

(cl-defmethod transient-format-description ((obj forge--topic-list-command))
  (with-slots (description type filter global) obj
    (if (and (eq   type   forge--buffer-list-type)
             (memq filter (list nil forge--buffer-list-filter))
             (eq   global forge--buffer-list-global))
        (propertize description 'face 'forge-suffix-active)
      description)))

;;;; Topic

(defun forge--topic-list-setup (filter fn &optional repo global columns)
  (forge-topic-list-setup 'topic filter fn repo global columns))

;;;###autoload (autoload 'forge-list-topics "forge-topics" nil t)
(transient-define-suffix forge-list-topics (&optional repository)
  "List topics of the current repository.
Non-interactively if optional REPOSITORY is non-nil, then list
topics for that instead."
  :class 'forge--topic-list-command :type 'topic
  (interactive)
  (forge--topic-list-setup nil
                           (list #'forge--ls-issues
                                 #'forge--ls-pullreqs)
                           repository))
(put 'forge-list-topics 'interactive-only nil)

;;;###autoload (autoload 'forge-list-labeled-topics "forge-topics" nil t)
(transient-define-suffix forge-list-labeled-topics (label)
  "List topics of the current repository that have LABEL."
  :class 'forge--topic-list-command :type 'topic :filter 'labeled
  (interactive (list (forge-read-topic-label)))
  (forge--topic-list-setup 'labeled
                           (list (-cut forge--ls-labeled-issues   <> label)
                                 (-cut forge--ls-labeled-pullreqs <> label))))

;;;###autoload (autoload 'forge-list-assigned-topics "forge-topics" nil t)
(transient-define-suffix forge-list-assigned-topics ()
  "List topics of the current repository that are assigned to you."
  :class 'forge--topic-list-command :type 'topic :filter 'assigned
  (interactive)
  (forge--topic-list-setup 'assigned
                           (list #'forge--ls-assigned-issues
                                 #'forge--ls-assigned-pullreqs)))

;;;###autoload (autoload 'forge-list-authored-topics "forge-topics" nil t)
(transient-define-suffix forge-list-authored-topics ()
  "List open topics from the current repository that are authored by you."
  :class 'forge--topic-list-command :type 'topic :filter 'authored
  (interactive)
  (forge--topic-list-setup 'authored
                           (list #'forge--ls-authored-issues
                                 #'forge--ls-authored-pullreqs)))

;;;###autoload (autoload 'forge-list-owned-topics "forge-topics" nil t)
(transient-define-suffix forge-list-owned-topics ()
  "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--topic-list-command :type 'topic :filter 'owned :global t
  (interactive)
  (forge--topic-list-setup 'owned
                           (list (lambda (_) (forge--ls-owned-issues))
                                 (lambda (_) (forge--ls-owned-pullreqs)))
                           nil t forge-global-topic-list-columns))
(put 'forge-list-owned-topics 'interactive-only nil)

;;;; Issue

(defun forge--issue-list-setup (filter fn &optional repo global columns)
  (forge-topic-list-setup 'issue filter fn repo global columns))

;;;###autoload (autoload 'forge-list-issues "forge-topics" nil t)
(transient-define-suffix forge-list-issues ()
  "List issues of the current repository."
  :class 'forge--topic-list-command :type 'issue
  (interactive)
  (forge--issue-list-setup nil #'forge--ls-issues))

;;;###autoload (autoload 'forge-list-labeled-issues "forge-topics" nil t)
(transient-define-suffix forge-list-labeled-issues (label)
  "List issues of the current repository that have LABEL."
  :class 'forge--topic-list-command :type 'issue :filter 'labeled
  (interactive (list (forge-read-topic-label)))
  (forge--issue-list-setup 'labeled (-cut forge--ls-labeled-issues <> label)))

;;;###autoload (autoload 'forge-list-assigned-issues "forge-topics" nil t)
(transient-define-suffix forge-list-assigned-issues ()
  "List issues of the current repository that are assigned to you."
  :class 'forge--topic-list-command :type 'issue :filter 'assigned
  (interactive)
  (forge--issue-list-setup 'assigned #'forge--ls-assigned-issues))

;;;###autoload (autoload 'forge-list-authored-issues "forge-topics" nil t)
(transient-define-suffix forge-list-authored-issues ()
  "List open issues from the current repository that are authored by you."
  :class 'forge--topic-list-command :type 'issue :filter 'authored
  (interactive)
  (forge--issue-list-setup 'authored #'forge--ls-authored-issues))

;;;###autoload (autoload 'forge-list-owned-issues "forge-topics" nil t)
(transient-define-suffix forge-list-owned-issues ()
  "List open issues from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--topic-list-command :type 'issue :filter 'owned :global t
  (interactive)
  (forge--issue-list-setup 'owned #'forge--ls-owned-issues
                           nil t forge-global-topic-list-columns))

;;;; Pullreq

(defun forge--pullreq-list-setup (filter fn &optional repo global columns)
  (forge-topic-list-setup 'pullreq filter fn repo global columns))

;;;###autoload (autoload 'forge-list-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-pullreqs ()
  "List pull-requests of the current repository."
  :class 'forge--topic-list-command :type 'pullreq
  (interactive)
  (forge--pullreq-list-setup nil #'forge--ls-pullreqs))

;;;###autoload (autoload 'forge-list-labeled-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-labeled-pullreqs (label)
  "List pull-requests of the current repository that have LABEL."
  :class 'forge--topic-list-command :type 'pullreq :filter 'labeled
  (interactive (list (forge-read-topic-label)))
  (forge--pullreq-list-setup 'labeled (-cut forge--ls-labeled-pullreqs <> label)))

;;;###autoload (autoload 'forge-list-assigned-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-assigned-pullreqs ()
  "List pull-requests of the current repository that are assigned to you."
  :class 'forge--topic-list-command :type 'pullreq :filter 'assigned
  (interactive)
  (forge--pullreq-list-setup 'assigned #'forge--ls-assigned-pullreqs))

;;;###autoload (autoload 'forge-list-requested-reviews "forge-topics" nil t)
(transient-define-suffix forge-list-requested-reviews ()
  "List pull-requests of the current repository that are awaiting your review."
  :class 'forge--topic-list-command :type 'pullreq :filter 'review
  (interactive)
  (forge--pullreq-list-setup 'review #'forge--ls-requested-reviews))

;;;###autoload (autoload 'forge-list-authored-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-authored-pullreqs ()
  "List open pull-requests from the current repository that are authored by you."
  :class 'forge--topic-list-command :type 'pullreq :filter 'authored
  (interactive)
  (forge--pullreq-list-setup 'authored #'forge--ls-authored-pullreqs))

;;;###autoload (autoload 'forge-list-owned-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-owned-pullreqs ()
  "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--topic-list-command :type 'pullreq :filter 'owned :global t
  (interactive)
  (forge--pullreq-list-setup 'owned #'forge--ls-owned-pullreqs
                             nil t forge-global-topic-list-columns))

;;; _
(provide 'forge-topics)
;;; forge-topics.el ends here
