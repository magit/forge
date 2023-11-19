;;; forge-list.el --- Tabulated-list interface  -*- lexical-binding:t -*-

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

(require 'hl-line)
(require 'tabulated-list)

(require 'forge)

(defvar x-stretch-cursor)

;;; Options

(defcustom forge-topic-list-mode-hook '(hl-line-mode)
  "Hook run after entering Forge-Topic-List mode."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'hook
  :options '(hl-line-mode))

(defconst forge--tablist-columns-type
  '(repeat
    (list :tag "Column"
          (string  :tag "Header Label")
          (choice  :tag "Value source"
                   (function)
                   (symbol :tag "Object slot"))
          (integer :tag "Column Width")
          (choice  :tag "Sort predicate"
                   (const :tag "Don't sort" nil)
                   (const :tag "Default" t)
                   (function))
          (repeat  :tag "Properties"
                   (list (choice :tag "Property"
                                 (const :right-align)
                                 (const :pad-right)
                                 (symbol))
                         (sexp :tag "Value"))))))

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

(defcustom forge-repository-list-columns
  '(("Owner"    owner         20   t nil)
    ("Name"     name          20   t nil)
    ("N"        sparse-p       1   t nil)
    ("S"        selective-p    1   t nil)
    ("Worktree" worktree      99   t nil))
  "List of columns displayed when listing repositories.

Each element has the form (HEADER SOURCE WIDTH SORT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  SOURCE is used to get the value, it has to be the
name of a slot of `forge-repository' or a function that takes
such an object as argument.  SORT is a boolean or a function used
to sort by this column.  Supported PROPS include `:right-align'
and `:pad-right'."
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

;;; Variables

(defvar-local forge--tabulated-list-columns nil)
(put 'forge--tabulated-list-columns 'permanent-local t)

(defvar-local forge--tabulated-list-query nil)
(put 'forge--tabulated-list-query 'permanent-local t)

;;; Modes
;;;; Topics

(defvar-keymap forge-topic-list-mode-map
  :doc "Local keymap for Forge-Topic-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-topic
  "<return>" #'forge-visit-this-topic
  "o"        #'forge-browse-this-topic
  "L"        #'forge-topics-menu
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

(define-derived-mode forge-topic-list-mode tabulated-list-mode
  "Forge Topics"
  "Major mode for browsing a list of topics."
  (setq-local x-stretch-cursor  nil)
  (setq-local hl-line-face 'forge-tablist-hl-line)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "#" nil)))

(defun forge-topic-list-setup (fn &optional repo global-name columns)
  (let* ((repo (or repo
                   (and (not global-name)
                        (forge-get-repository t))))
         (topdir (and repo (oref repo worktree)))
         (buffer (get-buffer-create
                  (or global-name
                      (format "*forge-topic-list: %s/%s*"
                              (oref repo owner)
                              (oref repo name))))))
    (with-current-buffer buffer
      (setq default-directory (or topdir "/"))
      (setq forge-buffer-repository repo)
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
      (forge-topic-list-refresh)
      (add-hook 'tabulated-list-revert-hook
                #'forge-topic-list-refresh nil t)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (when hl-line-mode
        (hl-line-highlight)))
    (switch-to-buffer buffer)))

(defun forge-topic-list-refresh ()
  (setq tabulated-list-format
        (vconcat (mapcar (pcase-lambda (`(,name ,_get ,width ,sort ,props))
                           `(,name ,width ,sort . ,props))
                         forge--tabulated-list-columns)))
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar
         (lambda (topic)
           (list (oref topic id)
                 (vconcat
                  (mapcar (pcase-lambda (`(,_name ,get ,_width ,_sort ,_props))
                            (cond
                             ((functionp get)
                              (funcall get topic))
                             ((eq (car-safe get) 'repository)
                              (eieio-oref (forge-get-repository topic)
                                          (cadr get)))
                             ((eieio-oref topic get))))
                          forge--tabulated-list-columns))))
         (funcall forge--tabulated-list-query))))

;;;; Repository

(defvar-keymap forge-repository-list-mode-map
  :doc "Local keymap for Forge-Repository-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-repository
  "<return>" #'forge-visit-this-repository
  "o"        #'forge-browse-this-repository
  "L"        #'forge-repository-menu
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

(define-derived-mode forge-repository-list-mode tabulated-list-mode
  "Repositories"
  "Major mode for browsing a list of repositories."
  (setq-local x-stretch-cursor  nil)
  (setq forge--tabulated-list-columns forge-repository-list-columns)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Owner" nil))
  (setq tabulated-list-format
        (vconcat (mapcar (pcase-lambda (`(,name ,_get ,width ,sort ,props))
                           `(,name ,width ,sort . ,props))
                         forge--tabulated-list-columns)))
  (tabulated-list-init-header))

(defun forge-repository-list-setup (fn buf)
  (let ((buffer (get-buffer-create buf)))
    (with-current-buffer buffer
      (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
        (forge-repository-list-mode))
      (funcall fn)
      (add-hook 'tabulated-list-revert-hook fn nil t)
      (tabulated-list-print))
    (switch-to-buffer buffer)))

(defun forge-repository-list-refresh ()
  (forge-repository--tabulate-entries))

(defun forge-repository-list-owned-refresh ()
  (forge-repository--tabulate-entries
   [:where (and (in owner $v2) (not (in name $v3)))]
   (vconcat (mapcar #'car forge-owned-accounts))
   (vconcat forge-owned-ignored)))

(defun forge-repository--tabulate-entries (&optional where &rest args)
  (setq tabulated-list-entries
        (mapcar
         (pcase-lambda (`(,id . ,row))
           (list id (vconcat (mapcar (lambda (v) (if v (format "%s" v) "")) row))))
         (apply #'forge-sql
                (vconcat [:select $i1 :from repository]
                         where
                         [:order-by [(asc owner) (asc name)]])
                (vconcat [id] (mapcar #'cadr forge--tabulated-list-columns))
                args))))

;;; Commands
;;;; Menus

;;;###autoload (autoload 'forge-topics-menu "forge-list" nil t)
(transient-define-prefix forge-topics-menu ()
  "Control list of topics and topic at point."
  [["List"
    ("t" "topics"        forge-list-topics)
    ("i" "issues"        forge-list-issues)
    ("p" "pull-requests" forge-list-pullreqs)
    ("l a" "awaiting review"        forge-list-requested-reviews)
    ("n i" "labeled issues"         forge-list-labeled-issues)
    ("n p" "labeled pull-requests"  forge-list-labeled-pullreqs)
    ("m i" "authored issues"        forge-list-authored-issues)
    ("m p" "authored pull-requests" forge-list-authored-pullreqs)
    ("o i" "owned issues"           forge-list-owned-issues)
    ("o p" "owned pull-requests"    forge-list-owned-pullreqs)]]
  [["Set status"
    ("u" forge-topic-status-set-unread)
    ("x" forge-topic-status-set-pending)
    ("d" forge-topic-status-set-done)
    ("s" forge-topic-toggle-saved)]])

;;;###autoload (autoload 'forge-repository-menu "forge-list" nil t)
(transient-define-prefix forge-repository-menu ()
  "Control list of repositories and repository at point."
  [["List"
    ("r" "repositories"       forge-list-repositories)
    ("o" "owned repositories" forge-list-owned-repositories)]])

;;;; Topic

(defun forge--topic-list-setup (fn &optional repo buffer-name columns)
  (forge-topic-list-setup fn repo buffer-name columns))

;;;###autoload
(defun forge-list-topics (&optional repository)
  "List topics of the current repository.
Non-interactively if optional REPOSITORY is non-nil, then list
topics for that instead."
  (interactive)
  (forge--topic-list-setup (list #'forge-ls-issues #'forge-ls-pullreqs)
                           repository))

;;;###autoload
(defun forge-list-labeled-topics (label)
  "List topics of the current repository that have LABEL."
  (interactive (list (forge-read-topic-label)))
  (forge--topic-list-setup (list (-cut forge--ls-labeled-issues   <> label)
                                 (-cut forge--ls-labeled-pullreqs <> label))))

;;;###autoload
(defun forge-list-assigned-topics ()
  "List topics of the current repository that are assigned to you."
  (interactive)
  (forge--topic-list-setup (list #'forge--ls-assigned-issues
                                 #'forge--ls-assigned-pullreqs)))

;;;###autoload
(defun forge-list-authored-topics ()
  "List open topics from the current repository that are authored by you."
  (interactive)
  (forge--topic-list-setup (list #'forge--ls-authored-issues
                                 #'forge--ls-authored-pullreqs)))

;;;###autoload
(defun forge-list-owned-topics ()
  "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  (interactive)
  (forge--topic-list-setup (list (lambda (_) (forge--ls-owned-issues))
                                 (lambda (_) (forge--ls-owned-pullreqs)))
                           nil "My topics" forge-global-topic-list-columns))

;;;; Issue

(defun forge--issue-list-setup (fn &optional repo buffer-name columns)
  (forge-topic-list-setup fn repo buffer-name columns))

;;;###autoload
(defun forge-list-issues ()
  "List issues of the current repository."
  (interactive)
  (forge--issue-list-setup #'forge-ls-issues))

;;;###autoload
(defun forge-list-labeled-issues (label)
  "List issues of the current repository that have LABEL."
  (interactive (list (forge-read-topic-label)))
  (forge--issue-list-setup (-cut forge--ls-labeled-issues <> label)))

;;;###autoload
(defun forge-list-assigned-issues ()
  "List issues of the current repository that are assigned to you."
  (interactive)
  (forge--issue-list-setup #'forge--ls-assigned-issues))

;;;###autoload
(defun forge-list-authored-issues ()
  "List open issues from the current repository that are authored by you."
  (interactive)
  (forge--issue-list-setup #'forge--ls-authored-issues))

;;;###autoload
(defun forge-list-owned-issues ()
  "List open issues from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  (interactive)
  (forge--issue-list-setup #'forge--ls-owned-issues nil "My issues"
                           forge-global-topic-list-columns))

;;;; Pullreq

(defun forge--pullreq-list-setup (fn &optional repo buffer-name columns)
  (forge-topic-list-setup fn repo buffer-name columns))

;;;###autoload
(defun forge-list-pullreqs ()
  "List pull-requests of the current repository."
  (interactive)
  (forge--pullreq-list-setup #'forge-ls-pullreqs))

;;;###autoload
(defun forge-list-labeled-pullreqs (label)
  "List pull-requests of the current repository that have LABEL."
  (interactive (list (forge-read-topic-label)))
  (forge--pullreq-list-setup (-cut forge--ls-labeled-pullreqs <> label)))

;;;###autoload
(defun forge-list-assigned-pullreqs ()
  "List pull-requests of the current repository that are assigned to you."
  (interactive)
  (forge--pullreq-list-setup #'forge--ls-assigned-pullreqs))

;;;###autoload
(defun forge-list-requested-reviews ()
  "List pull-requests of the current repository that are awaiting your review."
  (interactive)
  (forge--pullreq-list-setup #'forge--ls-requested-reviews))

;;;###autoload
(defun forge-list-authored-pullreqs ()
  "List open pull-requests of the current repository that are authored by you."
  (interactive)
  (forge--pullreq-list-setup #'forge--ls-authored-pullreqs))

;;;###autoload
(defun forge-list-owned-pullreqs ()
  "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  (interactive)
  (forge--pullreq-list-setup #'forge--ls-owned-pullreqs nil "My pullreqs"
                             forge-global-topic-list-columns))

;;;; Repository

;;;###autoload
(defun forge-list-repositories ()
  "List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database."
  (interactive)
  (forge-repository-list-setup #'forge-repository-list-refresh
                               "*Forge Repositories*"))

;;;###autoload
(defun forge-list-owned-repositories ()
  "List your own known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database
and options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  (interactive)
  (forge-repository-list-setup #'forge-repository-list-owned-refresh
                               "*Forge Owned Repositories*"))

;;; _
(provide 'forge-list)
;;; forge-list.el ends here
