;;; forge-repos.el --- List repositories  -*- lexical-binding:t -*-

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

(require 'forge-repo)
(require 'forge-tablist)

(defvar x-stretch-cursor)

;;; Options

(defcustom forge-repository-list-mode-hook '(hl-line-mode)
  "Hook run after entering Forge-Repository-List mode."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'hook
  :options '(hl-line-mode))

(defcustom forge-repository-list-columns
  '(("Owner"    owner                       20   t nil)
    ("Name"     name                        20   t nil)
    ("T"        forge-format-repo-condition  1   t nil)
    ("S"        forge-format-repo-selective  1   t nil)
    ("Worktree" worktree                    99   t nil))
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

;;; Mode

(defvar-keymap forge-repository-list-mode-map
  :doc "Local keymap for Forge-Repository-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-repository
  "<return>" #'forge-visit-this-repository
  "o"        #'forge-browse-this-repository
  "C-c C-m"  #'forge-repositories-menu
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

(defvar forge-repository-list-buffer-name "*forge-repositories*"
  "Buffer name to use for displaying lists of repositories.")

(defvar forge-repository-list-mode-name
  '((:eval (capitalize
            (concat (if forge--buffer-list-filter
                        (format "%s " forge--buffer-list-filter)
                      "")
                    "repositories"))))
  "Information shown in the mode-line for `forge-repository-list-mode'.
Must be set before `forge-list' is loaded.")

(define-derived-mode forge-repository-list-mode tabulated-list-mode
  forge-repository-list-mode-name
  "Major mode for browsing a list of repositories."
  (setq-local x-stretch-cursor nil)
  (setq tabulated-list-padding 0)
  (setq tabulated-list-sort-key (cons "Owner" nil)))

(defun forge-repository-list-setup (filter fn)
  (let ((buffer (get-buffer-create forge-repository-list-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory "/")
      (setq forge--tabulated-list-columns forge-repository-list-columns)
      (setq forge--tabulated-list-query fn)
      (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
        (forge-repository-list-mode))
      (setq forge--buffer-list-type 'repo)
      (setq forge--buffer-list-filter filter)
      (setq forge--buffer-list-global t)
      (forge--tablist-refresh)
      (add-hook 'tabulated-list-revert-hook #'forge--tablist-refresh nil t)
      (tabulated-list-print)
      (when hl-line-mode
        (hl-line-highlight)))
    (switch-to-buffer buffer)))

(defun forge-format-repo-condition (repo)
  "Return a character representing the value of REPO's `condition' slot."
  (pcase-exhaustive (oref repo condition)
    (:tracked "*")
    (:known " ")
    (:stub (propertize "s" 'face 'warning))))

(defun forge-format-repo-selective (repo)
  "Return a character representing the value of REPO's `selective-p' slot."
  (pcase-exhaustive (oref repo selective-p)
    ('t   "*")
    ('nil " ")))

;;; Menu

;;;###autoload (autoload 'forge-repositories-menu "forge-repos" nil t)
(transient-define-prefix forge-repositories-menu ()
  "Control list of repositories and repository at point."
  :transient-suffix t
  :transient-non-suffix 'call
  :transient-switch-frame nil
  :refresh-suffixes t
  [:hide always ("q" forge-menu-quit-list)]
  [["Type"
    ("t" "topics..."        forge-topics-menu       :transient replace)
    ("n" "notifications..." forge-notifications-menu :transient replace)
    ("r" "repositories"     forge-list-repositories)]
   ["Filter"
    ("o" "owned" forge-list-owned-repositories)]]
  (interactive)
  (unless (derived-mode-p 'forge-repository-list-mode)
    (if-let ((buffer (get-buffer forge-repository-list-buffer-name)))
        (switch-to-buffer buffer)
      (with-no-warnings ; "interactive use only"
        (forge-list-repositories))))
  (transient-setup 'forge-repositories-menu))

;;; Class

(defclass forge--repo-list-command (transient-suffix)
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

(cl-defmethod transient-format-description ((obj forge--repo-list-command))
  (with-slots (description type filter global) obj
    (if (and (eq   type   forge--buffer-list-type)
             (memq filter (list nil forge--buffer-list-filter))
             (eq   global forge--buffer-list-global))
        (propertize description 'face 'forge-active-suffix)
      description)))

;;; Commands

;;;###autoload (autoload 'forge-list-repositories "forge-repos" nil t)
(transient-define-suffix forge-list-repositories ()
  "List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database."
  :class 'forge--repo-list-command :type 'repo :global t
  (interactive)
  (forge-repository-list-setup nil #'forge--ls-repos))

;;;###autoload (autoload 'forge-list-owned-repositories "forge-repos" nil t)
(transient-define-suffix forge-list-owned-repositories ()
  "List your own known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database
and options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--repo-list-command :type 'repo :filter 'owned :global t
  (interactive)
  (forge-repository-list-setup 'owned #'forge--ls-owned-repos))

;;; _
(provide 'forge-repos)
;;; forge-repos.el ends here
