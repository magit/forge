;;; forge-post.el --- Post support  -*- lexical-binding:t -*-

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

(require 'markdown-mode)

(require 'forge)

;;; Options

(defcustom forge-post-mode-hook
  '(visual-line-mode
    turn-on-flyspell)
  "Hook run after entering Forge-Post mode.
This hook is run early on while setting up a buffer to edit a post.
If you want to make changes to the already populated buffer, instead
use `forge-edit-post-hook'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type 'hook
  :options '(visual-line-mode
             turn-on-flyspell))

(defcustom forge-edit-post-hook
  '(forge-create-pullreq-insert-single-commit-message
    forge-create-pullreq-show-diff)
  "Hook run after setting up a buffer to edit a post.
Consult the variable `forge-edit-post-action' to determine the action;
one of `new-discussion', `new-issue', `new-pullreq', `reply' and `edit'."
  :package-version '(forge . "0.6.0")
  :group 'forge
  :type 'hook
  :options '(forge-create-pullreq-insert-single-commit-message
             forge-create-pullreq-insert-branch-description
             forge-create-pullreq-show-diff))

(defcustom forge-post-fallback-directory
  (locate-user-emacs-file "forge-drafts/")
  "Directory used to store post drafts for locally unavailable repositories.
Normally drafts are stored inside the Git directory.  If that does not
exist (or its location is unknown), then this directory is used instead."
  :package-version '(forge . "0.4.7")
  :group 'forge
  :type 'directory)

;;; Class

(defclass forge-post (forge-object) () :abstract t)

;;; Query
;;;; Get

(cl-defmethod forge-get-parent ((post forge-post))
  (forge-get-topic post))

(cl-defmethod forge-get-repository ((post forge-post))
  (forge-get-repository (forge-get-topic post)))

;;;; Current

(defun forge-post-at-point (&optional assert)
  "Return the post at point.
If there is no such post and DEMAND is non-nil, then signal
an error."
  (or (magit-section-value-if '(issue pullreq post))
      (and assert (user-error "There is no post at point"))))

(defun forge-comment-at-point (&optional assert)
  "Return the comment at point.
If there is no such comment and DEMAND is non-nil, then signal
an error."
  (or (and (magit-section-value-if '(post))
           (let ((post (oref (magit-current-section) value)))
             (and (or (forge-pullreq-post-p post)
                      (forge-issue-post-p post))
                  post)))
      (and assert (user-error "There is no comment at point"))))

;;; Utilities

(cl-defmethod forge--format ((post forge-post) slot &optional spec)
  (forge--format (forge-get-topic post) slot
                 `(,@spec (?I . ,(oref post number)))))

;;; Mode

(defvar-keymap forge-post-mode-map
  "<remap> <forge--item-menu>"             #'forge-post-menu
  "C-c C-e"                                #'forge-post-menu
  "C-c C-c"                                #'forge-post-submit
  "<remap> <evil-save-and-close>"          #'forge-post-submit
  "<remap> <evil-save-modified-and-close>" #'forge-post-submit
  "C-c C-k"                                #'forge-post-cancel
  "<remap> <kill-buffer>"                  #'forge-post-cancel
  "<remap> <ido-kill-buffer>"              #'forge-post-cancel
  "<remap> <iswitchb-kill-buffer>"         #'forge-post-cancel
  "<remap> <evil-quit>"                    #'forge-post-cancel)

(define-derived-mode forge-post-mode gfm-mode "Forge-Post"
  "Major mode for editing topic posts."
  :interactive nil)

(defvar-local forge--pre-post-buffer nil)
(defvar-local forge--pre-post-winconf nil)

(defvar-local forge--submit-post-function nil)

(defvar-local forge-edit-post-action nil
  "The action being carried out by editing this post buffer.
One of `new-discussion', `new-issue', `new-pullreq', `reply' and `edit'.")

(defvar-local forge--buffer-post-object nil)
(defvar-local forge--buffer-template nil)
(defvar-local forge--buffer-category nil)
(defvar-local forge--buffer-milestone nil)
(defvar-local forge--buffer-labels nil)
(defvar-local forge--buffer-assignees nil)
(defvar-local forge--buffer-base-branch nil)
(defvar-local forge--buffer-head-branch nil)
(defvar-local forge--buffer-draft-p nil)

(defun forge--setup-post-buffer ( obj-or-action submit file header
                                  &optional bindings fn)
  (declare (indent defun))
  (let* ((prevbuf (current-buffer))
         (winconf (current-window-configuration))
         (action  (cond ((symbolp obj-or-action)             obj-or-action)
                        ((forge--childp obj-or-action 'forge-topic) 'reply)
                        ((forge--childp obj-or-action 'forge-post)   'edit)))
         (obj     (if (symbolp obj-or-action)
                      (forge-get-repository :tracked)
                    obj-or-action))
         (repo    (forge-get-repository obj))
         (header  (forge--format obj header))
         (file    (forge--post-expand-file-name (forge--format obj file) repo))
         (_       (make-directory (file-name-directory file) t))
         (buffer  (find-file-noselect file))
         (resume  (forge--post-resume-p file buffer)))
    (with-current-buffer buffer
      (forge-post-mode)
      (magit-set-header-line-format header)
      (setq forge--pre-post-buffer prevbuf)
      (setq forge--pre-post-winconf winconf)
      (forge-set-buffer-repository)
      (setq forge-edit-post-action action)
      (setq forge--buffer-post-object obj)
      (setq forge--submit-post-function submit)
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val)
        (when (eq var 'forge--buffer-template)
          (let-alist forge--buffer-template
            (setq forge--buffer-assignees .assignees)
            (setq forge--buffer-labels .labels)
            (setq forge--buffer-draft-p .draft))))
      (cond-let
        (resume)
        ((not (memq action '(new-discussion new-issue new-pullreq))))
        ([template (alist-get 'text forge--buffer-template)]
         (unless (string-prefix-p "# " template)
           (insert "# \n\n"))
         (insert template)
         (goto-char 3))
        ((insert "# ")))
      (when fn
        (funcall fn)))
    (run-hook-wrapped 'forge-edit-post-hook
                      (lambda (fn) (with-current-buffer buffer (funcall fn)) nil))
    (message (substitute-command-keys
              "Use \\[forge-post-menu] to set fields and submit or abort"))
    (forge--display-post-buffer buffer)))

(defun forge--display-post-buffer (buf)
  (magit-display-buffer buf #'display-buffer))

(defun forge--post-expand-file-name (file repo)
  (if-let ((worktree (oref repo worktree)))
      (expand-file-name (concat "magit/posts/" file) (magit-gitdir worktree))
    (expand-file-name (with-slots (githost owner name) repo
                        (format "%s_%s-%s_%s" githost owner name file))
                      forge-post-fallback-directory)))

(defun forge--post-resume-p (file buffer)
  (and (file-exists-p file)
       (> (file-attribute-size (file-attributes file)) 0)
       (progn (forge--display-post-buffer buffer)
              (or (magit-read-char-case "" nil
                    (?r "[r]esume editing this draft" t)
                    (?d "[d]iscard and start over?"))
                  (progn (erase-buffer)
                         nil)))))

(defun forge-create-pullreq-insert-single-commit-message ()
  "When creating a pull-request from a single commit, insert its message."
  (when-let* ((source forge--buffer-head-branch)
              (target forge--buffer-base-branch)
              (_(= (car (magit-rev-diff-count source target)) 1)))
    (when (alist-get 'text forge--buffer-template)
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert ?\n))
      (insert "\n<!-- Message of single commit: -->\n\n"))
    (magit-rev-insert-format "%B" source)
    (when (= (char-before (1- (point))) ?\n)
      (delete-char -1))
    (goto-char 3)))

(defun forge-create-pullreq-insert-branch-description ()
  "When creating a pull-request, insert branch description, if any.
Insert the value of `branch.BRANCH.description' of the source BRANCH."
  (when-let* ((source forge--buffer-head-branch)
              (description (magit-get "branch"
                                      (cdr (magit-split-branch-name source))
                                      "description")))
    (when (or (alist-get 'text forge--buffer-template)
              (> (point-max) 3))
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert ?\n))
      (insert "\n<!-- Branch description: -->\n\n"))
    (insert description)
    (goto-char 3)))

(defun forge-create-pullreq-show-diff ()
  "When creating a pull-request, show diff for the branch's changes."
  (when (eq forge-edit-post-action 'new-pullreq)
    (magit-diff-setup-buffer
     (format "%s...%s"
             forge--buffer-base-branch
             forge--buffer-head-branch)
     nil (car (magit-diff-arguments)) nil 'committed t)))

(defun forge--post-buffer-text ()
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward "\s\t\n")
      (let (title body)
        (when (looking-at "^#*[\s\t]*")
          (goto-char (match-end 0)))
        (setq title (buffer-str (point) (line-end-position)))
        (forward-line)
        (setq body (buffer-str (point)))
        (cons (string-trim title)
              (string-trim body))))))

(defun forge--post-submit-callback (&optional full-pull)
  (let* ((file    buffer-file-name)
         (winconf forge--pre-post-winconf)
         (editbuf (current-buffer))
         (prevbuf forge--pre-post-buffer)
         (topic   (ignore-errors (forge-get-topic forge--buffer-post-object)))
         (repo    (forge-get-repository (or topic forge--buffer-post-object))))
    (lambda (value &optional headers status req)
      (run-hook-with-args 'forge-post-submit-callback-hook
                          value headers status req)
      (delete-file file t)
      (let ((dir (file-name-directory file)))
        (unless (cddr (directory-files dir nil nil t))
          (delete-directory dir nil t)))
      (when (buffer-live-p editbuf)
        (with-current-buffer editbuf
          (magit-mode-bury-buffer 'kill)))
      (forge--maybe-restore-winconf winconf)
      (with-current-buffer
          (if (buffer-live-p prevbuf) prevbuf (current-buffer))
        (if (or (not full-pull)
                (oref repo selective-p))
            (forge--pull-topic repo topic)
          (forge--pull repo))))))

(defun forge--post-submit-errorback ()
  (lambda (error &rest _)
    (error "Failed to submit post: %S" error)))

(defun forge--maybe-restore-winconf (winconf)
  (when (and winconf
             (eq (window-configuration-frame winconf)
                 (selected-frame)))
    (set-window-configuration winconf)))

;;; Commands

(transient-define-prefix forge-post-menu ()
  "Dispatch a post creation command."
  [["Set"
    :if (lambda ()
          (and (forge-github-repository-p (forge-get-repository :tracked))
               (string-prefix-p "new-"
                                (file-name-nondirectory buffer-file-name))))
    ("-m" forge-new-topic-set-milestone)
    ("-l" forge-new-topic-set-labels)
    ("-a" forge-new-topic-set-assignees)
    ("-d" forge-new-pullreq-toggle-draft)]
   ["Actions"
    ("C-c" "Submit" forge-post-submit)
    ("C-k" "Cancel" forge-post-cancel)]])

(defun forge-post-submit ()
  "Submit the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (funcall forge--submit-post-function
           (forge-get-repository forge--buffer-post-object)
           forge--buffer-post-object))

(defun forge-post-cancel ()
  "Cancel the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (let ((winconf forge--pre-post-winconf))
    (when (yes-or-no-p "Also delete draft? ")
      (dired-delete-file buffer-file-name nil magit-delete-by-moving-to-trash))
    (magit-mode-bury-buffer 'kill)
    (forge--maybe-restore-winconf winconf)))

(defclass forge--new-topic-set-slot-command (transient-lisp-variable)
  ((name :initarg :name)
   (reader :initarg :reader)
   (formatter :initarg :formatter)
   (format :initform " %k %d")
   (description :initform (lambda (obj)
                            (with-slots (name variable formatter) obj
                              (if-let* ((value (symbol-value variable))
                                        (value (funcall formatter value)))
                                  (format "%s %s" name value)
                                (format "%s" name)))))))

(transient-define-infix forge-new-topic-set-milestone ()
  "Set milestone for the topic being created."
  :class 'forge--new-topic-set-slot-command
  :variable 'forge--buffer-milestone
  :name "milestone"
  :reader (lambda (&rest _) (forge-read-topic-milestone))
  :formatter (lambda (milestone) (propertize milestone 'face 'forge-topic-label))
  :if (lambda () (equal (file-name-nondirectory buffer-file-name) "new-issue")))

(transient-define-infix forge-new-topic-set-labels ()
  "Set labels for the topic being created."
  :class 'forge--new-topic-set-slot-command
  :variable 'forge--buffer-labels
  :name "labels"
  :reader (lambda (&rest _) (forge-read-topic-labels))
  :formatter (##forge--format-labels % t)
  :if (lambda () (equal (file-name-nondirectory buffer-file-name) "new-issue")))

(transient-define-infix forge-new-topic-set-assignees ()
  "Set assignees for the pull-request being created."
  :class 'forge--new-topic-set-slot-command
  :variable 'forge--buffer-assignees
  :name "assignees"
  :reader (lambda (&rest _) (forge-read-topic-assignees))
  :formatter #'forge--format-topic-assignees
  :if (lambda () (equal (file-name-nondirectory buffer-file-name) "new-issue")))

(transient-define-infix forge-new-pullreq-toggle-draft ()
  "Toggle whether the pull-request being created is a draft."
  :class 'forge--new-topic-set-slot-command
  :variable 'forge--buffer-draft-p
  :name "draft"
  :reader (lambda (&rest _) (not forge--buffer-draft-p))
  :description (lambda ()
                 (format (propertize "[%s]" 'face 'transient-delimiter)
                         (propertize "draft" 'face
                                     (if forge--buffer-draft-p
                                         'transient-value
                                       'transient-inactive-value))))
  :if (lambda () (equal (file-name-nondirectory buffer-file-name) "new-pullreq")))

;;; Notes

(defclass forge-note (forge-post) ())

(defvar-keymap forge-note-section-map
  "<remap> <magit-edit-thing>" #'forge-edit-topic-note)

(defun forge--save-note (_repo topic)
  (let ((value (string-trim (buffer-str))))
    (oset topic note (if (equal value "") nil value)))
  (delete-file buffer-file-name t)
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (cddr (directory-files dir nil nil t))
      (delete-directory dir)))
  (let ((prevbuf forge--pre-post-buffer))
    (magit-mode-bury-buffer 'kill)
    (forge-refresh-buffer prevbuf)))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"          . "cond-let--and$")
;;   ("and-let"       . "cond-let--and-let")
;;   ("if-let"        . "cond-let--if-let")
;;   ("when-let"      . "cond-let--when-let")
;;   ("buffer-string" . "buffer-string")
;;   ("buffer-str"    . "forge--buffer-substring-no-properties"))
;; End:
(provide 'forge-post)
;;; forge-post.el ends here
