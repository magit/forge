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
  "Hook run after entering Forge-Post mode."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type 'hook
  :options '(visual-line-mode
             turn-on-flyspell))

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
  "C-c C-e"                                #'forge-post-dispatch
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

(defvar-local forge--submit-post-function nil)
(defvar-local forge--cancel-post-function nil)

(defvar-local forge--buffer-post-object nil)
(defvar-local forge--buffer-category nil)
(defvar-local forge--buffer-base-branch nil)
(defvar-local forge--buffer-head-branch nil)
(defvar-local forge--buffer-draft-p nil)

(defun forge--prepare-post-buffer (filename header &optional template)
  (let* ((prevbuf (current-buffer))
         (repo    (forge-get-repository :tracked))
         (file    (forge--post-expand-file-name filename repo))
         (_       (make-directory (file-name-directory file) t))
         (buffer  (find-file-noselect file))
         (resume  (forge--post-resume-p file buffer)))
    (with-current-buffer buffer
      (forge-post-mode)
      (magit-set-header-line-format header)
      (setq forge--pre-post-buffer prevbuf)
      (when (and (not resume) template)
        (forge--post-insert-template template)
        (setq forge--buffer-draft-p (alist-get 'draft template)))
      (run-hooks 'forge-edit-post-hook))
    buffer))

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

(defun forge--post-insert-template (template)
  (let-alist template
    (cond
     (.name
      ;; A Github issue with yaml frontmatter.
      (save-excursion (insert .text))
      (unless (re-search-forward "^title: " nil t)
        (when (re-search-forward "^---" nil t 2)
          (beginning-of-line)
          (insert "title: \n")
          (backward-char))))
     (t
      (insert "# ")
      (let* ((source (alist-get 'source template))
             (target (alist-get 'target template))
             (single (and source
                          (= (car (magit-rev-diff-count source target)) 1))))
        (save-excursion
          (when single
            ;; A pull-request.
            (magit-rev-insert-format "%B" source))
          (when .text
            (if single
                (insert "-------\n")
              (insert "\n"))
            (insert "\n" .text))))))))

(defun forge--post-submit-callback ()
  (let* ((file    buffer-file-name)
         (editbuf (current-buffer))
         (prevbuf forge--pre-post-buffer)
         (topic   (ignore-errors (forge-get-topic forge--buffer-post-object)))
         (repo    (forge-get-repository topic)))
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
      (with-current-buffer
          (if (buffer-live-p prevbuf) prevbuf (current-buffer))
        (if (and topic
                 (forge--childp repo 'forge-github-repository)
                 (oref repo selective-p))
            (forge--pull-topic repo topic)
          (forge-pull))))))

(defun forge--post-submit-errorback ()
  (lambda (error &rest _)
    (error "Failed to submit post: %S" error)))

;;; Commands

(transient-define-prefix forge-post-dispatch ()
  "Dispatch a post creation command."
  ["Variables"
   ("d" "Create draft" forge-post-toggle-draft)]
  ["Act"
   ("C-c" "Submit" forge-post-submit)
   ("C-k" "Cancel" forge-post-cancel)])

(defun forge-post-submit ()
  "Submit the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--submit-post-function))
      (funcall fn
               (forge-get-repository forge--buffer-post-object)
               forge--buffer-post-object)
    (error "forge--submit-post-function is nil")))

(defun forge-post-cancel ()
  "Cancel the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--cancel-post-function))
      (funcall fn forge--buffer-post-object)
    (magit-mode-bury-buffer 'kill)))

(transient-define-infix forge-post-toggle-draft ()
  "Toggle whether the pull-request being created is a draft."
  :class 'transient-lisp-variable
  :variable 'forge--buffer-draft-p
  :reader (lambda (&rest _) (not forge--buffer-draft-p))
  :if (lambda () (equal (file-name-nondirectory buffer-file-name) "new-pullreq")))

;;; Notes

(defclass forge-note (forge-post) ())

(defvar-keymap forge-note-section-map
  "<remap> <magit-edit-thing>" #'forge-edit-topic-note)

(defun forge--save-note (_repo topic)
  (let ((value (string-trim (magit--buffer-string))))
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
;;   ("partial" . "llama--left-apply-partially")
;;   ("rpartial" . "llama--right-apply-partially"))
;; End:
(provide 'forge-post)
;;; forge-post.el ends here
