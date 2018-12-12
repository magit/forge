;;; forge-post.el --- Post support                 -*- lexical-binding: t -*-

;; Copyright (C) 2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'markdown-mode)

(require 'forge)

;;; Options

(defcustom forge-post-mode-hook '(visual-line-mode)
  "Hook run after entering Forge-Post mode."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'hook
  :options '(visual-line-mode))

;;; Class

(defclass forge-post (forge-object) () :abstract t)

;;; Query

(cl-defmethod forge-get-parent ((post forge-post))
  (forge-get-topic post))

(cl-defmethod forge-get-repository ((post forge-post))
  (forge-get-repository (forge-get-topic post)))

;;; Sections

(defun forge-post-at-point ()
  (magit-section-value-if '(issue pullreq post)))

(defun forge-topic-at-point ()
  (or (magit-section-value-if '(issue pullreq))
      (when-let ((branch (magit-branch-at-point)))
        (when-let ((n (magit-get "branch" branch "pullRequest")))
          (forge-get-pullreq (string-to-number n))))
      (when-let ((rev (magit-commit-at-point)))
        (forge--pullreq-from-rev rev))))

(defun forge-current-topic ()
  (or (forge-topic-at-point)
      (and (derived-mode-p 'forge-topic-mode)
           (car magit-refresh-args))))

(defun forge--pullreq-from-rev (rev)
  (when-let ((repo    (forge-get-repository nil))
             (refspec (oref repo pullreq-refspec))
             (name    (magit-rev-name rev (cadr (split-string refspec ":")))))
    (save-match-data
      (when (string-match "[0-9]*\\'" name)
        (forge-get-pullreq (string-to-number (match-string 0 name)))))))

;;; Utilities

(cl-defmethod forge--format-url ((post forge-post) slot &optional spec)
  (forge--format-url (forge-get-topic post) slot
                     `(,@spec (?I . ,(oref post number)))))

(defun forge--topic-title-and-body ()
  (let (title body)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "\\`#* *")
        (goto-char (match-end 0)))
      (setq title (buffer-substring-no-properties (point) (line-end-position)))
      (forward-line)
      (when (looking-at "\n")
        (forward-line))
      (setq body (buffer-substring-no-properties (point) (point-max))))
    (list (string-trim title)
          (string-trim body))))

;;; Mode

(defvar forge-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'forge-post-submit)
    (define-key map (kbd "C-c C-k") 'forge-post-cancel)
    map))

(define-derived-mode forge-post-mode gfm-mode "Forge-Post" "")

(defvar-local forge--buffer-base-branch nil)
(defvar-local forge--buffer-head-branch nil)
(defvar-local forge--buffer-post-object nil)
(defvar-local forge--submit-post-function nil)
(defvar-local forge--cancel-post-function nil)
(defvar-local forge--pre-post-buffer nil)

(defun forge--prepare-post-buffer (filename)
  (let ((file (magit-git-dir
               (convert-standard-filename
                (concat "magit/posts/" filename)))))
    (make-directory (file-name-directory file) t)
    (let ((prevbuf (current-buffer))
          (resume (file-exists-p file))
          (buf (find-file-noselect file)))
      (with-current-buffer buf
        (forge-post-mode)
        (setq forge--pre-post-buffer prevbuf)
        (unless resume
          (pcase filename
            ("new-pullreq"
             (insert "# ")
             (save-excursion (insert "\n\n")))
            ("new-issue"
             (insert "# ")
             (save-excursion (insert "\n\n"))))))
      buf)))

(defun forge--display-post-buffer (buf)
  (magit-display-buffer buf #'display-buffer))

(defun forge-post-cancel ()
  "Cancel the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--cancel-post-function))
      (funcall fn forge--buffer-post-object)
    (magit-mode-bury-buffer 'kill)))

(defun forge-post-submit ()
  "Submit the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--submit-post-function))
      (funcall fn
               (forge-get-repository forge--buffer-post-object)
               forge--buffer-post-object)
    (error "forge--submit-post-function is nil")))

(defun forge--post-submit-callback ()
  (let* ((file    buffer-file-name)
         (editbuf (current-buffer))
         (prevbuf forge--pre-post-buffer)
         (topic   (ignore-errors (forge-get-topic forge--buffer-post-object)))
         (repo    (forge-get-repository topic)))
    (lambda (&rest _)
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
                 (fboundp 'forge-pullreq-p)
                 (forge-pullreq-p topic))
            (forge--pull-pullreq repo topic)
          (forge-pull))))))

(defun forge--post-submit-errorback ()
  (lambda (error &rest _)
    (error "Failed to submit post: %S" error)))

;;; _
(provide 'forge-post)
;;; forge-post.el ends here
