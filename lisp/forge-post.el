;;; forge-post.el --- forge post support          -*- lexical-binding: t -*-

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

;;; Class

(defclass forge-post (forge-object) () :abstract t)

;;; Query

(cl-defmethod forge-get-parent ((post forge-post))
  (forge-get-topic post))

(cl-defmethod forge-get-repository ((post forge-post))
  (forge-get-repository (forge-get-topic post)))

;;; Sections

(defun forge-post-at-point ()
  (magit-section-when (issue pullreq post)))

(defvar magit-post-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-post)
    ;;(define-key map [remap magit-edit-thing]   'forge-edit-post)
    map))

;;; Mode

(defvar forge-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'forge-post-submit)
    (define-key map (kbd "C-c C-k") 'forge-post-cancel)
    map))

(define-derived-mode forge-post-mode gfm-mode "Forge-Post" "")

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
          (buf (find-file-noselect file)))
      (with-current-buffer buf
        (forge-post-mode)
        (setq forge--pre-post-buffer prevbuf))
      buf)))

(defun forge--display-post-buffer (buf)
  (magit-display-buffer buf))

(defun forge-post-cancel ()
  (interactive)
  (save-buffer)
  (if-let ((fn forge--submit-post-function))
      (funcall fn forge--buffer-post-object)
    (magit-mode-bury-buffer 'kill)))

(defun forge-post-submit ()
  (interactive)
  (save-buffer)
  (if-let ((fn forge--submit-post-function))
      (funcall fn
               (forge-get-repository forge--buffer-post-object)
               forge--buffer-post-object)
    (error "forge--submit-post-function is nil")))

(defun forge--post-submit-callback ()
  (let ((file    buffer-file-name)
        (editbuf (current-buffer))
        (prevbuf forge--pre-post-buffer))
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
        (forge-pull)))))

(defun forge--post-submit-errorback ()
  (lambda (&rest _)
    (error "Failed to submit post")))

;;; _
(provide 'forge-post)
;;; forge-post.el ends here
