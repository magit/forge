;;; forge-list.el --- Tabulated-list interface     -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'forge)

;;; Options

(defcustom forge-topic-list-mode-hook '(hl-line-mode)
  "Hook run after entering Forge-Topic-List mode."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'hook
  :options '(hl-line-mode))

(defvar forge-topic-list-columns
  '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
    ("Title" 35 t nil title  nil)
    ))

(defvar forge-repository-list-columns
  '(("Owner"    20 t   nil owner nil)
    ("Name"     20 t   nil name  nil)
    ("N"         1 t   nil sparse-p nil)
    ("S"         1 t   nil selective-p nil)
    ("Worktree" 99 t   nil worktree nil)
    ))

;;; Modes
;;;; Topics

(defvar forge-topic-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'forge-visit-topic)
    (define-key map [return]    'forge-visit-topic)
    (define-key map (kbd "o")   'forge-browse-topic)
    (define-key map (kbd "'")   'forge-dispatch)
    (define-key map (kbd "?")   'magit-dispatch)
    map)
  "Local keymap for Forge-Topic-List mode buffers.")

(define-derived-mode forge-topic-list-mode tabulated-list-mode
  "Issues"
  "Major mode for browsing a list of topics."
  (setq-local x-stretch-cursor  nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "#" nil))
  (setq tabulated-list-format
        (vconcat (--map `(,@(-take 3 it)
                          ,@(-flatten (nth 3 it)))
                        forge-topic-list-columns)))
  (tabulated-list-init-header))

(define-derived-mode forge-issue-list-mode forge-topic-list-mode
  "Issues"
  "Major mode for browsing a list of issues.")

(define-derived-mode forge-pullreq-list-mode forge-topic-list-mode
  "Pull-Requests"
  "Major mode for browsing a list of pull-requests.")

;;;; Repository

(defvar forge-repository-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'forge-visit-repository)
    (define-key map [return]    'forge-visit-repository)
    (define-key map (kbd "o")   'forge-browse-repository)
    (define-key map (kbd "'")   'forge-dispatch)
    (define-key map (kbd "?")   'magit-dispatch)
    map)
  "Local keymap for Forge-Repository-List mode buffers.")

(define-derived-mode forge-repository-list-mode tabulated-list-mode
  "Repositories"
  "Major mode for browsing a list of repositories."
  (setq-local x-stretch-cursor  nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Owner" nil))
  (setq tabulated-list-format
        (vconcat (--map `(,@(-take 3 it)
                          ,@(-flatten (nth 3 it)))
                        forge-repository-list-columns)))
  (add-hook 'tabulated-list-revert-hook 'forge-repository-list-refresh nil t)
  (tabulated-list-init-header))

(defun forge-repository-list-refresh ()
  (setq tabulated-list-entries
        (mapcar (lambda (row)
                  (list (car row)
                        (vconcat
                         (cl-mapcar (lambda (val col)
                                      (if-let ((pp (nth 5 col)))
                                          (funcall pp val)
                                        (if val (format "%s" val) "")))
                                    (cdr row)
                                    forge-repository-list-columns))))
                (forge-sql [:select $i1 :from repository
                            :order-by [(asc owner) (asc name)]]
                           (forge--list-columns-vector
                            forge-repository-list-columns)))))

;;; Commands
;;;; Issue

;;;###autoload
(defun forge-list-issues (id)
  "List issues of the current repository in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge--list-topics id 'forge-issue-list-mode nil
    (forge-sql [:select $i1 :from issue :where (= repository $s2)]
               (forge--topic-list-columns-vector)
               id)))

;;;###autoload
(defun forge-list-assigned-issues (id)
  "List issues of the current repository that are assigned to you.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge--list-topics id 'forge-issue-list-mode nil
    (forge-sql
     [:select $i1 :from [issue issue_assignee assignee]
      :where (and (= issue_assignee:issue issue:id)
                  (= issue_assignee:id    assignee:id)
                  (= issue:repository     $s2)
                  (= assignee:login       $s3)
                  (isnull issue:closed))
      :order-by [(desc updated)]]
     (forge--topic-list-columns-vector)
     id (ghub--username (ghub--host)))))

;;;; Pullreq

;;;###autoload
(defun forge-list-pullreqs (id)
  "List pull-requests of the current repository in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge--list-topics id 'forge-pullreq-list-mode nil
    (forge-sql [:select $i1 :from pullreq :where (= repository $s2)]
               (forge--topic-list-columns-vector)
               id)))

;;;###autoload
(defun forge-list-assigned-pullreqs (id)
  "List pull-requests of the current repository that are assigned to you.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge--list-topics id 'forge-pullreq-list-mode nil
    (forge-sql
     [:select $i1 :from [pullreq pullreq_assignee assignee]
      :where (and (= pullreq_assignee:pullreq pullreq:id)
                  (= pullreq_assignee:id      assignee:id)
                  (= pullreq:repository       $s2)
                  (= assignee:login           $s3)
                  (isnull pullreq:closed))
      :order-by [(desc updated)]]
     (forge--topic-list-columns-vector)
     id (ghub--username (ghub--host)))))

;;;; Repository

;;;###autoload
(defun forge-list-repositories ()
  "List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database."
  (interactive)
  (with-current-buffer (get-buffer-create "*Forge Repositories*")
    (forge-repository-list-mode)
    (forge-repository-list-refresh)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

;;; Internal

(defun forge--list-topics (repo-id mode buffer-name rows)
  (declare (indent 2))
  (let ((repo (forge-get-repository (list :id repo-id)))
        (topdir (magit-toplevel)))
    (with-current-buffer
        (get-buffer-create
         (or buffer-name
             (format "*%s: %s/%s*"
                     (substring (symbol-name mode) 0 -5)
                     (oref repo owner)
                     (oref repo name))))
      (funcall mode)
      (setq forge-buffer-repository repo)
      (when topdir
        (setq default-directory topdir))
      (setq tabulated-list-entries
            (mapcar (lambda (row)
                      (list (car row)
                            (vconcat
                             (cl-mapcar (lambda (val col)
                                          (if-let ((pp (nth 5 col)))
                                              (funcall pp val)
                                            (if val (format "%s" val) "")))
                                        (cdr row)
                                        forge-topic-list-columns))))
                    rows))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun forge-topic-list-sort-by-number (a b)
  "Sort the `tabulated-list-entries' by topic number.
This assumes that `number' is the first column, otherwise
it silently fails."
  (ignore-errors
    (> (read (aref (cadr a) 0))
       (read (aref (cadr b) 0)))))

(defun forge--topic-list-columns-vector (&optional qualify)
  (forge--list-columns-vector forge-topic-list-columns qualify))

(defun forge--list-columns-vector (columns &optional qualify)
  (let ((lst (cons 'id (--map (nth 4 it) columns))))
    (vconcat (if qualify (-replace 'name 'packages:name lst) lst))))

;;; _
(provide 'forge-list)
;;; forge-list.el ends here
