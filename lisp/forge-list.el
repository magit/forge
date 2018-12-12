;;; forge-list.el --- Tabulated-list interface     -*- lexical-binding: t -*-

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

(require 'forge)

;;; Options

(defcustom forge-topic-list-mode-hook '(hl-line-mode)
  "Hook run after entering Forge-Topic-List mode."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'hook
  :options '(hl-line-mode))

;;; Modes
;;;; Topic

(defvar forge-topic-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    map)
  "Local keymap for Forge-Topic-List mode buffers.")

(define-derived-mode forge-topic-list-mode tabulated-list-mode
  "Issues"
  "Major mode for browsing a list of topics."
  (setq x-stretch-cursor        nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "#" nil))
  (setq tabulated-list-format   (vconcat (--map `(,@(-take 3 it)
                                                  ,@(-flatten (nth 3 it)))
                                                forge-topic-list-columns)))
  (tabulated-list-init-header))

;;;; Issue

(defvar forge-issue-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map forge-topic-list-mode-map)
    (define-key map [return] 'forge-list-visit-issue)
    map)
  "Local keymap for Forge-Issue-List mode buffers.")

(define-derived-mode forge-issue-list-mode forge-topic-list-mode
  "Issues"
  "Major mode for browsing a list of issues.")

;;;; Pullreq

(defvar forge-pullreq-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map forge-topic-list-mode-map)
    (define-key map [return] 'forge-list-visit-pullreq)
    map)
  "Local keymap for Forge-Pullreq-List mode buffers.")

(define-derived-mode forge-pullreq-list-mode forge-topic-list-mode
  "Pull-Requests"
  "Major mode for browsing a list of pull-requests.")

;;; Commands
;;;; Issue

;;;###autoload
(defun forge-list-issues (repo)
  "List issues in a separate buffer."
  (interactive (list (forge-get-repository t)))
  (forge--list-topics 'forge-issue-list-mode "*Issues*"
    (forge-sql [:select $i1 :from issue :where (= repository $s2)]
               (forge--topic-list-columns-vector)
               (oref repo id))))

(defun forge-list-visit-issue ()
  "View the issue at point in a separate buffer."
  (interactive)
  (forge-visit-issue (forge-get-issue (tabulated-list-get-id))))

;;;; Pullreq

;;;###autoload
(defun forge-list-pullreqs (repo)
  "List pull-requests in a separate buffer."
  (interactive (list (forge-get-repository t)))
  (forge--list-topics 'forge-pullreq-list-mode "*Pull-Requests*"
    (forge-sql [:select $i1 :from pullreq :where (= repository $s2)]
               (forge--topic-list-columns-vector)
               (oref repo id))))

(defun forge-list-visit-pullreq ()
  "View the pull-request at point in a separate buffer."
  (interactive)
  (forge-visit-pullreq (forge-get-pullreq (tabulated-list-get-id))))

;;; Internal

(defun forge--list-topics (mode buffer-name rows)
  (declare (indent 2))
  (let ((topdir (magit-toplevel)))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq default-directory topdir)
      (funcall mode)
      (setq tabulated-list-entries
            (mapcar (lambda (row)
                      (list (car row)
                            (vconcat
                             (cl-mapcar (lambda (val col)
                                          (if-let ((pp (nth 5 col)))
                                              (funcall pp val)
                                            (if val (format "%s" val) "")))
                                        row forge-topic-list-columns))))
                    rows))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defvar forge-topic-list-columns
  '(("#" 5
     (lambda (a b)
       (> (car a) (car b)))
     (:right-align t) number nil)
    ("Title" 35 t nil title  nil)
    ))

(defun forge--topic-list-columns-vector (&optional qualify)
  (let ((lst (--map (nth 4 it) forge-topic-list-columns)))
    (vconcat (if qualify (-replace 'name 'packages:name lst) lst))))

;;; _
(provide 'forge-list)
;;; forge-list.el ends here
