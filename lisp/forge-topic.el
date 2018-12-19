;;; forge-topic.el --- Topics support              -*- lexical-binding: t -*-

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

(require 'bug-reference)
(require 'markdown-mode)

(require 'forge)
(require 'forge-post)

;;; Options

(defcustom forge-topic-list-order '(updated . string>)
  "Order of topics listed in the status buffer.

The value has the form (SLOT . PREDICATE), where SLOT is a
slot of issue or pullreq objects, and PREDICATE is a function
used to order the topics by that slot.  Reasonable values
include (number . >) and (updated . string>)."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type '(cons (symbol   :tag "Slot")
               (function :tag "Predicate")))

(defcustom forge-topic-list-limit '(60 . 5)
  "Limit the number of topics listed in the status buffer.

All unread topics are always shown.  If the value of this option
has the form (OPEN . CLOSED), then the integer OPEN specifies the
maximal number of topics and CLOSED specifies the maximal number
of closed topics.  The value can also be an integer, in which
case it limits the number of closed topics only."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type '(choice (number :tag "Maximal number of closed issues")
                 (cons (number :tag "Maximal number of closed issues")
                       (number :tag "Maximal number of open issues"))))

(defcustom forge-post-heading-format "%a %C\n"
  "Format for post headings in topic view.

The following %-sequences are supported:

`%a' The forge nickname of the author.
`%c' The absolute creation date.
`%C' The relative creation date."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'string)

(defcustom forge-post-fill-region t
  "Whether to call `fill-region' before displaying forge posts."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'boolean)

;;; Faces

(defface forge-topic-unread
  '((t :inherit bold))
  "Face used for title unread topics."
  :group 'forge-faces)

(defface forge-topic-closed
  '((t :inherit magit-dimmed))
  "Face used for title of unread topics."
  :group 'forge-faces)

(defface forge-topic-open
  '((t :inherit default))
  "Face used for title of open topics."
  :group 'forge-faces)

(defface forge-topic-merged
  '((t :inherit magit-dimmed))
  "Face used for number of merged topics."
  :group 'forge-faces)

(defface forge-topic-unmerged
  '((t :inherit magit-dimmed :slant italic))
  "Face used for number of unmerged topics."
  :group 'forge-faces)

(defface forge-post-author
  '((t :inherit bold))
  "Face used for post author in topic view."
  :group 'forge-faces)

(defface forge-post-date
  '((t :inherit italic))
  "Face used for post date in topic view."
  :group 'forge-faces)

;;; Class

(defclass forge-topic (forge-post) () :abstract t)

(cl-defmethod forge--object-id ((class (subclass forge-topic)) repo number)
  "Return the id of the specified topic."
  (base64-encode-string
   (format "%s:%s%s"
           (base64-decode-string (oref repo id))
           (substring (symbol-name class)
                      (length (oref-default class closql-class-prefix)))
           number)
   t))

(cl-defmethod forge--object-id ((type symbol) parent id)
  (base64-encode-string
   (format "%s:%s%s"
           (base64-decode-string parent)
           type
           (or (ignore-errors (base64-decode-string id)) id))
   t))

(cl-defmethod forge--object-id ((prefix string) id)
  (base64-encode-string
   (format "%s:%s"
           (base64-decode-string prefix)
           (or (ignore-errors (base64-decode-string id)) id))
   t))

;;; Query

(cl-defmethod forge-get-parent ((topic forge-topic))
  (forge-get-repository topic))

(cl-defmethod forge-get-repository ((topic forge-topic))
  (closql-get (forge-db)
              (oref topic repository)
              'forge-repository))

(cl-defmethod forge-list-recent-topics ((repo forge-repository) table)
  (let* ((id (oref repo id))
         (limit forge-topic-list-limit)
         (closed-limit (if (consp limit) (cdr limit) limit))
         (issues (forge-sql [:select * :from $s1
                             :where (and (= repository $s2)
                                         (notnull unread-p))]
                            table id)))
    (mapc (lambda (row)
            (cl-pushnew row issues :test #'equal))
          (if (consp limit)
              (forge-sql [:select * :from $s1
                          :where (and (= repository $s2)
                                      (isnull closed))
                          :order-by [(desc updated)]
                          :limit $s3]
                         table id (car limit))
            (forge-sql [:select * :from $s1
                        :where (and (= repository $s2)
                                    (isnull closed))]
                       table id)))
    (unless (zerop closed-limit)
      (mapc (lambda (row)
              (cl-pushnew row issues :test #'equal))
            (forge-sql [:select * :from $s1
                        :where (and (= repository $s2)
                                    (notnull closed))
                        :order-by [(desc updated)]
                        :limit $s3]
                       table id closed-limit)))
    (cl-sort (mapcar (lambda (row)
                       (closql--remake-instance 'forge-issue (forge-db) row))
                     issues)
             (cdr forge-topic-list-order)
             :key (lambda (it) (eieio-oref it (car forge-topic-list-order))))))

;;; Utilities

(cl-defmethod forge--format-url ((topic forge-topic) slot &optional spec)
  (forge--format-url (forge-get-repository topic) slot
                     `(,@spec (?i . ,(oref topic number)))))

(defun forge--sanitize-string (string)
  (replace-regexp-in-string "\r\n" "\n" string t t))

;;; Mode

(defvar forge-topic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'forge-create-post)
    (define-key map (kbd "C-c C-r") 'forge-create-post)
    (define-key map [remap magit-browse-thing] 'forge-browse-topic)
    map))

(define-derived-mode forge-topic-mode magit-mode "View Topic"
  "View a forge issue or pull-request.")

(defvar forge-topic-headers-hook
  '(forge-insert-topic-title
    forge-insert-topic-state
    forge-insert-topic-labels
    forge-insert-topic-assignees))

(defvar forge-post-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-post)
    (define-key map [remap magit-edit-thing]   'forge-edit-post)
    map))

(defun forge-topic-refresh-buffer (topic)
  (setq topic (closql-reload topic))
  (setcar magit-refresh-args topic)
  (magit-set-header-line-format
   (format "#%s: %s"
           (oref topic number)
           (oref topic title)))
  (magit-insert-section (topicbuf)
    (magit-insert-headers 'forge-topic-headers-hook)
    (dolist (post (cons topic (oref topic posts)))
      (with-slots (author created body) post
        (magit-insert-section section (post post)
          (oset section heading-highlight-face
                'magit-diff-hunk-heading-highlight)
          (let ((heading
                 (format-spec
                  forge-post-heading-format
                  `((?a . ,(propertize author  'face 'forge-post-author))
                    (?c . ,(propertize created 'face 'forge-post-date))
                    (?C . ,(propertize (apply #'format "%s %s ago"
                                              (magit--age
                                               (float-time
                                                (date-to-time created))))
                                       'face 'forge-post-date))))))
            (add-face-text-property 0 (length heading)
                                    'magit-diff-hunk-heading t heading)
            (magit-insert-heading heading))
          (insert (forge--fontify-markdown body) "\n\n"))))))

(defvar forge-topic-title-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-edit-thing] 'forge-edit-topic-title)
    map))

(cl-defun forge-insert-topic-title
    (&optional (topic (car magit-refresh-args)))
  (magit-insert-section (topic-title)
    (insert (format "%-11s" "Title: ") (oref topic title) "\n")))

(defvar forge-topic-state-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-edit-thing] 'forge-edit-topic-state)
    map))

(cl-defun forge-insert-topic-state
    (&optional (topic (car magit-refresh-args)))
  (magit-insert-section (topic-state)
    (insert (format "%-11s" "State: ")
            (symbol-name (oref topic state))
            "\n")))

(defvar forge-topic-labels-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-edit-thing] 'forge-edit-topic-labels)
    map))

(cl-defun forge-insert-topic-labels
    (&optional (topic (car magit-refresh-args)))
  (magit-insert-section (topic-labels)
    (insert (format "%-11s" "Labels: "))
    (if-let ((labels (closql--iref topic 'labels)))
        (insert (mapconcat (pcase-lambda (`(,name ,color ,_desc))
                             (propertize name 'face (list :box color)))
                           labels " "))
      (insert (propertize "none" 'face 'magit-dimmed)))
    (insert ?\n)))

(defvar forge-topic-assignees-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-edit-thing] 'forge-edit-topic-assignees)
    map))

(cl-defun forge-insert-topic-assignees
    (&optional (topic (car magit-refresh-args)))
  (magit-insert-section (topic-assignees)
    (insert (format "%-11s" "Assignees: "))
    (if-let ((assignees (closql--iref topic 'assignees)))
        (insert (mapconcat (pcase-lambda (`(,login ,name))
                             (format "%s (@%s)" name login))
                           assignees ", "))
      (insert (propertize "none" 'face 'magit-dimmed)))
    (insert ?\n)))

(defun forge-topic-buffer-name (_mode topic)
  (with-slots (owner name)
      (forge-get-repository topic)
    (format "*%s/%s #%d*" owner name (oref topic number))))

(defun forge--fontify-markdown (text)
  (with-temp-buffer
    (delay-mode-hooks
      (gfm-mode))
    (insert text)
    (font-lock-ensure)
    (when forge-post-fill-region
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun forge--topic-type-prefix (topic)
  (and (forge--childp (forge-get-repository topic) 'forge-gitlab-repository)
       (if (forge--childp topic 'forge-pullreq) "!" "#")))

(defun forge--topic-buffer-lock-value (args)
  (and (derived-mode-p 'forge-topic-mode)
       (let ((topic (car args)))
         (concat (forge--topic-type-prefix topic)
                 (number-to-string (oref topic number))))))

(add-hook 'magit-buffer-lock-functions 'forge--topic-buffer-lock-value)

;;; Completion

(defun forge-read-topic (prompt)
  (let* ((default (forge-current-topic))
         (repo    (forge-get-repository (or default t)))
         (choices (append (oref repo pullreqs)
                          (oref repo issues)))
         (choice  (magit-completing-read
                   prompt
                   (sort (mapcar #'forge--topic-format-choice choices)
                         #'string>)
                   nil nil nil nil
                   (and default
                        (forge--topic-format-choice default))))
         (number  (and (string-match "\\`\\([!#]*\\)\\([0-9]+\\)" choice)
                       (string-to-number (match-string 2 choice)))))
    (and number
         (if (equal (match-string 1 choice) "!")
             (forge-get-pullreq repo number)
           (or (forge-get-issue repo number)
               (forge-get-pullreq repo number))))))

(defun forge--topic-format-choice (topic)
  (format "%s%s  %s"
          (or (forge--topic-type-prefix topic) "")
          (oref topic number)
          (oref topic title)))

(defun forge-topic-completion-at-point ()
  (when-let ((repo (forge-get-repository t)))
    (when (and (or (not bug-reference-prog-mode)
	           (nth 8 (syntax-ppss))) ; inside comment or string
               (looking-back (if (forge--childp repo 'forge-gitlab-repository)
                                 "\\(?3:[!#]\\)\\(?2:[0-9]*\\)"
                               "#\\(?2:[0-9]*\\)")
                             (line-beginning-position)))
      (list (match-beginning 2)
            (match-end 0)
            (mapcar (lambda (row)
                      (propertize (number-to-string (car row))
                                  :title (format " %s" (cadr row))))
                    (if (forge--childp repo 'forge-gitlab-repository)
                        (forge-sql [:select [number title]
                                    :from $i1
                                    :where (= repository $s2)
                                    :order-by [(desc updated)]]
                                   (if (equal (match-string 3) "#")
                                       'issue
                                     'pullreq)
                                   (oref repo id))
                      (cl-sort
                       (nconc
                        (forge-sql [:select [number title updated]
                                    :from pullreq
                                    :where (= repository $s1)]
                                   (oref repo id))
                        (forge-sql [:select [number title updated]
                                    :from issue
                                    :where (= repository $s1)]
                                   (oref repo id)))
                       #'string> :key #'cl-caddr)))
            :annotation-function (lambda (c) (get-text-property 0 :title c))))))

;;; Bug-Reference

(defun bug-reference-fontify (start end)
  "Apply bug reference overlays to region."
  (save-excursion
    (let ((beg-line (progn (goto-char start) (line-beginning-position)))
	  (end-line (progn (goto-char end) (line-end-position))))
      ;; Remove old overlays.
      (bug-reference-unfontify beg-line end-line)
      (goto-char beg-line)
      (while (and (< (point) end-line)
		  (re-search-forward bug-reference-bug-regexp end-line 'move))
	(when (and (or (not bug-reference-prog-mode)
		       ;; This tests for both comment and string syntax.
		       (nth 8 (syntax-ppss)))
                   (not (and (derived-mode-p 'magit-status-mode
                                             'forge-notifications-mode)
                             (= (match-beginning 0)
                                (line-beginning-position)))))
	  (let ((overlay (make-overlay (match-beginning 0) (match-end 0)
	                               nil t nil)))
	    (overlay-put overlay 'category 'bug-reference)
	    ;; Don't put a link if format is undefined
	    (when bug-reference-url-format
              (overlay-put overlay 'bug-reference-url
                           (if (stringp bug-reference-url-format)
                               (format bug-reference-url-format
                                       (match-string-no-properties 2))
                             (funcall bug-reference-url-format))))))))))

(defun forge-bug-reference-setup ()
  (when-let ((repo (forge-get-repository 'full))
             (format (oref repo issue-url-format)))
    (unless bug-reference-url-format
      (setq-local bug-reference-url-format
                  (if (forge--childp repo 'forge-gitlab-repository)
                      (lambda ()
                        (forge--format-url repo
                                           (if (equal (match-string 3) "#")
                                               'issue-url-format
                                             'pullreq-url-format)
                                           `((?i . ,(match-string 2)))))
                    (forge--format-url repo 'issue-url-format '((?i . "%s")))))
      (setq-local bug-reference-bug-regexp
                  (if (forge--childp repo 'forge-gitlab-repository)
                      "\\(?3:[!#]\\)\\(?2:[0-9]+\\)"
                    "#\\(?2:[0-9]+\\)")))
    (if (derived-mode-p 'prog-mode)
        (bug-reference-prog-mode 1)
      (bug-reference-mode 1))
    (add-hook 'completion-at-point-functions
              'forge-topic-completion-at-point nil t)))

(unless noninteractive
  (add-hook 'find-file-hook        #'forge-bug-reference-setup)
  (add-hook 'git-commit-setup-hook #'forge-bug-reference-setup)
  (add-hook 'magit-mode-hook       #'forge-bug-reference-setup))

;;; _
(provide 'forge-topic)
;;; forge-topic.el ends here
