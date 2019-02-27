;;; forge-topic.el --- Topics support              -*- lexical-binding: t -*-

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
                 (cons (number :tag "Maximal number of open issues")
                       (number :tag "Maximal number of closed issues"))))

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
  "Face used for title of unread topics."
  :group 'forge-faces)

(defface forge-topic-closed
  '((t :inherit magit-dimmed))
  "Face used for title of closed topics."
  :group 'forge-faces)

(defface forge-topic-open
  '((t :inherit default))
  "Face used for title of open topics."
  :group 'forge-faces)

(defface forge-topic-merged
  '((t :inherit magit-dimmed))
  "Face used for number of merged pull-requests."
  :group 'forge-faces)

(defface forge-topic-unmerged
  '((t :inherit magit-dimmed :slant italic))
  "Face used for number of unmerged pull-requests."
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
  "Return the id for a CLASS object in REPO identified by id NUMBER."
  (base64-encode-string
   (format "%s:%s%s"
           (base64-decode-string (oref repo id))
           (substring (symbol-name class)
                      (length (oref-default class closql-class-prefix)))
           number)
   t))

(cl-defmethod forge--object-id ((prefix string) id)
  (base64-encode-string
   (format "%s:%s"
           (base64-decode-string prefix)
           ;; TODO Simply use `id', which is always an integer, except
           ;; when called by `forge--update-labels(gitlab)', in which
           ;; case the string also shouldn't be decoded because it is
           ;; NOT base64 encoded.
           (or (ignore-errors (base64-decode-string id)) id))
   t))

;;; Query

(cl-defmethod forge-get-parent ((topic forge-topic))
  (forge-get-repository topic))

(cl-defmethod forge-get-repository ((topic forge-topic))
  (closql-get (forge-db)
              (oref topic repository)
              'forge-repository))

(cl-defmethod forge-get-topic ((topic forge-topic))
  topic)

(cl-defmethod forge-ls-recent-topics ((repo forge-repository) table)
  (let* ((id (oref repo id))
         (limit forge-topic-list-limit)
         (open-limit   (if (consp limit) (car limit) limit))
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
                         table id open-limit)
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
    (cl-sort (mapcar (let ((class (if (eq table 'pullreq)
                                      'forge-pullreq
                                    'forge-issue)))
                       (lambda (row)
                         (closql--remake-instance class (forge-db) row)))
                     issues)
             (cdr forge-topic-list-order)
             :key (lambda (it) (eieio-oref it (car forge-topic-list-order))))))

(cl-defmethod forge-ls-topics ((repo forge-repository) class &optional type)
  (mapcar (lambda (row)
            (closql--remake-instance class (forge-db) row))
          (let ((table (oref-default class closql-table))
                (id (oref repo id)))
            (pcase-exhaustive type
              (`open   (forge-sql [:select * :from $s1
                                   :where (and (= repository $s2)
                                               (isnull closed))
                                   :order-by [(desc number)]]
                                  table id))
              (`closed (forge-sql [:select * :from $s1
                                   :where (and (= repository $s2)
                                               (notnull closed))
                                   :order-by [(desc number)]]
                                  table id))
              (`nil    (forge-sql [:select * :from $s1
                                   :where (= repository $s2)
                                   :order-by [(desc number)]]
                                  table id))))))

;;; Utilities

(cl-defmethod forge--format ((topic forge-topic) slot &optional spec)
  (forge--format (forge-get-repository topic) slot
                 `(,@spec (?i . ,(oref topic number)))))

(cl-defmethod forge-visit ((topic forge-topic))
  (let ((magit-generate-buffer-name-function 'forge-topic-buffer-name))
    (magit-mode-setup-internal #'forge-topic-mode (list topic) t)))

(cl-defmethod forge-visit :after ((topic forge-topic))
  (oset topic unread-p nil))

(defun forge--sanitize-string (string)
  ;; For Gitlab this may also be nil.
  (if string
      (replace-regexp-in-string "\r\n" "\n" string t t)
    ""))

(defun forge-insert-topics (heading topics prefix)
  "Under a new section with HEADING, insert TOPICS."
  (when topics
    (let ((width (length (number-to-string (oref (car topics) number))))
          list-section-type topic-section-type)
      (cond ((forge--childp (car topics) 'forge-issue)
             (setq list-section-type  'issues)
             (setq topic-section-type 'issue))
            ((forge--childp (car topics) 'forge-pullreq)
             (setq list-section-type  'pullreqs)
             (setq topic-section-type 'pullreq)))
      (magit-insert-section ((eval list-section-type) nil t)
        (magit-insert-heading
          (format "%s (%s)"
                  (propertize heading 'face 'magit-section-heading)
                  (length topics)))
        (magit-insert-section-body
          (dolist (topic topics)
            (forge-insert-topic topic topic-section-type width prefix))
          (insert ?\n))))))

(defun forge-insert-topic (topic &optional topic-section-type width prefix)
  "Insert TOPIC as a new section.
If TOPIC-SECTION-TYPE is provided, it is the section type to use.
If WIDTH is provided, it is a fixed width to use for the topic
identifier."
  (unless topic-section-type
    (setq topic-section-type
          (cond ((forge--childp topic 'forge-issue) 'issue)
                ((forge--childp topic 'forge-pullreq) 'pullreq))))
  (magit-insert-section ((eval topic-section-type) topic t)
    (forge--insert-topic-contents topic width prefix)))

(cl-defmethod forge--format-topic-id ((topic forge-topic) &optional prefix)
  (propertize (format "%s%s"
                      (or prefix (forge--topic-type-prefix topic))
                      (oref topic number))
              'face 'magit-dimmed))

(cl-defmethod forge--insert-topic-contents ((topic forge-topic) width prefix)
  (with-slots (number title unread-p closed) topic
    (insert
     (format (if width
                 (format "%%-%is %%s%%s\n" (1+ width))
               "%s %s%s\n")
             (forge--format-topic-id topic prefix)
             (magit-log-propertize-keywords
              nil (propertize title 'face
                              (cond (unread-p 'forge-topic-unread)
                                    (closed   'forge-topic-closed)
                                    (t        'forge-topic-open))))
             (if-let ((labels (forge--format-topic-labels topic)))
                 (concat " " labels)
               "")))))

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
    (when (and (forge-pullreq-p topic)
               (not (oref topic merged)))
      (magit-insert-section (pullreq topic)
        (magit-insert-heading "Commits")
        (forge--insert-pullreq-commits topic)))
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
          (insert (forge--fontify-markdown body) "\n\n"))))
    (when (fboundp 'markdown-display-inline-images)
      (let ((markdown-display-remote-images t))
        (markdown-display-inline-images)))))

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
    (if-let ((labels (forge--format-topic-labels topic)))
        (insert labels)
      (insert (propertize "none" 'face 'magit-dimmed)))
    (insert ?\n)))

(defun forge--format-topic-labels (topic)
  (when-let ((labels (closql--iref topic 'labels)))
    (mapconcat (pcase-lambda (`(,name ,color ,_desc))
                 (propertize name 'face (list :box color)))
               labels " ")))

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

(cl-defmethod forge--topic-type-prefix ((_ forge-topic))
  "Get the identifier prefix specific to the type of TOPIC."
  "#")

(cl-defmethod forge--topic-type-prefix ((_repo forge-repository) _type)
  "#")

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
         (gitlabp (forge--childp repo 'forge-gitlab-repository))
         (choices (sort
                   (nconc
                    (let ((prefix (if gitlabp "!" "")))
                      (mapcar (lambda (topic)
                                (forge--topic-format-choice topic prefix))
                              (oref repo pullreqs)))
                    (let ((prefix (if gitlabp "#" "")))
                      (mapcar (lambda (topic)
                                (forge--topic-format-choice topic prefix))
                              (oref repo issues))))
                   #'string>))
         (choice  (magit-completing-read
                   prompt choices nil nil nil nil
                   (and default
                        (forge--topic-format-choice default))))
         (number  (and (string-match "\\`\\([!#]*\\)\\([0-9]+\\)" choice)
                       (string-to-number (match-string 2 choice)))))
    (and number
         (if (equal (match-string 1 choice) "!")
             (forge-get-pullreq repo number)
           (or (forge-get-issue repo number)
               (forge-get-pullreq repo number))))))

(defun forge--topic-format-choice (topic &optional prefix)
  (format "%s%s  %s"
          (or prefix (forge--topic-type-prefix topic) "")
          (oref topic number)
          (oref topic title)))

(defun forge-topic-completion-at-point ()
  (let ((bol (line-beginning-position))
        repo)
    (and (looking-back "[!#][0-9]*" bol)
         (or (not bug-reference-prog-mode)
	     (nth 8 (syntax-ppss))) ; inside comment or string
         (setq repo (forge-get-repository t))
         (looking-back (if (forge--childp repo 'forge-gitlab-repository)
                           "\\(?3:[!#]\\)\\(?2:[0-9]*\\)"
                         "#\\(?2:[0-9]*\\)")
                       bol)
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

;;; Parse

(defun forge--topic-parse-buffer (&optional file)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      ;; Unlike for issues, Github ignores the yaml front-matter for
      ;; pull-requests.  We just assume that nobody tries to use it
      ;; anyway.  If that turned out to be wrong, we would have to
      ;; deal with it by complicating matters around here.
      (let ((alist (or (and (forge--childp (forge-get-repository t)
                                           'forge-github-repository)
                            (save-excursion (forge--topic-parse-yaml)))
                       (save-excursion (forge--topic-parse-plain)))))
        (setf (alist-get 'file alist) file)
        (setf (alist-get 'text alist) (magit--buffer-string nil nil ?\n))
        (when (and file (not (alist-get 'prompt alist)))
          (setf (alist-get 'prompt alist)
                (file-name-sans-extension (file-name-nondirectory file))))
        ;; If there is a yaml front-matter, then it is supposed
        ;; to have a `title' field, but this may not be the case.
        (when (and (not file)
                   (not (alist-get 'title alist)))
          (setf (alist-get 'title alist)
                (read-string "Title: ")))
        alist))))

(defun forge--topic-parse-yaml ()
  (let (alist beg end)
    (when (looking-at "^---[\s\t]*$")
      (forward-line)
      (setq beg (point))
      (when (re-search-forward "^---[\s\t]*$" nil t)
        (setq end (match-end 0))
        (push (cons 'head (magit--buffer-string nil end ?\n)) alist)
        ;; Appending a newline would be correct, but Github does it
        ;; too, regardless of whether one is there already or not.
        (push (cons 'body (magit--buffer-string end nil t)) alist)
        (goto-char beg)
        (while (re-search-forward "^\\([a-zA-Z]*\\):[\s\t]\\(.+\\)" end t)
          (push (cons (intern (match-string-no-properties 1))
                      (let ((v (match-string-no-properties 2)))
                        ;; This works if the template generator was
                        ;; used, but yaml allows for other formats.
                        ;; Do you want to implement a yaml parser?
                        (cond
                         ((string-match "^\\([\"']\\)\\1[\s\t]*$" v) nil)
                         ((string-match "^\\([\"']\\)\\(.+\\)\\1[\s\t]*$" v)
                          (string-trim (match-string 2 v)))
                         ((string-match "," v)
                          (split-string v ",[\s\t]+"))
                         (t (string-trim v)))))
                alist))
        (let-alist alist
          (setf (alist-get 'prompt alist)
                (format "[%s] %s" .name .about))
          (when (and .labels (atom .labels))
            (setf (alist-get 'labels alist) (list .labels)))
          (when (and .assignees (atom .assignees))
            (setf (alist-get 'assignees alist) (list .assignees))))
        alist))))

(defun forge--topic-parse-plain ()
  (let (title body)
    (when (looking-at "\\`#*")
      (goto-char (match-end 0)))
    (setq title (magit--buffer-string (point) (line-end-position) t))
    (forward-line)
    (setq body (magit--buffer-string (point) nil ?\n))
    `((title . ,(string-trim title))
      (body  . ,(string-trim body)))))

;;; Templates

(cl-defgeneric forge--topic-templates (repo class)
  "Return a list of topic template files for REPO and a topic of CLASS.")

(cl-defgeneric forge--topic-template (repo class)
  "Return a topic template alist for REPO and a topic of CLASS.
If there are multiple templates, then the user is asked to select
one of them.  It there are no templates, then return a very basic
alist, containing just `text' and `position'.")

(cl-defmethod forge--topic-template ((repo forge-repository)
                                     (class (subclass forge-topic)))
  (let* ((branch  (oref repo default-branch))
         (choices (mapcar (lambda (f)
                            (with-temp-buffer
                              (magit-git-insert "cat-file" "-p"
                                                (concat branch ":" f))
                              (forge--topic-parse-buffer f)))
                          (forge--topic-templates repo class)))
         (choice  (if (cdr choices)
                      (--first (equal (alist-get 'prompt it)
                                      (magit-completing-read
                                       (if (eq class 'forge-pullreq)
                                           "Select pull-request template"
                                         "Select issue template")
                                       (--map (alist-get 'prompt it) choices)
                                       nil t))
                               choices)
                    (car choices))))
    (cond ((assq 'name choice)
           (when (string-match "^title: .?" (alist-get 'text choice))
             (setf (alist-get 'position choice) (match-end 0))))
          (choice
           (let ((text (alist-get 'text choice)))
             (if (string-match "\\`#+[\s\t]+.?" text)
                 (setf (alist-get 'position choice) (match-end 0))
               (setf (alist-get 'text choice) (concat "# \n\n" text))
               (setf (alist-get 'position choice) 3))))
          (t (setq choice '((text . "# \n\n\n") (position . 3)))))
    choice))

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
  (magit--with-safe-default-directory nil
    (when-let ((repo (forge-get-repository 'full))
               (format (oref repo issue-url-format)))
      (unless bug-reference-url-format
        (setq-local bug-reference-url-format
                    (if (forge--childp repo 'forge-gitlab-repository)
                        (lambda ()
                          (forge--format repo
                                         (if (equal (match-string 3) "#")
                                             'issue-url-format
                                           'pullreq-url-format)
                                         `((?i . ,(match-string 2)))))
                      (forge--format repo 'issue-url-format '((?i . "%s")))))
        (setq-local bug-reference-bug-regexp
                    (if (forge--childp repo 'forge-gitlab-repository)
                        "\\(?3:[!#]\\)\\(?2:[0-9]+\\)"
                      "#\\(?2:[0-9]+\\)")))
      (if (derived-mode-p 'prog-mode)
          (bug-reference-prog-mode 1)
        (bug-reference-mode 1))
      (add-hook 'completion-at-point-functions
                'forge-topic-completion-at-point nil t))))

(unless noninteractive
  (add-hook 'find-file-hook        #'forge-bug-reference-setup)
  (add-hook 'git-commit-setup-hook #'forge-bug-reference-setup)
  (add-hook 'magit-mode-hook       #'forge-bug-reference-setup))

;;; _
(provide 'forge-topic)
;;; forge-topic.el ends here
