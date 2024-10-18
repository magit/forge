;;; forge-notify.el --- Notify support  -*- lexical-binding:t -*-

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

(require 'forge)
(require 'forge-topic)

;;; Class

(defclass forge-notification (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-table              :initform 'notification)
   (closql-primary-key        :initform 'id)
   (closql-order-by           :initform [(desc id)])
   (id                        :initarg :id)
   (thread-id                 :initarg :thread-id)
   (repository                :initarg :repository)
   (type                      :initarg :type)
   (topic                     :initarg :topic)
   (url                       :initarg :url)
   (title                     :initarg :title)
   (reason                    :initarg :reason)
   (last-read                 :initarg :last-read)
   (updated                   :initarg :updated)))

;;; Query
;;;; Get

(cl-defmethod forge-get-repository ((notify forge-notification))
  "Return the object for the repository that NOTIFY belongs to."
  (and-let* ((id (oref notify repository)))
    (closql-get (forge-db) id 'forge-repository)))

(cl-defmethod forge-get-topic ((notify forge-notification))
  (and-let* ((repo (forge-get-repository notify)))
    (forge-get-topic repo (oref notify topic))))

(cl-defmethod forge-get-notification ((id string))
  (closql-get (forge-db) id 'forge-notification))

(cl-defmethod forge-get-notification ((topic forge-topic))
  (and-let* ((row (car (forge-sql [:select * :from notification
                                   :where (and (= repository $s1)
                                               (= topic $s2))]
                                  (oref topic repository)
                                  (oref topic number)))))
    (closql--remake-instance 'forge-notification (forge-db) row)))

;;;; Current

(defun forge-current-notification (&optional demand)
  "Return the current notification, casting a topic if necessary.
If there is no such notification and DEMAND is non-nil, then
signal an error."
  (or (magit-section-value-if 'notification)
      (and-let* ((topic (forge-current-topic)))
        (forge-get-notification topic))
      (and demand (user-error "No current notification"))))

(defun forge-notification-at-point (&optional demand)
  "Return the notification at point, casting a topic if necessary.
If there is no such notification and DEMAND is non-nil, then
signal an error."
  (or (magit-section-value-if 'notification)
      (and-let* ((topic (forge-topic-at-point)))
        (forge-get-notification topic))
      (and demand (user-error "No notification at point"))))

;;;; List

(defun forge--ls-notifications (status)
  (let* ((status (ensure-list status))
         (savedp (memq 'saved status))
         (status (remq 'saved status)))
    (mapcar
     (lambda (row) (closql--remake-instance 'forge-notification (forge-db) row))
     (if (seq-set-equal-p status '(unread pending done) #'eq)
         (forge-sql [:select * :from notification :order-by [(desc updated)]])
       (forge-sql
        `[:select :distinct notification:*
          :from [notification (as issue topic)]
          :where (and (= notification:topic topic:id)
                      ,@(and status '((in topic:status $v1)))
                      ,@(and savedp '((= topic:saved-p 't))))
          :union
          :select :distinct notification:*
          :from [notification (as pullreq topic)]
          :where (and (= notification:topic topic:id)
                      ,@(and status '((in topic:status $v1)))
                      ,@(and savedp '((= topic:saved-p 't))))
          :order-by [(desc notification:updated)]]
        (vconcat status))))))

;;; Mode

(defvar-keymap forge-notifications-mode-map
  :doc "Keymap for `forge-notifications-mode'."
  :parent (make-composed-keymap forge-common-map magit-mode-map)
  "<remap> <magit-refresh>"    #'magit-refresh-buffer
  "<remap> <forge--list-menu>" #'forge-notifications-menu)

(define-derived-mode forge-notifications-mode magit-mode "Forge Notifications"
  "Major mode for looking at forge notifications."
  :interactive nil
  (magit-hack-dir-local-variables))

(defun forge-notifications-setup-buffer ()
  (magit-setup-buffer-internal #'forge-notifications-mode nil
                               '((default-directory "/")
                                 (forge-buffer-unassociated-p t))
                               (get-buffer-create "*forge-notifications*")))

(defun forge-notifications-refresh-buffer ()
  (magit-set-header-line-format (forge-notifications-buffer-desc))
  (forge-insert-notifications))

(defun forge-notifications-buffer-desc ()
  (let ((status forge-notifications-selection))
    (cond
     ((not (listp status))
      (format "%s notifications" (capitalize (symbol-name status))))
     ((seq-set-equal-p status '(unread pending)) "Inbox")
     ((seq-set-equal-p status '(unread pending done)) "All notifications")
     ((format "Notifications %s" status)))))

(defvar forge-notifications-display-style 'flat)
(defvar forge-notifications-selection '(unread pending))

;;; Commands

(transient-define-prefix forge-notifications-menu ()
  "Control list of notifications and notification at point."
  :transient-suffix t
  :transient-non-suffix #'transient--do-call
  :transient-switch-frame nil
  :refresh-suffixes t
  :environment #'forge--menu-environment
  :column-widths forge--topic-menus-column-widths
  [:hide always ("q" forge-menu-quit-list)]
  [forge--topic-menus-group
   ["Selection"
    ("I" forge-notifications-display-inbox)
    ("S" forge-notifications-display-saved)
    ("D" forge-notifications-display-done)
    ("A" forge-notifications-display-all)]]
  [forge--lists-group
   ["Display"
    ("-F" forge-notifications-style-flat)
    ("-G" forge-notifications-style-nested)
    ("-H" forge-toggle-topic-legend)]]
  [forge--topic-legend-group]
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (forge-list-notifications))
  (transient-setup 'forge-notifications-menu))

(transient-augment-suffix forge-notifications-menu
  :transient #'transient--do-replace
  :if-mode 'forge-notifications-mode
  :inapt-if (lambda () (eq (oref transient--prefix command) 'forge-notifications-menu))
  :inapt-face 'forge-suffix-active)

;;;###autoload(autoload 'forge-list-notifications "forge-notify" nil t)
(transient-define-suffix forge-list-notifications ()
  "List notifications."
  :inapt-if-mode 'forge-notifications-mode
  :inapt-face 'forge-suffix-active
  (declare (interactive-only nil))
  (interactive)
  (forge-notifications-setup-buffer)
  (transient-setup 'forge-notifications-menu))

(transient-define-suffix forge-notifications-display-inbox ()
  "List unread and pending notifications."
  :description "inbox"
  :inapt-if (lambda () (equal forge-notifications-selection '(unread pending)))
  :inapt-face 'forge-suffix-active
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection '(unread pending))
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-display-saved ()
  "List saved notifications."
  :description "saved"
  :inapt-if (lambda () (eq forge-notifications-selection 'saved))
  :inapt-face 'forge-suffix-active
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection 'saved)
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-display-done ()
  "List done notifications."
  :description "done"
  :inapt-if (lambda () (eq forge-notifications-selection 'done))
  :inapt-face 'forge-suffix-active
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection 'done)
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-display-all ()
  "List all notifications."
  :description "all"
  :inapt-if (lambda () (equal forge-notifications-selection '(unread pending done)))
  :inapt-face 'forge-suffix-active
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection '(unread pending done))
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-style-flat ()
  "Show a flat notification list."
  :description "single list"
  :inapt-if (lambda () (eq forge-notifications-display-style 'flat))
  :inapt-face 'forge-suffix-active
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-display-style 'flat)
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-style-nested ()
  "Group notifications by repository."
  :description "group by repo"
  :inapt-if (lambda () (eq forge-notifications-display-style 'nested))
  :inapt-face 'forge-suffix-active
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-display-style 'nested)
  (forge-refresh-buffer))

;;; Sections

(defclass forge-repository-section (magit-section)
  ((type   :initform 'forge-repo)
   (keymap :initform 'forge-repository-section-map)))

(define-obsolete-variable-alias 'forge-forge-repo-section-map
  'forge-repository-section-map "Forge 0.4.0")

(defvar-keymap forge-repository-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-this-repository
  "<remap> <magit-visit-thing>"  #'forge-visit-this-repository)

(defun forge-insert-notifications ()
  (let ((notifs (forge--ls-notifications forge-notifications-selection)))
    (magit-insert-section (notifications)
      (cond
       ((not notifs)
        (insert "(empty)\n"))
       ((eq forge-notifications-display-style 'flat)
        (magit-insert-section-body
          (dolist (notif notifs)
            (forge-insert-notification notif))
          (insert ?\n)))
       ((pcase-dolist (`(,_ . ,notifs)
                       (--group-by (oref it repository) notifs))
          (let ((repo (forge-get-repository (car notifs))))
            (magit-insert-section (forge-repo repo)
              (magit-insert-heading
                (concat (propertize (oref repo slug) 'font-lock-face 'bold)
                        (format " (%s)" (length notifs))))
              (magit-insert-section-body
                (dolist (notif notifs)
                  (forge-insert-notification notif))
                (insert ?\n))))))))))

(defun forge-insert-notification (notif)
  (with-slots (type title url) notif
    (pcase type
      ((or 'issue 'pullreq)
       (forge--insert-topic (forge-get-topic notif)))
      ('commit
       (magit-insert-section (ncommit nil) ; !commit
         (string-match "[^/]*\\'" url)
         (insert
          (format "%s %s\n"
                  (propertize (substring (match-string 0 url)
                                         0 (magit-abbrev-length))
                              'font-lock-face 'magit-hash)
                  (magit-log-propertize-keywords
                   nil
                   (propertize title 'font-lock-face
                               (if-let ((topic (oref notif topic))
                                        ((eq (oref topic status) 'unread)))
                                   'forge-topic-unread
                                 'forge-topic-open)))))))
      (_
       ;; The documentation does not mention what "types"
       ;; exist.  Make it obvious that this is something
       ;; we do not know how to handle properly yet.
       (magit-insert-section (notification notif)
         (insert (propertize (format "(%s) %s\n" type title)
                             'font-lock-face 'error)))))))

;;; _
(provide 'forge-notify)
;;; forge-notify.el ends here
