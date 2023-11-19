;;; forge-notify.el --- Notify support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;;; Options

(defcustom forge-notifications-github-kludge 'pending-again
  "The kludge used to work around Github's abysmal notification API."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type '(choice
          (const :tag "Abort pulling because not yet configured" nil)
          (const :tag "Every updated notification becomes unread"
                 always-unread)
          (const :tag "Unless unread, updated notifications become pending"
                 pending-again)
          (const :tag (concat "Unless unread or non-nil local status, "
                              "updated notifications become pending")
                 pending-if-unset)))

(defcustom forge-notifications-repo-slug-width 28
  "Width of repository slugs in `forge-notifications-mode' buffers."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type (if (>= emacs-major-version 28) 'natnum 'number))

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

;;; Special

(cl-defmethod forge-topic-mark-read ((_ forge-repository) topic)
  (oset topic status 'done))

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
      (and demand (user-error "No notication at point"))))

;;;; List

(defun forge--ls-notifications-all ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select * :from notification
                      :order-by [(desc updated)]])))

(defun forge--ls-notifications-pending ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select :distinct n:*
                      :from [(as notification n)
                             (as issue        i)
                             (as pullreq      p)]
                      :where (or (and (= n:topic i:id)
                                      (= i:status 'pending))
                                 (and (= n:topic i:id)
                                      (= p:status 'pending)))
                      :order-by [(desc n:updated)]])))

(defun forge--ls-notifications-unread ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select :distinct n:*
                      :from [(as notification n)
                             (as issue        i)
                             (as pullreq      p)]
                      :where (or (and (= n:topic i:id)
                                      (= i:status 'unread))
                                 (and (= n:topic i:id)
                                      (= p:status 'unread)))
                      :order-by [(desc n:updated)]])))

;;; Mode

(defvar-keymap forge-notifications-mode-map
  :doc "Keymap for `forge-notifications-mode'."
  :parent magit-mode-map
  "L" #'forge-notification-menu)

(define-derived-mode forge-notifications-mode magit-mode "Forge Notifications"
  "Mode for looking at forge notifications."
  (hack-dir-local-variables-non-file-buffer))

(defun forge-notifications-setup-buffer (&optional create)
  (let ((name "*forge-notifications*"))
    (if create
        ;; There should only ever be one such buffer.
        (cl-letf (((symbol-function 'magit-get-mode-buffer)
                   (lambda (&rest _)
                     (get-buffer-create name))))
          (magit-setup-buffer #'forge-notifications-mode nil
            (default-directory "/")
            (forge-buffer-unassociated-p t)))
      (get-buffer name))))

(defun forge-notifications-refresh-buffer ()
  (forge-insert-notifications))

(defvar forge-notifications-display-style 'flat)
(defvar forge-notifications-display-list-function
  #'forge--ls-notifications-all)

;;; Commands

(transient-define-prefix forge-notification-menu ()
  "Control list of notifications and notification at point."
  :transient-suffix t
  :transient-non-suffix t
  :transient-switch-frame nil
  :refresh-suffixes t
  [:hide always ("q" forge-menu-quit-list)]
  [["Type"
    ("t"   "topics...        " forge-topics-menu     :transient replace)
    (:info "notifications    " :face forge-active-suffix)
    ("r"   "repositories...  " forge-repository-menu :transient replace)
    ""]
   ["Filter"
    ("U" "unread"  forge-set-notifications-display-selection)
    ("P" "pending" forge-set-notifications-display-selection)
    ("A" "all"     forge-set-notifications-display-selection)]]
  [["Set status"
    ("u" forge-topic-status-set-unread)
    ("x" forge-topic-status-set-pending)
    ("d" forge-topic-status-set-done)
    ("s" forge-topic-toggle-saved)]
   ["Group"
    ("g" "by repository" forge-set-notifications-display-style)
    ("f" "flat list"     forge-set-notifications-display-style)]
   ["Margin"
    (magit-toggle-margin)
    (magit-cycle-margin-style)
    ("e" magit-toggle-margin-details)]]
  (interactive)
  (forge-list-notifications)
  (transient-setup 'forge-notification-menu))

;;;###autoload
(defun forge-list-notifications ()
  "List notifications."
  (interactive)
  (forge-notifications-setup-buffer t))

(transient-define-suffix forge-set-notifications-display-selection ()
  "Set the value of `forge-notifications-display-list-function' and refresh."
  (interactive)
  (setq forge-notifications-display-list-function
        (pcase-exhaustive (oref (transient-suffix-object) description)
          ("unread"  #'forge--ls-notifications-unread)
          ("pending" #'forge--ls-notifications-pending)
          ("all"     #'forge--ls-notifications-all)))
  (forge-refresh-buffer))

(transient-define-suffix forge-set-notifications-display-style ()
  "Set the value of `forge-notifications-display-style' and refresh."
  (interactive)
  (setq forge-notifications-display-style
        (pcase-exhaustive (oref (transient-suffix-object) description)
          ("flat list"     'flat)
          ("by repository" 'nested)))
  (forge-refresh-buffer))

;;; Sections

;; The double-prefix is necessary due to a limitation of magit-insert-section.
(defvar-keymap forge-forge-repo-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-this-repository
  "<remap> <magit-visit-thing>"  #'forge-visit-this-repository)

(defun forge-insert-notifications ()
  (when-let ((notifs (funcall forge-notifications-display-list-function)))
    (magit-insert-section (notifications)
      (magit-insert-heading
        (concat (pcase forge-notifications-display-list-function
                  ('forge--ls-notifications-all     "All")
                  ('forge--ls-notifications-pending "Pending")
                  ('forge--ls-notifications-unread  "Unread"))
                " notifications:"))
      (if (eq forge-notifications-display-style 'flat)
          (magit-insert-section-body
            (dolist (notif notifs)
              (forge-insert-notification notif))
            (insert ?\n))
        (pcase-dolist (`(,_ . ,notifs)
                       (--group-by (oref it repository) notifs))
          (let ((repo (forge-get-repository (car notifs))))
            (magit-insert-section (forge-repo repo)
              (magit-insert-heading
                (concat (propertize (format "%s/%s"
                                            (oref repo owner)
                                            (oref repo name))
                                    'font-lock-face 'bold)
                        (format " (%s)" (length notifs))))
              (magit-insert-section-body
                (dolist (notif notifs)
                  (forge-insert-notification notif))
                (insert ?\n)))))))))

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
                                        (! (eq (oref topic status) 'unread)))
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
