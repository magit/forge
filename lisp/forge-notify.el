;;; forge-notify.el --- Notify support             -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Jonas Bernoulli

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

;;; Class

(defclass forge-notification (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-table              :initform notification)
   (closql-primary-key        :initform id)
   (closql-order-by           :initform [(desc id)])
   (id                        :initarg :id)
   (thread-id                 :initarg :thread-id)
   (repository                :initarg :repository)
   (forge                     :initarg :forge)
   (reason                    :initarg :reason)
   (unread-p                  :initarg :unread-p)
   (last-read                 :initarg :last-read)
   (updated                   :initarg :updated)
   (title                     :initarg :title)
   (type                      :initarg :type)
   (topic                     :initarg :topic)
   (url                       :initarg :url)))

;;; Core

(cl-defmethod forge-get-repository ((notify forge-notification))
  "Return the object for the repository that NOTIFY belongs to."
  (when-let ((id (oref notify repository)))
    (closql-get (forge-db) id 'forge-repository)))

(cl-defmethod forge-get-notification ((topic forge-topic))
  (when-let ((row (car (forge-sql [:select * :from notification
				   :where (and (= repository $s1)
					       (= topic $s2))]
				  (oref topic repository)
				  (oref topic number)))))
    (closql--remake-instance 'forge-notification (forge-db) row t)))

;;; Utilities

(cl-defmethod forge-get-url ((notify forge-notification))
  (oref notify url))

;;; Mode

(defvar forge-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `forge-notifications-mode'.")

(define-derived-mode forge-notifications-mode magit-mode "Forge Notifications"
  "Mode for looking at forge notifications."
  (hack-dir-local-variables-non-file-buffer))

(defun forge-notifications-setup-buffer ()
  ;; There should only ever be one such buffer.
  (cl-letf (((symbol-function 'magit-get-mode-buffer)
             (lambda (&rest _)
               (get-buffer-create "*forge-notifications*"))))
    (magit-setup-buffer #'forge-notifications-mode)))

(defun forge-notifications-refresh-buffer ()
  (forge-insert-notifications))

;;; Utilities

(defun forge--list-notifications-all ()
  (closql-query (forge-db) nil nil 'forge-notification))

(defun forge--list-notifications-unread ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select * :from notification
                      :where (notnull unread-p)
                      :order-by [(desc id)]])))

;;; Sections

;; The double-prefix is necessary due to a limitation of magit-insert-section.
(defvar forge-forge-repo-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-repository)
    (define-key map [remap magit-visit-thing]  'forge-visit-repository)
    map))

(defun forge-insert-notifications ()
  (when-let ((ns (forge--list-notifications-all)))
    (magit-insert-section (notifications)
      (magit-insert-heading "Notifications:")
      (pcase-dolist (`(,_ . ,ns) (--group-by (oref it repository) ns))
        (let ((repo (forge-get-repository (car ns))))
          (magit-insert-section (forge-repo repo)
            (magit-insert-heading
              (propertize (format "%s/%s:" (oref repo owner) (oref repo name))
                          'font-lock-face 'bold))
            (dolist (notify ns)
              (with-slots (type topic title url unread-p) notify
                (pcase type
                  ('issue
                   (forge-insert-topic (forge-get-issue repo topic)))
                  ('pullreq
                   (forge-insert-topic (forge-get-pullreq repo topic)))
                  ('commit
                   (magit-insert-section (ncommit nil) ; !commit
                     (string-match "[^/]*\\'" url)
                     (insert
                      (format "%s %s\n"
                              (propertize (substring (match-string 0 url)
                                                     0 (magit-abbrev-length))
                                          'font-lock-face 'magit-hash)
                              (magit-log-propertize-keywords
                               nil (propertize title 'font-lock-face
                                               (if unread-p
                                                   'forge-topic-unread
                                                 'forge-topic-open)))))))
                  (_
                   ;; The documentation does not mention what "types"
                   ;; exist.  Make it obvious that this is something
                   ;; we do not know how to handle properly yet.
                   (magit-insert-section (notification notify)
                     (insert (propertize (format "(%s) %s\n" type title)
                                         'font-lock-face 'error)))))))
            (insert ?\n)))))))

;;; _
(provide 'forge-notify)
;;; forge-notify.el ends here
