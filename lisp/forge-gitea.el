;;; forge-gitea.el --- Gitea support              -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

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

(require 'gtea)
(require 'forge)

;;; Class

(defclass forge-gitea-repository (forge-unusedapi-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   ;; The anchor for the issue itself is .../%i#issue-%i
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#issuecomment-%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/pulls")
   (pullreq-url-format        :initform "https://%h/%o/%n/pulls/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/pulls/%i#issuecomment-%I")
   (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format         :initform "https://%h/%o/%n/commits/branch/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/pulls") ; sic
   (pullreq-refspec :initform "+refs/pull/*/head:refs/pullreqs/*")))


;;;; Notifications

(cl-defmethod forge--pull-notifications
  ((_class (subclass forge-gitea-repository)) githost &optional callback)
  (let ((spec (assoc githost forge-alist)))
    (unless spec
      (error "No entry for %S in forge-alist" githost))
    (forge--msg nil t nil "Pulling notifications")
    (pcase-let*
        ((`(,_ ,apihost ,forge ,_) spec)
         (notifs (-keep (lambda (data)
                          (forge--gtea-massage-notification
                           data forge githost))
                        (forge--gtea-get nil "/notifications"
                          '((all . "true"))
                          :host apihost :unpaginate t))))
      (forge--msg nil t nil "Storing notifications")
      (emacsql-with-transaction (forge-db)
        (forge-sql [:delete-from notification
                                 :where (= forge $s1)] forge)
        (dolist (obj notifs)
          (closql-insert (forge-db) obj)
          (forge--zap-repository-cache (forge-get-repository obj))))))
  (when callback
    (funcall callback)))


(defun forge--gtea-massage-notification (data forge githost)
  (let-alist data
    (let* ((type (intern (downcase .subject.type)))
           (type (if (eq type 'pull) 'pullreq type)))
      (and (memq type '(pullreq issue))
           (let* ((number (and (string-match "[0-9]*\\'" .subject.url)
                               (string-to-number (match-string 0 .subject.url))))
                  (repo   (forge-get-repository
                           (list githost
                                 .repository.owner.login
                                 .repository.name)
                           nil 'create))
                  (repoid (oref repo id))
                  (owner  (oref repo owner))
                  (name   (oref repo name))
                  (id     (forge--object-id repoid .id)))
             (forge-notification
              :id           id
              :thread-id    .id
              :repository   repoid
              :forge        forge
              :unread-p     .unread
              :last-read    .last_read_at
              :updated      .updated_at
              :title        .subject.title
              :type         type
              :topic        number
              :url          .subject.url))))))

;;; Utilities

(cl-defun forge--gtea-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host
                               callback errorback)
  (declare (indent defun))
  (gtea-get (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

(cl-defun forge--gtea-put (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host
                               callback errorback)
  (declare (indent defun))
  (gtea-put (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

(cl-defun forge--gtea-post (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                host callback errorback)
  (declare (indent defun))
  (gtea-post (forge--format-resource obj resource)
             params
             :host (or host (oref (forge-get-repository obj) apihost))
             :auth 'forge
             :query query :payload payload :headers headers
             :silent silent :unpaginate unpaginate
             :noerror noerror :reader reader
             :callback callback :errorback errorback))

(cl-defun forge--gtea-patch (obj resource
                                 &optional params
                                 &key query payload headers
                                 silent unpaginate noerror reader
                                 host callback errorback)
  (declare (indent defun))
  (gtea-patch (forge--format-resource obj resource)
              params
              :host (or host (oref (forge-get-repository obj) apihost))
              :auth 'forge
              :query query :payload payload :headers headers
              :silent silent :unpaginate unpaginate
              :noerror noerror :reader reader
              :callback callback :errorback errorback))

(cl-defun forge--gtea-delete (obj resource
                                  &optional params
                                  &key query payload headers
                                  silent unpaginate noerror reader
                                  host callback errorback)
  (declare (indent defun))
  (gtea-delete (forge--format-resource obj resource)
               params
               :host (or host (oref (forge-get-repository obj) apihost))
               :auth 'forge
               :query query :payload payload :headers headers
               :silent silent :unpaginate unpaginate
               :noerror noerror :reader reader
               :callback callback :errorback errorback))

;;; _
(provide 'forge-gitea)
;;; forge-gitea.el ends here
