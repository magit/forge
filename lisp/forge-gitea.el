;;; forge-gitea.el --- Gitea support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2026 Jonas Bernoulli

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

(require 'forge-client)
(require 'forge-issue)
(require 'forge-pullreq)

;;; Class

(defclass forge-gitea-repository (forge-repository)
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
   (blob-url-format           :initform "https://%h/%o/%n/src/%r/%f")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/pulls") ; sic
   (pullreq-refspec :initform "+refs/pull/*/head:refs/pullreqs/*")))

;;; Pull
;;;; Repository

(cl-defmethod forge--pull ((repo forge-gitea-repository)
                           &optional callback since)
  (cl-assert (not (and since (forge-get-repository repo nil :tracked?))))
  (setq forge--mode-line-buffer (current-buffer))
  (forge--msg repo t nil "Pulling REPO")
  (let ((cb (let ((buf (current-buffer))
                  (val nil))
              (lambda (cb &optional v)
                (when v (if val (push v val) (setq val v)))
                (let-alist val
                  (cond
                    ((not val)
                     (forge--fetch-repository repo cb))
                    ((not (assq 'assignees val))
                     (forge--fetch-assignees repo cb))
                    ((not (assq 'labels val))
                     (forge--fetch-labels repo cb))
                    ((not (assq 'milestones val))
                     (forge--fetch-milestones repo cb))
                    ((and .has_issues
                          (not (assq 'issues val)))
                     (forge--fetch-issues repo cb since))
                    ((and .has_pull_requests
                          (not (assq 'pullreqs val)))
                     (forge--fetch-pullreqs repo cb since))
                    (t
                     (forge--msg repo t t   "Pulling REPO")
                     (forge--msg repo t nil "Storing REPO")
                     (closql-with-transaction (forge-db)
                       (forge--update-repository repo val)
                       (forge--update-assignees  repo .assignees)
                       (forge--update-labels     repo .labels)
                       (forge--update-milestones repo .milestones)
                       (dolist (v .issues)   (forge--update-issue repo v))
                       (dolist (v .pullreqs) (forge--update-pullreq repo v))
                       (oset repo condition :tracked))
                     (forge--msg repo t t "Storing REPO")
                     (cond
                       ((oref repo selective-p))
                       (callback (funcall callback))
                       ((forge--maybe-git-fetch repo buf))))))))))
    (funcall cb cb)))

(cl-defmethod forge--fetch-repository ((repo forge-gitea-repository) callback)
  (forge--gitea-get repo "/repos/:owner/:name" nil
    :callback (lambda (value _headers _status _req)
                (cond ((oref repo selective-p)
                       (setq value (append '((assignees) (labels)
                                             (milestones) (issues) (pullreqs))
                                           value)))
                      ((magit-get-boolean "forge.omitExpensive")
                       (setq value (append '((assignees) (labels)
                                             (milestones))
                                           value))))
                (funcall callback callback value))))

(cl-defmethod forge--update-repository ((repo forge-gitea-repository) data)
  (let-alist data
    (oset repo created        .created_at)
    (oset repo updated        (or .updated_at .updated))
    (oset repo pushed         nil)
    (oset repo parent         .parent.full_name)
    (oset repo description    .description)
    (oset repo homepage       (and (not (equal .website "")) .website))
    (oset repo default-branch .default_branch)
    (oset repo archived-p     .archived)
    (oset repo fork-p         .fork)
    (oset repo locked-p       nil)
    (oset repo mirror-p       .mirror)
    (oset repo private-p      .private)
    (oset repo issues-p       .has_issues)
    (oset repo wiki-p         .has_wiki)
    (oset repo stars          .stars_count)
    (oset repo watchers       .watchers_count)))

;;;; Topics

(cl-defmethod forge--pull-topic ((repo forge-gitea-repository) _topic
                                 &key callback _errorback)
  (forge--pull repo callback)) ; TODO Pull only the one topic.

;;;; Issues

(cl-defmethod forge--fetch-issues ((repo forge-gitea-repository) callback since)
  (let ((cb (let (val cur cnt pos)
              (lambda (cb &optional v)
                (cond
                  ((not pos)
                   (if (setq cur (setq val v))
                       (progn
                         (setq pos 1)
                         (setq cnt (length val))
                         (forge--msg nil nil nil "Pulling issue %s/%s" pos cnt)
                         (forge--fetch-issue-posts repo cur cb))
                     (forge--msg repo t t "Pulling REPO issues")
                     (funcall callback callback (cons 'issues val))))
                  (t
                   (if (setq cur (cdr cur))
                       (progn
                         (cl-incf pos)
                         (forge--msg nil nil nil "Pulling issue %s/%s" pos cnt)
                         (forge--fetch-issue-posts repo cur cb))
                     (forge--msg repo t t "Pulling REPO issues")
                     (funcall callback callback (cons 'issues val)))))))))
    (forge--msg repo t nil "Pulling REPO issues")
    (forge--gitea-get repo "/repos/:owner/:name/issues"
      `((limit . 50)
        (state . "all")
        (type . "issues")
        ,@(and-let ((after (or since (oref repo issues-until))))
            `((since . ,after))))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (funcall cb cb value)))))

(cl-defmethod forge--fetch-issue-posts ((repo forge-gitea-repository) cur cb)
  (let-alist (car cur)
    (forge--gitea-get repo
      (format "/repos/:owner/:name/issues/%s/comments" .number)
      '((limit . 50))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'comments (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-issue ((repo forge-gitea-repository) data)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((updated (or .updated_at .updated))
             (issue-id (forge--object-id 'forge-issue repo .number))
             (issue
              (forge-issue
               :id           issue-id
               :their-id     .number
               :number       .number
               :slug         (format "#%s" .number)
               :repository   (oref repo id)
               :state        (pcase-exhaustive .state
                               ("closed" 'completed)
                               ("open"   'open))
               :author       .user.login
               :title        .title
               :created      .created_at
               :updated      updated
               :closed       (or .closed_at (and (equal .state "closed") 1))
               :locked-p     .is_locked
               :milestone    (and .milestone.id
                                  (forge--object-id (oref repo id) .milestone.id))
               :body         (forge--sanitize-string .body))))
        (closql-insert (forge-db) issue t)
        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-connections repo issue 'assignees .assignees)
          (forge--set-connections repo issue 'labels .labels))
        (dolist (c .comments)
          (let-alist c
            (let ((post
                   (forge-issue-post
                    :id      (forge--object-id issue-id .id)
                    :issue   issue-id
                    :number  .id
                    :author  .user.login
                    :created .created_at
                    :updated (or .updated_at .updated)
                    :body    (forge--sanitize-string .body))))
              (closql-insert (forge-db) post t))))
        (let ((until (oref repo issues-until)))
          (when (or (not until) (and updated (string> updated until)))
            (oset repo issues-until updated)))
        issue))))

;;;; Pullreqs

(cl-defmethod forge--fetch-pullreqs ((repo forge-gitea-repository) callback _since)
  (let ((cb (let (val cur cnt pos)
              (lambda (cb &optional v)
                (cond
                  ((not pos)
                   (if (setq cur (setq val v))
                       (progn
                         (setq pos 1)
                         (setq cnt (length val))
                         (forge--msg nil nil nil "Pulling pullreq %s/%s" pos cnt)
                         (forge--fetch-pullreq-posts repo cur cb))
                     (forge--msg repo t t "Pulling REPO pullreqs")
                     (funcall callback callback (cons 'pullreqs val))))
                  (t
                   (if (setq cur (cdr cur))
                       (progn
                         (cl-incf pos)
                         (forge--msg nil nil nil "Pulling pullreq %s/%s" pos cnt)
                         (forge--fetch-pullreq-posts repo cur cb))
                     (forge--msg repo t t "Pulling REPO pullreqs")
                     (funcall callback callback (cons 'pullreqs val)))))))))
    (forge--msg repo t nil "Pulling REPO pullreqs")
    (forge--gitea-get repo "/repos/:owner/:name/pulls"
      '((limit . 50)
        (state . "all"))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (funcall cb cb value)))))

(cl-defmethod forge--fetch-pullreq-posts
  ((repo forge-gitea-repository) cur cb)
  (let-alist (car cur)
    (forge--gitea-get repo
      (format "/repos/:owner/:name/issues/%s/comments" .number)
      '((limit . 50))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'comments (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-pullreq ((repo forge-gitea-repository) data)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((updated (or .updated_at .updated))
             (merged (or .merged_at (and .merged 1)))
             (closed (or .closed_at (and (equal .state "closed") 1)))
             (pullreq-id (forge--object-id 'forge-pullreq repo .number))
             (pullreq
              (forge-pullreq
               :id           pullreq-id
               :their-id     .number
               :number       .number
               :slug         (format "#%s" .number)
               :repository   (oref repo id)
               :state        (pcase-exhaustive .state
                               ("closed" (if merged 'merged 'rejected))
                               ("open"   'open))
               :author       .user.login
               :title        .title
               :created      .created_at
               :updated      updated
               :closed       closed
               :merged       merged
               :draft-p      .draft
               :locked-p     .is_locked
               :editable-p   .allow_maintainer_edit
               :cross-repo-p (not (equal .base.repo.id .head.repo.id))
               :base-ref     .base.ref
               :base-rev     .base.sha
               :base-repo    .base.repo.full_name
               :head-ref     .head.ref
               :head-rev     .head.sha
               :head-user    .head.repo.owner.login
               :head-repo    .head.repo.full_name
               :milestone    (and .milestone.id
                                  (forge--object-id (oref repo id) .milestone.id))
               :body         (forge--sanitize-string .body))))
        (closql-insert (forge-db) pullreq t)
        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-connections repo pullreq 'assignees .assignees)
          (forge--set-connections repo pullreq 'review-requests .requested_reviewers)
          (forge--set-connections repo pullreq 'labels .labels))
        (dolist (c .comments)
          (let-alist c
            (let ((post
                   (forge-pullreq-post
                    :id      (forge--object-id pullreq-id .id)
                    :pullreq pullreq-id
                    :number  .id
                    :author  .user.login
                    :created .created_at
                    :updated (or .updated_at .updated)
                    :body    (forge--sanitize-string .body))))
              (closql-insert (forge-db) post t))))
        (let ((until (oref repo pullreqs-until)))
          (when (or (not until) (and updated (string> updated until)))
            (oset repo pullreqs-until updated)))
        pullreq))))

;;;; Other

(cl-defmethod forge--fetch-assignees ((repo forge-gitea-repository) callback)
  (forge--gitea-get repo "/repos/:owner/:name/assignees"
    '((limit . 50))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'assignees value)))))

(cl-defmethod forge--update-assignees ((repo forge-gitea-repository) data)
  (oset repo assignees
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .login
                            .full_name
                            .id)))
                  data))))

(cl-defmethod forge--fetch-labels ((repo forge-gitea-repository) callback)
  (forge--gitea-get repo "/repos/:owner/:name/labels"
    '((limit . 50))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'labels value)))))

(cl-defmethod forge--update-labels ((repo forge-gitea-repository) data)
  (oset repo labels
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .name
                            (forge--gitea-color .color)
                            .description)))
                  (delete-dups data)))))

(cl-defmethod forge--fetch-milestones ((repo forge-gitea-repository) callback)
  (forge--gitea-get repo "/repos/:owner/:name/milestones"
    '((limit . 50)
      (state . "all"))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'milestones value)))))

(cl-defmethod forge--update-milestones ((repo forge-gitea-repository) data)
  (oset repo milestones
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .id
                            .title
                            .created_at
                            (or .updated_at .updated)
                            .due_on
                            .closed_at
                            .description)))
                  (delete-dups data)))))

;;; Wrappers

(defun forge--gitea-forge-symbol (repo)
  (pcase-exhaustive (eieio-object-class-name repo)
    ('forge-forgejo-repository 'forgejo)
    ('forge-gitea-repository 'gitea)))

(defun forge--gitea-color (color)
  (and color
       (if (string-prefix-p "#" color)
           (downcase color)
         (concat "#" (downcase color)))))

(cl-defun forge--gitea-get (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                host callback errorback)
  (declare (indent defun))
  (let ((repo (forge-get-repository obj)))
    (ghub-request "GET" (if obj (forge--format-resource obj resource) resource)
      params
      :forge (forge--gitea-forge-symbol repo)
      :host (or host (oref repo apihost))
      :auth 'forge
      :query query :payload payload :headers headers
      :silent silent :unpaginate unpaginate
      :noerror noerror :reader reader
      :callback callback
      :errorback (or errorback (and callback t)))))

(cl-defun forge--gitea-post (obj resource
                                 &optional params
                                 &key query payload headers
                                 silent unpaginate noerror reader
                                 host callback errorback)
  (declare (indent defun))
  (let ((repo (forge-get-repository obj)))
    (ghub-request "POST" (forge--format-resource obj resource)
      params
      :forge (forge--gitea-forge-symbol repo)
      :host (or host (oref repo apihost))
      :auth 'forge
      :query query :payload payload :headers headers
      :silent silent :unpaginate unpaginate
      :noerror noerror :reader reader
      :callback callback
      :errorback (or errorback (and callback t)))))

(cl-defun forge--gitea-put (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                host callback errorback)
  (declare (indent defun))
  (let ((repo (forge-get-repository obj)))
    (ghub-request "PUT" (forge--format-resource obj resource)
      params
      :forge (forge--gitea-forge-symbol repo)
      :host (or host (oref repo apihost))
      :auth 'forge
      :query query :payload payload :headers headers
      :silent silent :unpaginate unpaginate
      :noerror noerror :reader reader
      :callback callback
      :errorback (or errorback (and callback t)))))

(cl-defun forge--gitea-patch (obj resource
                                  &optional params
                                  &key query payload headers
                                  silent unpaginate noerror reader
                                  host callback errorback)
  (declare (indent defun))
  (let ((repo (forge-get-repository obj)))
    (ghub-request "PATCH" (forge--format-resource obj resource)
      params
      :forge (forge--gitea-forge-symbol repo)
      :host (or host (oref repo apihost))
      :auth 'forge
      :query query :payload payload :headers headers
      :silent silent :unpaginate unpaginate
      :noerror noerror :reader reader
      :callback callback
      :errorback (or errorback (and callback t)))))

(cl-defun forge--gitea-delete (obj resource
                                   &optional params
                                   &key query payload headers
                                   silent unpaginate noerror reader
                                   host callback errorback)
  (declare (indent defun))
  (let ((repo (forge-get-repository obj)))
    (ghub-request "DELETE" (forge--format-resource obj resource)
      params
      :forge (forge--gitea-forge-symbol repo)
      :host (or host (oref repo apihost))
      :auth 'forge
      :query query :payload payload :headers headers
      :silent silent :unpaginate unpaginate
      :noerror noerror :reader reader
      :callback callback
      :errorback (or errorback (and callback t)))))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"          . "cond-let--and$")
;;   ("and>"          . "cond-let--and>")
;;   ("and-let"       . "cond-let--and-let")
;;   ("if-let"        . "cond-let--if-let")
;;   ("when-let"      . "cond-let--when-let"))
;; End:
(provide 'forge-gitea)
;;; forge-gitea.el ends here
