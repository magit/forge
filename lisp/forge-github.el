;;; forge-github.el --- github support            -*- lexical-binding: t -*-

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

(require 'treepy)

(require 'ghub)

(require 'forge)
(require 'forge-issue)
(require 'forge-pullreq)

;;; Variables

(defvar forge-github-token-scopes '(repo)
  "The Github API scopes needed by Magit.

`repo' is the only required scope.  Without this scope none of
Magit's features that use the API work.  Instead of this scope
you could use `public_repo' if you are only interested in public
repositories.

`repo' Grants read/write access to code, commit statuses,
  invitations, collaborators, adding team memberships, and
  deployment statuses for public and private repositories
  and organizations.

`public_repo' Grants read/write access to code, commit statuses,
  collaborators, and deployment statuses for public repositories
  and organizations. Also required for starring public
  repositories.")

;;; Class

(defclass forge-github-repository (forge-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#issuecomment-%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/pulls")
   (pullreq-url-format        :initform "https://%h/%o/%n/pull/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/pull/%i#issuecomment-%I")
   (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format         :initform "https://%h/%o/%n/commits/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/compare")
   (pullreq-refspec           :initform "+refs/pull/*/head:refs/pullreqs/*")))

;;; Pull
;;;; Repository

(cl-defmethod forge--pull ((repo forge-github-repository))
  (forge--pull-repository repo))

(cl-defmethod forge--pull-repository ((repo forge-github-repository)
                                      &optional skip-notifications)
  (let ((buffer (current-buffer)))
    (ghub-fetch-repository
     (oref repo owner)
     (oref repo name)
     (lambda (data)
       (message "Storing in forge database...")
       (emacsql-with-transaction (forge-db)
         (let-alist data
           (forge--update-repository repo data)
           (forge--update-assignees  repo .assignableUsers)
           (forge--update-forks      repo .forks)
           (forge--update-labels     repo .labels)
           (forge--update-issues     repo .issues t)
           (forge--update-pullreqs   repo .pullRequests t)
           (forge--update-revnotes   repo .commitComments))
         (message "Storing in forge database...done"))
       (unless skip-notifications
         (forge--pull-notifications (eieio-object-class repo)
                                    (oref repo githost)
                                    repo))
       (with-current-buffer
           (if (buffer-live-p buffer) buffer (current-buffer))
         (magit-refresh))
       (message "Pulling forge repository %s/%s...done"
                (oref repo owner)
                (oref repo name)))
     `((issues-until       . ,(forge--topics-until repo 'issue))
       (pullRequests-until . ,(forge--topics-until repo 'pullreq)))
     :auth 'forge)))

(cl-defmethod forge--topics-until ((repo forge-github-repository) table)
  (and (not (oref repo sparse-p))
       (caar (forge-sql [:select [updated] :from $s1
                         :where (= repository $s2)
		         :order-by [(desc updated)]
                         :limit 1]
                        table (oref repo id)))))

(cl-defmethod forge--update-repository ((repo forge-github-repository) data)
  (let-alist data
    (oset repo created        .createdAt)
    (oset repo updated        .updatedAt)
    (oset repo pushed         .pushedAt)
    (oset repo parent         .nameWithOwner)
    (oset repo description    .description)
    (oset repo homepage       .homepageUrl)
    (oset repo default-branch .defaultBranchRef.name)
    (oset repo archived-p     .isArchived)
    (oset repo fork-p         .isFork)
    (oset repo locked-p       .isLocked)
    (oset repo mirror-p       .isMirror)
    (oset repo private-p      .isPrivate)
    (oset repo stars          .stargazers.totalCount)
    (oset repo watchers       .watchers.totalCount)))

(cl-defmethod forge--update-issues ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (mapc (lambda (e) (forge--update-issue repo e bump)) data)))

(cl-defmethod forge--update-issue ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let* ((issue-id (forge--object-id 'forge-issue repo .number))
             (_ (magit-msg "Storing %s..." issue-id))
             (issue (or (forge-get-issue repo .number)
                        (closql-insert
                         (forge-db)
                         (forge-issue :id         issue-id
                                      :repository (oref repo id)
                                      :number     .number)))))
        (oset issue state      (intern (downcase .state)))
        (oset issue author     .author.login)
        (oset issue title      .title)
        (oset issue created    .createdAt)
        (when bump
          (oset issue updated  .updatedAt))
        (oset issue closed     .closedAt)
        (oset issue locked-p   .locked)
        (oset issue milestone  .milestone)
        (oset issue body       (forge--sanitize-string .body))
        (dolist (c .comments)
          (let-alist c
            (closql-insert
             (forge-db)
             (forge-issue-post
              :id      (forge--object-id issue-id .databaseId)
              :issue   issue-id
              :number  .databaseId
              :author  .author.login
              :created .createdAt
              :updated .updatedAt
              :body    (forge--sanitize-string .body))
             t)))
        (forge--set-id-slot repo issue 'assignees .assignees)
        (forge--set-id-slot repo issue 'labels .labels)
        issue))))

(cl-defmethod forge--update-pullreqs ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (mapc (lambda (e) (forge--update-pullreq repo e bump)) data)))

(cl-defmethod forge--update-pullreq ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .number))
             (_ (magit-msg "Storing %s..." pullreq-id))
             (pullreq (or (forge-get-pullreq repo .number)
                          (closql-insert
                           (forge-db)
                           (forge-pullreq :id           pullreq-id
                                          :repository   (oref repo id)
                                          :number       .number)))))
        (oset pullreq state        (intern (downcase .state)))
        (oset pullreq author       .author.login)
        (oset pullreq title        .title)
        (oset pullreq created      .createdAt)
        (when bump
          (oset pullreq updated    .updatedAt))
        (oset pullreq closed       .closedAt)
        (oset pullreq merged       .mergedAt)
        (oset pullreq locked-p     .locked)
        (oset pullreq editable-p   .maintainerCanModify)
        (oset pullreq cross-repo-p .isCrossRepository)
        (oset pullreq base-ref     .baseRef.name)
        (oset pullreq base-repo    .baseRef.repository.nameWithOwner)
        (oset pullreq head-ref     .headRef.name)
        (oset pullreq head-user    .headRef.repository.owner.login)
        (oset pullreq head-repo    .headRef.repository.nameWithOwner)
        (oset pullreq milestone    .milestone)
        (oset pullreq body         (forge--sanitize-string .body))
        (dolist (p .comments)
          (let-alist p
            (closql-insert
             (forge-db)
             (forge-pullreq-post
              :id      (forge--object-id pullreq-id .databaseId)
              :pullreq pullreq-id
              :number  .databaseId
              :author  .author.login
              :created .createdAt
              :updated .updatedAt
              :body    (forge--sanitize-string .body))
             t)))
        (forge--set-id-slot repo pullreq 'assignees .assignees)
        (forge--set-id-slot repo pullreq 'labels .labels)
        pullreq))))

(cl-defmethod forge--update-revnotes ((repo forge-github-repository) data)
  (emacsql-with-transaction (forge-db)
    (mapc (apply-partially 'forge--update-revnote repo) data)))

(cl-defmethod forge--update-revnote ((repo forge-github-repository) data)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (closql-insert
       (forge-db)
       (forge-revnote
        :id           (forge--object-id 'forge-revnote repo .id)
        :repository   (oref repo id)
        :commit       .commit.oid
        :file         .path
        :line         .position
        :author       .author.login
        :body         .body)
       t))))

(cl-defmethod forge--update-assignees ((repo forge-github-repository) data)
  (magit-msg "Storing assignees...")
  (oset repo assignees
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .login
                            .name)))
                  data))))

(cl-defmethod forge--update-forks ((repo forge-github-repository) data)
  (magit-msg "Storing forks...")
  (oset repo forks
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id (eieio-object-class repo)
                                              (oref repo githost)
                                              .owner.login
                                              .name)
                            .owner.login
                            .name)))
                  data))))

(cl-defmethod forge--update-labels ((repo forge-github-repository) data)
  (magit-msg "Storing labels...")
  (oset repo labels
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .name
                            (concat "#" (downcase .color))
                            .description)))
                  data))))

;;;; Notifications

(cl-defmethod forge--pull-notifications
  ((_class (subclass forge-github-repository))
   githost &optional (repo nil srepo))
  (message "Pulling forge notifications...")
  (emacsql-with-transaction (forge-db)
    (if repo
        (forge-sql [:delete-from notification :where (= repository $s1)]
                   (oref repo id))
      (forge-sql [:drop-table-if-exists notification])
      (forge-sql [:create-table notification $S1]
                 (cdr (assq 'notification forge--db-table-schemata)))))
  (let (pulled)
  (if-let ((spec (assoc githost forge-alist)))
      (pcase-let ((`(,_ ,apihost ,forge ,_) spec))
        (dolist (n (forge--ghub-get
                    nil (if repo
                            (format "/repos/%s/%s/notifications"
                                    (oref repo owner)
                                    (oref repo name))
                          "/notifications")
                    '((all . "true"))
                    :host apihost :unpaginate t))
          (let-alist n
            (let* ((type   (intern (downcase .subject.type)))
                   (type   (if (eq type 'pullrequest) 'pullreq type))
                   (repoid (list githost
                                 .repository.owner.login
                                 .repository.name))
                   (repo   (forge-get-repository repoid nil t))
                   (owner  (oref repo owner))
                   (name   (oref repo name))
                   (number (and (string-match "[0-9]*\\'" .subject.url)
                                (string-to-number
                                 (match-string 0 .subject.url)))))
              (cond
               ((oref repo sparse-p)
                (pcase type
                  ('issue
                   (if-let ((issue (forge-get-issue repo number)))
                       (oset issue unread-p .unread)
                     (when (or (not issue)
                               (string< (oref issue updated) .updated_at))
                       (message "Pulling forge issue %s/%s#%s..."
                                owner name number)
                       (ghub-fetch-issue
                        owner name number
                        (lambda (data)
                          (oset (forge--update-issue repo data)
                                unread-p .unread))
                        nil :auth 'forge))))
                  ('pullreq
                   (if-let ((pullreq (forge-get-pullreq repo number)))
                       (oset pullreq unread-p .unread)
                     (when (or (not pullreq)
                               (string< (oref pullreq updated) .updated_at))
                       (message "Pulling forge pullreq %s/%s#%s..."
                                owner name number)
                       (ghub-fetch-pullreq
                        owner name number
                        (lambda (data)
                          (oset (forge--update-pullreq repo data)
                                unread-p .unread))
                        nil :auth 'forge))))))
               ((not (or srepo (member repoid pulled)))
                (push repoid pulled)
                (forge--pull-repository repo)))
              (closql-insert
               (forge-db)
               (forge-notification
                :id           (forge--object-id (oref repo id) .id)
                :repository   (oref repo id)
                :forge        forge
                :reason       (intern (downcase .reason))
                :unread-p     .unread
                :last-read    .last_read_at
                :updated      .updated_at
                :title        .subject.title
                :type         type
                :topic        number
                :url          .subject.url)
               t)))))
    (error "No entry for %S in forge-alist" githost)))
  (message "Pulling forge notifications...done"))

;;; Mutations

(cl-defmethod forge--submit-create-pullreq ((_ forge-github-repository) _repo)
  (pcase-let* ((`(,title ,body) (forge--topic-title-and-body))
               (`(,base-remote . ,base-branch)
                (magit-split-branch-name forge--buffer-base-branch))
               (`(,head-remote . ,head-branch)
                (magit-split-branch-name forge--buffer-head-branch))
               (repo (forge-get-repository 'stub base-remote)))
    (forge--ghub-post repo "/repos/:owner/:repo/pulls"
                      `((title . ,title)
                        (body  . ,body)
                        (base  . ,base-branch)
                        (head  . ,(if (equal head-remote base-remote)
                                      head-branch
                                    (concat (oref repo name) ":" head-branch)))
                        (maintainer_can_modify . t))
                      :callback  (forge--post-submit-callback)
                      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-issue ((_ forge-github-repository) repo)
  (pcase-let ((`(,title ,body) (forge--topic-title-and-body)))
    (forge--ghub-post repo "/repos/:owner/:repo/issues"
                      `((title . ,title)
                        (body  . ,body))
                      :callback  (forge--post-submit-callback)
                      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-post ((_ forge-github-repository) topic)
  (forge--ghub-post topic "/repos/:owner/:repo/issues/:number/comments"
                    `((body . ,(string-trim (buffer-string))))
                    :callback  (forge--post-submit-callback)
                    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-edit-post ((_ forge-github-repository) post)
  (forge--ghub-patch post
                     (cl-typecase post
                       (forge-pullreq "/repos/:owner/:repo/pulls/:number")
                       (forge-issue   "/repos/:owner/:repo/issues/:number")
                       (forge-post    "/repos/:owner/:repo/issues/comments/:number"))
                     (if (cl-typep post 'forge-topic)
                         (-zip-pair '(title body)
                                    (forge--topic-title-and-body))
                       `((body . ,(string-trim (buffer-string)))))
                     :callback  (forge--post-submit-callback)
                     :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--set-topic-title
  ((_repo forge-github-repository) topic title)
  (forge--ghub-patch topic
                     "/repos/:owner/:repo/issues/:number"
                     `((title . ,title))
                     :callback (forge--ghub-set-callback)))

(cl-defmethod forge--set-topic-labels
  ((_repo forge-github-repository) topic labels)
  (forge--ghub-put topic "/repos/:owner/:repo/issues/:number/labels" nil
                   :payload labels
                   :callback (forge--ghub-set-callback)))

(cl-defmethod forge--set-topic-assignees
  ((_repo forge-github-repository) topic assignees)
  (let ((value (mapcar #'car (closql--iref topic 'assignees))))
    (when-let ((add (cl-set-difference assignees value :test #'equal)))
      (message "+ %S" (ghub--encode-payload add))
      (forge--ghub-post
       topic "/repos/:owner/:repo/issues/:number/assignees"
       `((assignees . ,add))))
    (when-let ((remove (cl-set-difference value assignees :test #'equal)))
      (message "- %S" (ghub--encode-payload remove))
      (forge--ghub-delete
       topic "/repos/:owner/:repo/issues/:number/assignees"
       `((assignees . ,remove)))))
  (forge-pull))

;;; Utilities

(cl-defun forge--ghub-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host
                               callback errorback)
  (ghub-get (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

(cl-defun forge--ghub-put (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host
                               callback errorback)
  (ghub-put (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

(cl-defun forge--ghub-post (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                host callback errorback)
  (ghub-post (forge--format-resource obj resource)
             params
             :host (or host (oref (forge-get-repository obj) apihost))
             :auth 'forge
             :query query :payload payload :headers headers
             :silent silent :unpaginate unpaginate
             :noerror noerror :reader reader
             :callback callback :errorback errorback))

(cl-defun forge--ghub-patch (obj resource
                                 &optional params
                                 &key query payload headers
                                 silent unpaginate noerror reader
                                 host callback errorback)
  (ghub-patch (forge--format-resource obj resource)
              params
              :host (or host (oref (forge-get-repository obj) apihost))
              :auth 'forge
              :query query :payload payload :headers headers
              :silent silent :unpaginate unpaginate
              :noerror noerror :reader reader
              :callback callback :errorback errorback))

(cl-defun forge--ghub-delete (obj resource
                                 &optional params
                                 &key query payload headers
                                 silent unpaginate noerror reader
                                 host callback errorback)
  (ghub-delete (forge--format-resource obj resource)
               params
               :host (or host (oref (forge-get-repository obj) apihost))
               :auth 'forge
               :query query :payload payload :headers headers
               :silent silent :unpaginate unpaginate
               :noerror noerror :reader reader
               :callback callback :errorback errorback))

(defun forge--ghub-set-callback ()
  (let ((buf (current-buffer)))
    (lambda (&rest _)
      (with-current-buffer
          (or buf (current-buffer))
        (forge-pull)))))

;;; _
(provide 'forge-github)
;;; forge-github.el ends here
