;;; forge-github.el --- Github support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

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

(require 'ghub)

(require 'forge)
(require 'forge-discussion)
(require 'forge-issue)
(require 'forge-pullreq)

;;; Class

(defclass forge-github-repository (forge-repository)
  ((discussions-url-format     :initform "https://%h/%o/%n/discussions")
   (discussion-url-format      :initform "https://%h/%o/%n/discussions/%i")
   (discussion-post-url-format :initform "https://%h/%o/%n/discussions/%i#discussioncomment-%I")
   (issues-url-format          :initform "https://%h/%o/%n/issues")
   (issue-url-format           :initform "https://%h/%o/%n/issues/%i")
   (issue-post-url-format      :initform "https://%h/%o/%n/issues/%i#issuecomment-%I")
   (pullreqs-url-format        :initform "https://%h/%o/%n/pulls")
   (pullreq-url-format         :initform "https://%h/%o/%n/pull/%i")
   (pullreq-post-url-format    :initform "https://%h/%o/%n/pull/%i#issuecomment-%I")
   (commit-url-format          :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format          :initform "https://%h/%o/%n/commits/%r")
   (remote-url-format          :initform "https://%h/%o/%n")
   (blob-url-format            :initform "https://%h/%o/%n/blob/%r/%f")
   (create-issue-url-format    :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format  :initform "https://%h/%o/%n/compare")
   (pullreq-refspec            :initform "+refs/pull/*/head:refs/pullreqs/*")))

;;; Query

(defun forge--get-github-repository ()
  (forge-github-repository-p (forge-get-repository :stub?)))

;;; Pull
;;;; Repository

(cl-defmethod forge--pull ((repo forge-github-repository)
                           &optional callback since)
  (cl-assert (not (and since (forge-get-repository repo nil :tracked?))))
  (setq forge--mode-line-buffer (current-buffer))
  (forge--msg repo t nil "Pulling REPO")
  (let ((buf (current-buffer)))
    (ghub-fetch-repository
     (oref repo owner)
     (oref repo name)
     (lambda (data)
       (forge--msg repo t t   "Pulling REPO")
       (forge--msg repo t nil "Storing REPO")
       (closql-with-transaction (forge-db)
         (let-alist data
           (forge--update-repository  repo data)
           (forge--update-assignees   repo .assignableUsers)
           (forge--update-forks       repo .forks)
           (forge--update-labels      repo .labels)
           (forge--update-milestones  repo .milestones)
           (forge--update-discussion-categories repo .discussionCategories)
           (forge--update-discussions repo .discussions t)
           (forge--update-issues      repo .issues t)
           (forge--update-pullreqs    repo .pullRequests t)
           (forge--update-revnotes    repo .commitComments))
         (oset repo condition :tracked))
       (forge--msg repo t t   "Storing REPO")
       (cond
        ((oref repo selective-p))
        (callback (funcall callback))
        ((forge--maybe-git-fetch repo buf))))
     ;; Keys have the form `FIELD-until', where FIELD is the name of a
     ;; field of Repository objects.  See `ghub--graphql-walk-response'.
     `((discussions-until  . ,(or since (oref repo discussions-until)))
       (issues-until       . ,(or since (oref repo issues-until)))
       (pullRequests-until . ,(or since (oref repo pullreqs-until))))
     :host (oref repo apihost)
     :auth 'forge
     :sparse (oref repo selective-p))))

(cl-defmethod forge--update-repository ((repo forge-github-repository) data)
  (let-alist data
    (oset repo created        .createdAt)
    (oset repo updated        .updatedAt)
    (oset repo pushed         .pushedAt)
    (oset repo parent         .parent.nameWithOwner)
    (oset repo description    .description)
    (oset repo homepage       (and (not (equal .homepageUrl "")) .homepageUrl))
    (oset repo default-branch .defaultBranchRef.name)
    (oset repo archived-p     .isArchived)
    (oset repo fork-p         .isFork)
    (oset repo locked-p       .isLocked)
    (oset repo mirror-p       .isMirror)
    (oset repo private-p      .isPrivate)
    (oset repo issues-p       .hasIssuesEnabled)
    (oset repo discussions-p  .hasDiscussionsEnabled)
    (oset repo wiki-p         .hasWikiEnabled)
    (oset repo stars          .stargazers.totalCount)
    (oset repo watchers       .watchers.totalCount)
    (oset repo teams          (mapcar #'cdar .owner.teams))))

(cl-defmethod forge--update-revnotes ((repo forge-github-repository) data)
  (closql-with-transaction (forge-db)
    (mapc (##forge--update-revnote repo %) data)))

(cl-defmethod forge--update-revnote ((repo forge-github-repository) data)
  (closql-with-transaction (forge-db)
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
  (oset repo assignees
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .login
                            .name
                            .id)))
                  (delete-dups data)))))

(cl-defmethod forge--update-forks ((repo forge-github-repository) data)
  (oset repo forks
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (nconc (forge--repository-ids
                              (eieio-object-class repo)
                              (oref repo githost)
                              .owner.login
                              .name)
                             (list .owner.login
                                   .name))))
                  (delete-dups data)))))

(cl-defmethod forge--update-labels ((repo forge-github-repository) data)
  (oset repo labels
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .name
                            (concat "#" (downcase .color))
                            .description)))
                  (delete-dups data)))))

(cl-defmethod forge--update-milestones ((repo forge-github-repository) data)
  (oset repo milestones
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .number
                            .title
                            .createdAt
                            .updatedAt
                            .dueOn
                            .closedAt
                            .description)))
                  (delete-dups data)))))

(cl-defmethod forge--update-discussion-categories ((repo forge-github-repository) data)
  (oset repo discussion-categories
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .id
                            .name
                            .emoji
                            .isAnswerable
                            .description)))
                  (delete-dups data)))))

;;;; Topics

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (number number))
  (let ((id (oref repo id)))
    (forge--pull-topic
     repo
     (forge-issue :repository id :number number)
     :errorback
     (lambda (err _headers _status _req)
       (when (equal (cdr (assq 'type (cadr err))) "NOT_FOUND")
         (forge--pull-topic
          repo
          (forge-pullreq :repository id :number number)
          :errorback
          (lambda (err _headers _status _req)
            (when (equal (cdr (assq 'type (cadr err))) "NOT_FOUND")
              (forge--pull-topic
               repo
               (forge-discussion :repository id :number number))))))))))

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (topic forge-discussion)
                                 &key callback errorback)
  (let ((buffer (current-buffer)))
    (ghub-fetch-discussion
     (oref repo owner)
     (oref repo name)
     (oref topic number)
     (lambda (data)
       (forge--update-discussion repo data)
       (forge-refresh-buffer buffer)
       (when callback (funcall callback)))
     nil
     :host (oref repo apihost)
     :auth 'forge
     :errorback errorback)))

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (topic forge-issue)
                                 &key callback errorback)
  (let ((buffer (current-buffer)))
    (ghub-fetch-issue
     (oref repo owner)
     (oref repo name)
     (oref topic number)
     (lambda (data)
       (forge--update-issue repo data)
       (forge-refresh-buffer buffer)
       (when callback (funcall callback)))
     nil
     :host (oref repo apihost)
     :auth 'forge
     :errorback errorback)))

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (topic forge-pullreq)
                                 &key callback errorback)
  (let ((buffer (current-buffer)))
    (ghub-fetch-pullreq
     (oref repo owner)
     (oref repo name)
     (oref topic number)
     (lambda (data)
       (forge--update-pullreq repo data)
       (forge-refresh-buffer buffer)
       (when callback (funcall callback)))
     nil
     :host (oref repo apihost)
     :auth 'forge
     :errorback errorback)))

(cl-defmethod forge--update-status ((repo forge-github-repository)
                                    topic data bump initial-pull)
  (let-alist data
    (let ((updated (or .updatedAt .createdAt))
          (current-status (oref topic status)))
      (if (forge-discussion-p topic)
          ;; Discussions lack `isReadByViewer'.
          (cond (initial-pull
                 (oset topic status 'done))
                ((null current-status)
                 (oset topic status 'unread))
                ((string> updated (oref topic updated))
                 (oset topic status 'unread)))
        (cond ((not .isReadByViewer)
               (oset topic status 'unread))
              (initial-pull
               (oset topic status 'done))
              ((null current-status)
               (oset topic status 'pending))
              ((string> updated (oref topic updated))
               (oset topic status 'pending))))
      (oset topic updated updated)
      (when bump
        (let* ((slot (cl-typecase topic
                       (forge-discussion 'discussions-until)
                       (forge-issue      'issues-until)
                       (forge-pullreq    'pullreqs-until)))
               (until (eieio-oref repo slot)))
          (when (or (not until) (string> updated until))
            (eieio-oset repo slot updated)))))))

;;;; Discussions

(cl-defmethod forge--update-discussions ((repo forge-github-repository) data bump)
  (closql-with-transaction (forge-db)
    (let ((initial-pull (not (oref repo discussions-until))))
      (dolist (elt data)
        (forge--update-discussion repo elt bump initial-pull)))))

(cl-defmethod forge--update-discussion ((repo forge-github-repository) data
                                        &optional bump initial-pull)
  (let ((repo-id (oref repo id))
        discussion-id discussion)
    (let-alist data
      (closql-with-transaction (forge-db)
        (setq discussion-id (forge--object-id 'forge-discussion repo .number))
        (setq discussion (or (forge-get-discussion repo .number)
                             (closql-insert
                              (forge-db)
                              (forge-discussion :id         discussion-id
                                                :repository repo-id
                                                :number     .number))))
        (oset discussion their-id   .id)
        (oset discussion slug       (format "#%s" .number))
        (oset discussion author     .author.login)
        (oset discussion title      .title)
        (oset discussion created    .createdAt)
        (oset discussion closed     .closedAt)
        (oset discussion locked-p   .locked)
        (oset discussion category   (forge--object-id repo-id .category.id))
        (oset discussion body       (forge--sanitize-string .body))
        (oset discussion answer
              (and .answer.id
                   (forge--object-id discussion-id .answer.id)))
        (oset discussion state
              (pcase-exhaustive .stateReason
                ("RESOLVED"  'completed) ;sic
                ("DUPLICATE" 'duplicate)
                ("OUTDATED"  'outdated)
                ("REOPENED"  'open)
                ('nil        'open)))
        (dolist (p .comments)
          (let-alist p
            (let ((post-id (forge--object-id discussion-id .databaseId)))
              (closql-insert
               (forge-db)
               (forge-discussion-post
                :id         post-id
                :their-id   .id
                :number     .databaseId
                :discussion discussion-id
                :author     .author.login
                :created    .createdAt
                :updated    .updatedAt
                :body       (forge--sanitize-string .body))
               t)
              (dolist (reply-data .replies)
                (let-alist reply-data
                  (closql-insert
                   (forge-db)
                   (forge-discussion-reply
                    :id         (forge--object-id discussion-id .databaseId)
                    :their-id   .id
                    :number     .databaseId
                    :post       post-id
                    :discussion discussion-id
                    :author     .author.login
                    :created    .createdAt
                    :updated    .updatedAt
                    :body       (forge--sanitize-string .body))
                   t))))))
        (forge--update-status repo discussion data bump initial-pull))
      (forge--set-connections repo discussion 'labels .labels)
      discussion)))

;;;; Issues

(cl-defmethod forge--update-issues ((repo forge-github-repository) data
                                    &optional bump)
  (closql-with-transaction (forge-db)
    (let ((initial-pull (not (oref repo issues-until))))
      (dolist (elt data)
        (forge--update-issue repo elt bump initial-pull)))))

(cl-defmethod forge--update-issue ((repo forge-github-repository) data
                                   &optional bump initial-pull)
  (let ((repo-id (oref repo id))
        issue-id issue)
    (let-alist data
      (closql-with-transaction (forge-db)
        (setq issue-id (forge--object-id 'forge-issue repo .number))
        (setq issue (or (forge-get-issue repo .number)
                        (closql-insert
                         (forge-db)
                         (forge-issue :id         issue-id
                                      :repository repo-id
                                      :number     .number))))
        (oset issue their-id   .id)
        (oset issue slug       (format "#%s" .number))
        (oset issue state
              (pcase-exhaustive (list .stateReason .state)
                ('("COMPLETED"   "CLOSED") 'completed)
                ('("NOT_PLANNED" "CLOSED") 'unplanned)
                ('("DUPLICATE"   "CLOSED") 'duplicate)
                ('("REOPENED"      "OPEN") 'open)
                ('(nil             "OPEN") 'open)))
        (oset issue author     .author.login)
        (oset issue title      .title)
        (oset issue created    .createdAt)
        (oset issue closed     .closedAt)
        (oset issue locked-p   .locked)
        (oset issue milestone  (forge--object-id repo-id .milestone.id))
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
        (forge--update-status repo issue data bump initial-pull))
      (forge--set-connections repo issue 'assignees .assignees)
      (forge--set-connections repo issue 'labels .labels))
    issue))

;;;; Pullreqs

(cl-defmethod forge--update-pullreqs ((repo forge-github-repository) data
                                      &optional bump)
  (closql-with-transaction (forge-db)
    (let ((initial-pull (not (oref repo pullreqs-until))))
      (dolist (elt data)
        (forge--update-pullreq repo elt bump initial-pull)))))

(cl-defmethod forge--update-pullreq ((repo forge-github-repository) data
                                     &optional bump initial-pull)
  (let ((repo-id (oref repo id))
        pullreq-id pullreq)
    (let-alist data
      (closql-with-transaction (forge-db)
        (setq pullreq-id (forge--object-id 'forge-pullreq repo .number))
        (setq pullreq (or (forge-get-pullreq repo .number)
                          (closql-insert
                           (forge-db)
                           (forge-pullreq :id         pullreq-id
                                          :repository repo-id
                                          :number     .number))))
        (oset pullreq their-id     .id)
        (oset pullreq slug         (format "#%s" .number))
        (oset pullreq state        (pcase-exhaustive .state
                                     ("MERGED" 'merged)
                                     ("CLOSED" 'rejected)
                                     ("OPEN"   'open)))
        (oset pullreq author       .author.login)
        (oset pullreq title        .title)
        (oset pullreq created      .createdAt)
        (oset pullreq closed       .closedAt)
        (oset pullreq merged       .mergedAt)
        (oset pullreq draft-p      .isDraft)
        (oset pullreq locked-p     .locked)
        (oset pullreq editable-p   .maintainerCanModify)
        (oset pullreq cross-repo-p .isCrossRepository)
        (oset pullreq base-ref     .baseRef.name)
        (oset pullreq base-rev     .baseRefOid)
        (oset pullreq base-repo    .baseRef.repository.nameWithOwner)
        (oset pullreq head-ref     .headRef.name)
        (oset pullreq head-rev     .headRefOid)
        (oset pullreq head-user    .headRef.repository.owner.login)
        (oset pullreq head-repo    .headRef.repository.nameWithOwner)
        (oset pullreq milestone    (forge--object-id repo-id .milestone.id))
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
        (forge--update-status repo pullreq data bump initial-pull))
      (forge--set-connections repo pullreq 'assignees .assignees)
      (forge--set-connections repo pullreq 'review-requests
                              (mapcar (##alist-get 'requestedReviewer %)
                                      .reviewRequests))
      (forge--set-connections repo pullreq 'labels .labels))
    pullreq))

;;;; Notifications

(cl-defmethod forge--pull-notifications
  ((_class (subclass forge-github-repository)) githost &optional callback)
  ;; The GraphQL API doesn't support notifications and support in the
  ;; REST API is abysmal -- forcing us to perform a major rain dance.
  (let ((buffer (current-buffer))
        (spec (forge--get-forge-host githost t)))
    (forge--msg nil t nil "Pulling notifications")
    (pcase-let*
        ((`(,_ ,apihost ,forge ,_) spec)
         (since (forge--ghub-notifications-since forge))
         (notifs
          (seq-keep (lambda (data)
                      ;; Github returns notifications for repositories the
                      ;; user no longer has access to.  Trying to retrieve
                      ;; information for such repositories leads to errors,
                      ;; which we suppress.  See #164.
                      (with-demoted-errors "forge--pull-notifications: %S"
                        (forge--ghub-massage-notification data githost)))
                    (forge--ghub-get nil "/notifications"
                      `((all . t) ,@(and since `((since . ,since))))
                      :host apihost :unpaginate t)))
         ;; Split into multiple requests to reduce risk of timeouts.
         (groups (seq-partition notifs 50))
         (pages  (length groups))
         (page   0)
         (topics nil))
      (cl-labels
          ((cb (&optional data _headers _status _req)
             (when data
               (setq topics (nconc topics (cdr data))))
             (if groups
                 (let* ((query (seq-keep #'caddr (pop groups)))
                        (tries 3)
                        (errorback nil)
                        (vacuum (lambda ()
                                  (ghub--graphql-vacuum
                                   (cons 'query query) nil #'cb nil
                                   :auth 'forge :host apihost
                                   :errorback errorback))))
                   ;; Github also returns notifications for issues
                   ;; belonging to repositories for which issues
                   ;; have been disabled.  Drop them and try again.
                   (setq errorback
                         (lambda (errors _headers _status _req)
                           (if (zerop tries)
                               (ghub--signal-error errors)
                             (cl-decf tries)
                             (if-let ((notfound
                                       (seq-keep
                                        (lambda (err)
                                          (and (equal (cdr (assq 'type err))
                                                      "NOT_FOUND")
                                               (cadr (assq 'path err))
                                               (intern (cadr (assq 'path err)))))
                                        (cdr errors))))
                                 (progn
                                   (setq query (cl-delete-if (##memq % notfound)
                                                             query :key #'caar))
                                   (funcall vacuum))
                               (ghub--signal-error errors)))))
                   (cl-incf page)
                   (forge--msg nil t nil
                               "Pulling notifications (page %s/%s)" page pages)
                   (funcall vacuum))
               (forge--msg nil t t   "Pulling notifications")
               (forge--msg nil t nil "Storing notifications")
               (forge--ghub-update-notifications notifs topics (not since))
               (forge--msg nil t t "Storing notifications")
               (forge-refresh-buffer buffer)
               (when callback
                 (funcall callback)))))
        (cb)))))

(defun forge--ghub-notifications-since (forge)
  (caar (forge-sql [:select :distinct [notification:updated]
                    :from [notification repository]
                    :where (and (= repository:forge $s1)
                                (= repository:id notification:repository))
                    :order-by [(desc notification:updated)]]
                   forge)))

(defun forge--ghub-massage-notification (data githost)
  (let-alist data
    (let* ((type (intern (downcase .subject.type)))
           (type (if (eq type 'pullrequest) 'pullreq type))
           (_ (unless (memq type '( discussion issue pullreq
                                    commit release))
                (error "BUG: New unsupported notification type: %s" type)))
           (number-or-commit (and .subject.url
                                  (string-match "[^/]*\\'" .subject.url)
                                  (match-string 0 .subject.url)))
           (number (and (memq type '(discussion issue pullreq))
                        (string-to-number number-or-commit)))
           (repo   (forge-get-repository
                    (list githost
                          .repository.owner.login
                          .repository.name)
                    nil :insert!))
           (repoid (oref repo id))
           (owner  (oref repo owner))
           (name   (oref repo name))
           (id     (forge--object-id repoid (string-to-number .id)))
           (alias  (intern (concat "_" (string-replace "=" "_" id)))))
      (and number
           (list alias id
                 `((,alias repository)
                   [(name ,name)
                    (owner ,owner)]
                   ,@(cddr
                      (caddr
                       (ghub--graphql-prepare-query
                        ghub-fetch-repository
                        (pcase type
                          ('discussion `(repository
                                         discussions
                                         (discussion . ,number)))
                          ('issue      `(repository
                                         issues
                                         (issue . ,number)))
                          ('pullreq    `(repository
                                         pullRequest
                                         (pullRequest . ,number))))))))
                 repo type data)))))

(defun forge--ghub-update-notifications (notifs topics initial-pull)
  (closql-with-transaction (forge-db)
    (pcase-dolist (`(,alias ,id ,_query ,repo ,type ,data) notifs)
      (let-alist data
        (and-let*
            ((topic-data (cdr (cadr (assq alias topics))))
             (topic (funcall (pcase-exhaustive type
                               ('discussion #'forge--update-discussion)
                               ('issue      #'forge--update-issue)
                               ('pullreq    #'forge--update-pullreq))
                             repo topic-data nil initial-pull))
             (notif (or (forge-get-notification id)
                        (closql-insert (forge-db)
                                       (forge-notification
                                        :id           id
                                        :thread-id    .id
                                        :repository   (oref repo id)
                                        :type         type
                                        :topic        (oref topic id)
                                        :url          .subject.url)))))
          (oset notif title     .subject.title)
          (oset notif reason    (intern (downcase .reason)))
          (oset notif last-read .last_read_at)
          (oset notif updated   .updated_at))))))

;;;; Miscellaneous

(cl-defmethod forge--add-user-repos
  ((class (subclass forge-github-repository)) host user)
  (forge--fetch-user-repos
   class (forge--as-apihost host) user
   (partial #'forge--batch-add-callback (forge--as-githost host) user)))

(cl-defmethod forge--add-organization-repos
  ((class (subclass forge-github-repository)) host org)
  (forge--fetch-organization-repos
   class (forge--as-apihost host) org
   (partial #'forge--batch-add-callback (forge--as-githost host) org)))

(cl-defmethod forge--fetch-user-repos
  ((_ (subclass forge-github-repository)) host user callback)
  (ghub--graphql-vacuum
   '(query (user
            [(login $login String!)]
            (repositories
             [(:edges t)
              (ownerAffiliations . (OWNER))]
             name)))
   `((login . ,user))
   (lambda (d)
     (funcall callback
              (mapcar (##alist-get 'name %)
                      (let-alist d .user.repositories))))
   nil :auth 'forge :host host))

(cl-defmethod forge--fetch-organization-repos
  ((_ (subclass forge-github-repository)) host org callback)
  (ghub--graphql-vacuum
   '(query (organization
            [(login $login String!)]
            (repositories [(:edges t)] name)))
   `((login . ,org))
   (lambda (d)
     (funcall callback
              (mapcar (##alist-get 'name %)
                      (let-alist d .organization.repositories))))
   nil :auth 'forge :host host))

(defun forge--batch-add-callback (host owner names)
  (let ((repos (mapcan (lambda (name)
                         (let ((repo (forge-get-repository
                                      (list host owner name)
                                      nil :insert!)))
                           (and (not (forge-get-repository repo nil :tracked?))
                                (list repo))))
                       names))
        (cb nil))
    (setq cb (lambda ()
               (when-let ((repo (pop repos)))
                 (forge--pull repo cb))))
    (funcall cb)))

;;; Mutations

(cl-defmethod forge--submit-create-discussion ((_ forge-github-repository) repo)
  (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
    (forge--graphql
     '(mutation (createDiscussion
                 [(input $input CreateDiscussionInput!)]
                 clientMutationId))
     `((input (repositoryId . ,(forge--their-id repo))
              (categoryId
               . , (caar (forge-sql [:select [their-id]
                                     :from discussion-category
                                     :where (and (= repository $s1)
                                                 (= name $s2))]
                                    (oref repo id)
                                    forge--buffer-category)))
              (title . ,title)
              (body  . ,body)))
     :callback  (forge--post-submit-callback)
     :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-issue ((_ forge-github-repository) repo)
  (forge--ghub-post repo "/repos/:owner/:repo/issues"
    (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
      `((title . ,title)
        (body  . ,body)
        ,@(and forge--buffer-milestone
               `((milestone
                  . ,(caar (forge-sql [:select [number]
                                       :from milestone
                                       :where (= title $s1)]
                                      forge--buffer-milestone)))))
        ,@(and forge--buffer-labels
               `((labels . ,(vconcat forge--buffer-labels))))
        ,@(and forge--buffer-assignees
               `((assignees . ,(vconcat forge--buffer-assignees))))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--create-pullreq-from-issue ((repo forge-github-repository)
                                                (issue forge-issue)
                                                source target)
  (pcase-let* ((`(,base-remote . ,base-branch)
                (magit-split-branch-name target))
               (`(,head-remote . ,head-branch)
                (magit-split-branch-name source))
               (head-repo (forge-get-repository :stub head-remote)))
    (forge--ghub-post repo "/repos/:owner/:repo/pulls"
      `((issue . ,(oref issue number))
        (base  . ,base-branch)
        (head  . ,(if (equal head-remote base-remote)
                      head-branch
                    (concat (oref head-repo owner) ":"
                            head-branch)))
        (maintainer_can_modify . t))
      :callback  (lambda (&rest _)
                   (closql-delete issue)
                   (forge-pull))
      :errorback (lambda (&rest _) (forge-pull)))))

(cl-defmethod forge--submit-create-pullreq ((_ forge-github-repository) repo)
  (forge--ghub-post repo "/repos/:owner/:repo/pulls"
    (pcase-let* ((`(,title . ,body) (forge--post-buffer-text))
                 (`(,base-remote . ,base-branch)
                  (magit-split-branch-name forge--buffer-base-branch))
                 (`(,head-remote . ,head-branch)
                  (magit-split-branch-name forge--buffer-head-branch))
                 (head-repo (forge-get-repository :stub head-remote)))
      `((title . ,title)
        (body  . ,body)
        (base  . ,base-branch)
        (head  . ,(if (equal head-remote base-remote)
                      head-branch
                    (concat (oref head-repo owner) ":" head-branch)))
        (draft . ,forge--buffer-draft-p)
        (maintainer_can_modify . t)))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-create-post ((_ forge-github-repository) post)
  (cond
   ((cl-typep post '(or forge-discussion forge-discussion-post))
    (ghub-graphql
     `(mutation (addDiscussionComment
                 [(input $input AddDiscussionCommentInput!)]
                 clientMutationId))
     `((input
        ,@(if (cl-typep post 'forge-discussion-post)
              `((discussionId . ,(oref (forge-get-discussion post) their-id))
                (replyToId . ,(oref post their-id)))
            `((discussionId . ,(oref post their-id))))
        (body . ,(magit--buffer-string nil nil t))))
     :callback  (forge--post-submit-callback)
     :errorback (forge--post-submit-errorback)))
   (t
    (forge--ghub-post post "/repos/:owner/:repo/issues/:number/comments"
      `((body . ,(magit--buffer-string nil nil t)))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback)))))

(cl-defmethod forge--submit-edit-post ((_ forge-github-repository) post)
  (forge--ghub-patch post
    (cl-typecase post
      (forge-pullreq "/repos/:owner/:repo/pulls/:number")
      (forge-issue   "/repos/:owner/:repo/issues/:number")
      (forge-post    "/repos/:owner/:repo/issues/comments/:number"))
    (if (cl-typep post 'forge-topic)
        (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
          `((title . ,title)
            (body  . ,body)))
      `((body . ,(magit--buffer-string nil nil t))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-approve-pullreq ((_ forge-github-repository) repo)
  (let ((body (magit--buffer-string nil nil t)))
    (forge--ghub-post repo "/repos/:owner/:repo/pulls/:number/reviews"
      `((event . "APPROVE")
        ,@(and (not (equal body "")) `((body . ,body))))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-request-changes ((_ forge-github-repository) repo)
  (let ((body (magit--buffer-string nil nil t)))
    (forge--ghub-post repo "/repos/:owner/:repo/pulls/:number/reviews"
      `((event . "REQUEST_CHANGES")
        ,@(and (not (equal body "")) `((body . ,body))))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--set-topic-title
  ((_repo forge-github-repository) topic title)
  (forge--ghub-patch topic
    "/repos/:owner/:repo/issues/:number"
    `((title . ,title))
    :callback (forge--set-field-callback topic)))

(cl-defmethod forge--set-topic-state
  ((_repo forge-github-repository) topic state)
  (forge--ghub-patch topic
    "/repos/:owner/:repo/issues/:number"
    (pcase-exhaustive state
      ;; Merging isn't done through here.
      ;; Marking as a duplicate isn't supported via API.
      ('completed '((state . "closed") (state_reason . "completed")))
      ('unplanned '((state . "closed") (state_reason . "not_planned")))
      ('rejected  '((state . "closed")))
      ('open      '((state . "open"))))
    :callback (forge--set-field-callback topic)))

(cl-defmethod forge--set-topic-state
  ((_repo forge-github-repository)
   (topic forge-discussion)
   state)
  (with-slots (their-id) topic
    (cond ((eq state 'open)
           (forge--graphql
            '(mutation (reopenDiscussion
                        [(input $input ReopenDiscussionInput!)]
                        clientMutationId))
            `((input (discussionId . ,their-id)))
            :callback (forge--set-field-callback topic t)))
          ((forge--graphql
            '(mutation (closeDiscussion
                        [(input $input CloseDiscussionInput!)]
                        clientMutationId))
            `((input (discussionId . ,their-id)
                     (reason . ,(pcase-exhaustive state
                                  ('completed "RESOLVED")
                                  ('duplicate "DUPLICATE")
                                  ('outdated  "OUTDATED")))))
            :callback (forge--set-field-callback topic t))))))

(cl-defmethod forge--set-topic-draft
  ((_repo forge-github-repository) topic value)
  (let ((buffer (current-buffer)))
    (ghub-graphql
     `(mutation (,(if value
                      'convertPullRequestToDraft
                    'markPullRequestReadyForReview)
                 [(input $input ,(if value
                                     'ConvertPullRequestToDraftInput!
                                   'MarkPullRequestReadyForReviewInput!))]
                 (pullRequest isDraft)))
     `((input (pullRequestId . ,(oref topic their-id))))
     :host (oref (forge-get-repository topic) apihost)
     :auth 'forge
     :callback (lambda (data &rest _)
                 (if (assq 'error data)
                     (ghub--graphql-pp-response data)
                   (oset topic draft-p value)
                   (forge-refresh-buffer buffer))))))

(cl-defmethod forge--set-topic-category
  ((_repo forge-github-repository)
   (topic forge-discussion)
   category)
  (forge--graphql
   '(mutation (updateDiscussion
               [(input $input UpdateDiscussionInput!)]
               clientMutationId))
   `((input (discussionId . ,(oref topic their-id))
            (categoryId
             . , (caar (forge-sql [:select [their-id]
                                   :from discussion-category
                                   :where (and (= repository $s1)
                                               (= name $s2))]
                                  (oref (forge-get-repository :tracked) id)
                                  category)))))
   :callback (forge--set-field-callback topic t)))

(cl-defmethod forge--set-topic-answer
  ((_repo forge-github-repository)
   (topic forge-discussion)
   answer)
  (let* ((old (oref topic answer))
         (old (and old (forge--their-id old)))
         (new (and answer (oref answer their-id))))
    (forge--graphql
     `(mutation
       ,@(and old '((unmarkDiscussionCommentAsAnswer
                     [(input $old UnmarkDiscussionCommentAsAnswerInput!)]
                     clientMutationId)))
       ,@(and new '((markDiscussionCommentAsAnswer
                     [(input $new MarkDiscussionCommentAsAnswerInput!)]
                     clientMutationId))))
     `(,@(and old `((old (id . ,old))))
       ,@(and new `((new (id . ,new)))))
     :callback (forge--set-field-callback topic t))))

(cl-defmethod forge--set-topic-milestone
  ((repo forge-github-repository) topic milestone)
  (forge--ghub-patch topic
    "/repos/:owner/:repo/issues/:number"
    (if milestone
        `((milestone . ,(caar (forge-sql [:select [number]
                                          :from milestone
                                          :where (and (= repository $s1)
                                                      (= title $s2))]
                                         (oref repo id)
                                         milestone))))
      `((milestone . :null)))
    :callback (forge--set-field-callback topic)))

(cl-defmethod forge--set-topic-labels
  ((repo forge-github-repository) topic labels)
  (let* ((topic-id (oref topic their-id))
         (old (mapcar (##forge--their-id (car %)) (oref topic labels)))
         (new (mapcar (##forge--their-id (car %))
                      (forge-sql [:select [id] :from label
                                  :where (and (= repository $s1)
                                              (in name $v2))]
                                 (oref repo id)
                                 (vconcat labels))))
         (add (cl-set-difference new old :test #'equal))
         (del (cl-set-difference old new :test #'equal)))
    (when (or add del)
      (forge--graphql
       `(mutation
         ,@(and add '((addLabelsToLabelable
                       [(input $add AddLabelsToLabelableInput!)]
                       clientMutationId)))
         ,@(and del '((removeLabelsFromLabelable
                       [(input $del RemoveLabelsFromLabelableInput!)]
                       clientMutationId))))
       `(,@(and add `((add (labelableId . ,topic-id)
                           (labelIds . ,(vconcat add)))))
         ,@(and del `((del (labelableId . ,topic-id)
                           (labelIds . ,(vconcat del))))))
       :callback (forge--set-field-callback topic)))))

(cl-defmethod forge--set-topic-assignees
  ((repo forge-github-repository) topic assignees)
  (let* ((topic-id (oref topic their-id))
         (old (mapcar (##nth 3 %) (oref topic assignees)))
         (new (forge-sql-car [:select [forge-id] :from assignee
                              :where (and (= repository $s1)
                                          (in login $v2))]
                             (oref repo id)
                             (vconcat assignees)))
         (add (cl-set-difference new old :test #'equal))
         (del (cl-set-difference old new :test #'equal)))
    (when (or add del)
      (forge--graphql
       `(mutation
         ,@(and add '((addAssigneesToAssignable
                       [(input $add AddAssigneesToAssignableInput!)]
                       clientMutationId)))
         ,@(and del '((removeAssigneesFromAssignable
                       [(input $del RemoveAssigneesFromAssignableInput!)]
                       clientMutationId))))
       `(,@(and add `((add (assignableId . ,topic-id)
                           (assigneeIds . ,(vconcat add)))))
         ,@(and del `((del (assignableId . ,topic-id))
                      (assigneeIds . ,(vconcat del)))))
       :callback (forge--set-field-callback topic)))))

(cl-defmethod forge--set-topic-review-requests
  ((repo forge-github-repository) topic reviewers)
  (let ((users (forge-sql-car
                [:select [forge-id] :from assignee
                 :where (and (= repository $s1)
                             (in login $v2))]
                (oref repo id)
                (vconcat (seq-remove (##string-match "/" %) reviewers))))
        (teams nil)) ;TODO Investigate #742, track id, then use it here.
    (forge--graphql
     `(mutation (requestReviews
                 [(input $input RequestReviewsInput!)]
                 clientMutationId))
     `((input (pullRequestId . ,(oref topic their-id))
              ,@(and users `((userIds . ,(vconcat users))))
              ,@(and teams `((teamIds . ,(vconcat teams))))))
     :callback (forge--set-field-callback topic))))

(cl-defmethod forge--delete-comment
  ((_repo forge-github-repository) post)
  (forge--ghub-delete post "/repos/:owner/:repo/issues/comments/:number")
  (closql-delete post)
  (forge-refresh-buffer))

(cl-defmethod forge--topic-template-files ((repo forge-github-repository)
                                           (_ (subclass forge-issue)))
  ;; Upstream documentation is unclear but experimentation indicates that
  ;; placing the template directory in ./ or docs/ does not work, a single
  ;; template file is not supported, and silly names like IsSuE_tEmPlAtE
  ;; are supported (but we don't support that here anyway).  We do not
  ;; support experimental issue *forms* for now.  Make sure the config
  ;; file comes last.
  (or (nconc (forge--topic-template-files-1
              repo "md" ".github/issue_template")
             (forge--topic-template-files-1
              repo nil  ".github/issue_template/config.yml"))
      (nconc (forge--topic-template-files-1
              repo "md" ".github/ISSUE_TEMPLATE")
             (forge--topic-template-files-1
              repo nil  ".github/ISSUE_TEMPLATE/config.yml"))))

(cl-defmethod forge--topic-template-files ((repo forge-github-repository)
                                           (_ (subclass forge-pullreq)))
  ;; The web interface does not support multiple pull-request templates,
  ;; and while the API theoretically does, we don't support that here.
  ;; When there are multiple conflicting "default" templates, the rules
  ;; used by Github are more complex than just sorting alphabetically and
  ;; then taking the first found file.  Too bad; that's what we do.
  (let ((branch (oref repo default-branch))
        (case-fold-search t))
    (seq-some (lambda (file)
                (and (string-match-p "\
\\`\\(.github/\\|docs/\\)?pull_request_template\\(\\.[a-zA-Z0-9]+\\)?\\'" file)
                     (list (concat branch ":" file))))
              (magit-git-items "ls-tree" "-z" "--full-tree" "--name-only"
                               "-r" branch))))

(cl-defmethod forge--set-default-branch ((repo forge-github-repository) branch)
  (forge--ghub-patch repo
    "/repos/:owner/:repo"
    `((default_branch . ,branch)))
  (message "Waiting 5 seconds for GitHub to complete update...")
  (sleep-for 5)
  (message "Waiting 5 seconds for GitHub to complete update...done")
  (let ((remote (oref repo remote)))
    (magit-call-git "fetch" "--prune" remote)
    (magit-call-git "remote" "set-head" "--auto" remote)))

(cl-defmethod forge--rename-branch ((repo forge-github-repository)
                                    newname oldname)
  (forge--ghub-post repo
    (format "/repos/:owner/:name/branches/%s/rename" oldname)
    `((new_name . ,newname)))
  (message "Waiting 5 seconds for GitHub to complete rename...")
  (sleep-for 5)
  (message "Waiting 5 seconds for GitHub to complete rename...done")
  (magit-call-git "fetch" "--prune" (oref repo remote)))

(cl-defmethod forge--fork-repository ((repo forge-github-repository) fork)
  (with-slots (owner name) repo
    (forge--ghub-post repo
      (format "/repos/%s/%s/forks" owner name)
      (and (not (equal fork (ghub--username (ghub--host nil))))
           `((organization . ,fork))))
    (ghub-wait (format "/repos/%s/%s" fork name) nil :auth 'forge)))

(cl-defmethod forge--merge-pullreq ((repo forge-github-repository)
                                    pullreq hash method)
  (forge--ghub-put pullreq
    "/repos/:owner/:repo/pulls/:number/merge"
    `((merge_method . ,(symbol-name method))
      ,@(and hash `((sha . ,hash))))
    :errorback t
    :callback
    (lambda (&rest _)
      (forge--pull
       repo
       (lambda ()
         (when-let* ((branch (or (forge--pullreq-branch-active pullreq)
                                 (forge--branch-pullreq pullreq)))
                     (upstream (magit-get-local-upstream-branch branch))
                     (remote (oref repo remote)))
           (magit-call-git "checkout" upstream)
           (magit-call-git "pull" "--ff-only" remote (magit-pull-arguments))
           (magit-call-git "branch" "-d" branch)
           (forge-refresh-buffer)))))))

;;; Wrappers

(cl-defun forge--ghub-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host
                               callback errorback)
  (declare (indent defun))
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
  (declare (indent defun))
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
  (declare (indent defun))
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
  (declare (indent defun))
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
  (declare (indent defun))
  (ghub-delete (forge--format-resource obj resource)
               params
               :host (or host (oref (forge-get-repository obj) apihost))
               :auth 'forge
               :query query :payload payload :headers headers
               :silent silent :unpaginate unpaginate
               :noerror noerror :reader reader
               :callback callback :errorback errorback))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("partial" . "llama--left-apply-partially")
;;   ("rpartial" . "llama--right-apply-partially"))
;; End:
(provide 'forge-github)
;;; forge-github.el ends here
