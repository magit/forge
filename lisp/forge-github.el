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

(require 'forge-client)
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
;;;; GraphQL

(defconst forge--github-sparse-repository-query
  '(query
    (  repository
       [(owner $owner String!)
        (name  $name  String!)]
       name
       id
       createdAt
       updatedAt
       nameWithOwner
       (parent nameWithOwner)
       description
       homepageUrl
       (defaultBranchRef name)
       isArchived
       isFork
       isLocked
       isMirror
       isPrivate
       hasDiscussionsEnabled
       hasIssuesEnabled
       hasWikiEnabled
       (licenseInfo name)
       (stargazers totalCount)
       (watchers totalCount))))

(defconst forge--github-repository-query
  `(query
    (  repository
       ,@(cdr (cadr forge--github-sparse-repository-query))
       (  assignableUsers [(:edges t)]
          id
          login
          name)
       (  discussionCategories [(:edges t)]
          id
          name
          emoji
          isAnswerable
          description)
       (  discussions [(:edges t)
                       (:singular discussion number)
                       (orderBy ((field UPDATED_AT) (direction DESC)))]
          id
          databaseId
          number
          url
          stateReason
          ;; Discussions lack isReadByViewer.
          (answer id)
          (author login)
          title
          createdAt
          updatedAt
          closedAt
          locked
          (category id)
          body
          (  comments [(:edges t)]
             id
             databaseId
             (author login)
             createdAt
             updatedAt
             body
             (  replies [(:edges 20)]
                id
                databaseId
                (author login)
                createdAt
                updatedAt
                body))
          (   labels [(:edges t)] id))
       (  issues [(:edges t)
                  (:singular issue number)
                  (orderBy ((field UPDATED_AT) (direction DESC)))]
          number
          id
          state
          stateReason
          isReadByViewer
          (author login)
          title
          createdAt
          updatedAt
          closedAt
          locked
          (milestone id)
          body
          (  assignees [(:edges t)] id)
          (  comments [(:edges t)]
             id
             databaseId
             (author login)
             createdAt
             updatedAt
             body)
          (  labels [(:edges t)] id))
       (  labels [(:edges t) (:singular label id)]
          id
          name
          color
          description)
       (  milestones [(:edges t) (:singular milestone id)]
          id
          number
          title
          createdAt
          updatedAt
          dueOn
          closedAt
          description)
       (  pullRequests [(:edges t)
                        (:singular pullRequest number)
                        (orderBy ((field UPDATED_AT) (direction DESC)))]
          number
          id
          state
          isReadByViewer
          (author login)
          title
          createdAt
          updatedAt
          closedAt
          mergedAt
          isDraft
          locked
          maintainerCanModify
          isCrossRepository
          (milestone id)
          body
          (baseRef name (repository nameWithOwner))
          baseRefOid
          (headRef name (repository (owner login) nameWithOwner))
          headRefOid
          (  assignees [(:edges t)] id)
          (  reviewRequests [(:edges t)]
             (requestedReviewer "... on User { id }\n"))
          (  comments [(:edges t)]
             id
             databaseId
             (author login)
             createdAt
             updatedAt
             body)
          (  labels [(:edges t)] id)))))

;;;; Repository

(cl-defmethod forge--pull ((repo forge-github-repository)
                           &optional callback since)
  (cl-assert (not (and since (forge-get-repository repo nil :tracked?))))
  (setq forge--mode-line-buffer (current-buffer))
  (forge--msg repo t nil "Pulling REPO")
  (let ((buf (current-buffer)))
    (forge--query repo
      (if (oref repo selective-p)
          forge--github-sparse-repository-query
        forge--github-repository-query)
      `((owner . ,(oref repo owner))
        (name  . ,(oref repo name)))
      :callback
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
      :narrow '(repository)
      :until
      ;; Keys have the form `FIELD-until', where FIELD is the name of a
      ;; field of Repository objects.  See `ghub--graphql-walk-response'.
      `((discussions-until  . ,(or since (oref repo discussions-until)))
        (issues-until       . ,(or since (oref repo issues-until)))
        (pullRequests-until . ,(or since (oref repo pullreqs-until)))))))

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
  (forge--query repo
    `(query
      [($owner String!)
       ($name  String!)]
      (repository
      [(owner $owner)
       (name  $name)]
       ,(caddr (caddr (ghub--graphql-prepare-query
                       forge--github-repository-query
                       `(repository discussions (discussion . ,number)))))
       ,(caddr (caddr (ghub--graphql-prepare-query
                       forge--github-repository-query
                       `(repository issues (issue . ,number)))))
       ,(caddr (caddr (ghub--graphql-prepare-query
                       forge--github-repository-query
                       `(repository pullRequests (pullreq . ,number)))))))
    `((owner . ,(oref repo owner))
      (name  . ,(oref repo name)))
    :noerror t
    :callback (lambda (data)
                (let-alist data
                  (cond ((setq data .repository.discussion)
                         (forge--update-discussion repo data))
                        ((setq data .repository.issue)
                         (forge--update-issue repo data))
                        ((setq data .repository.pullRequest)
                         (forge--update-pullreq repo data))))
                (forge-refresh-buffer))))

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (topic forge-discussion))
  (forge--pull-topic-1 repo #'forge--update-discussion
    `(repository discussions (discussion . ,(oref topic number)))))

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (topic forge-issue))
  (forge--pull-topic-1 repo #'forge--update-issue
    `(repository issues (issue . ,(oref topic number)))))

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (topic forge-pullreq))
  (forge--pull-topic-1 repo #'forge--update-pullreq
    `(repository pullRequests (pullRequest . ,(oref topic number)))))

(cl-defun forge--pull-topic-1 (repo update narrow)
  (declare (indent defun))
  (forge--query repo
    (ghub--graphql-prepare-query forge--github-repository-query narrow)
    `((owner . ,(oref repo owner))
      (name  . ,(oref repo name)))
    :callback (lambda (data)
                (funcall update repo (cdr (cadr (cadr data))))
                (forge-refresh-buffer))))

(cl-defmethod forge--update-status ((repo forge-github-repository)
                                    topic data bump initial-pull)
  (let-alist data
    (let ((updated (or .updatedAt .createdAt))
          (current-status (oref topic status)))
      (if (forge-discussion-p topic)
          ;; Discussions lack `isReadByViewer', so we have to use a
          ;; heuristic, which is even worse than what we use for other
          ;; topic types.  Except during the repository's initial pull,
          ;; all new discussions start out as `unread'.
          ;;
          ;; If we pull a discussion after the user mutated it, setting
          ;; the status to `unread' is highly undesirable (since they made
          ;; the mutation, they have already "read" it), yet that is what
          ;; we do here.  However, the callback used by the discussion
          ;; method of `forge--pull-topic', afterwards resets the status
          ;; to the previous value.
          ;;
          ;; Of course it is possible that we pull other changes by other
          ;; people at the same time, but we have no (reasonable) way of
          ;; knowing that.  So in that case too the status sadly doesn't
          ;; end up as `unread'.
          ;;
          ;; The callback kludge is not needed for all mutations.  Some
          ;; mutations (e.g., setting labels) do not cause `updated_at'
          ;; to be bumped; this second defect cancels out the first.
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
                    (forge-rest apihost "GET" "/notifications"
                      ((all t)
                       (and since (since since)))
                      :unpaginate t)))
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
                 (let* ((query (cons 'query (seq-keep #'caddr (pop groups))))
                        (tries 3)
                        (errorback nil)
                        (vacuum (lambda ()
                                  (forge--query apihost query nil
                                    :callback #'cb
                                    :errorback errorback))))
                   ;; Github also returns notifications for issues
                   ;; belonging to repositories for which issues
                   ;; have been disabled.  Drop them and try again.
                   (setq errorback
                         (lambda (errors _headers _status _req)
                           (if (zerop tries)
                               (ghub--signal-error errors)
                             (cl-decf tries)
                             (cond-let
                               ([notfound
                                 (seq-keep
                                  (lambda (err)
                                    (and (equal (cdr (assq 'type err))
                                                "NOT_FOUND")
                                         (cadr (assq 'path err))
                                         (intern (cadr (assq 'path err)))))
                                  (cdr errors))]
                                (setq query (cl-delete-if (##memq % notfound)
                                                          query :key #'caar))
                                (funcall vacuum))
                               ((ghub--signal-error errors))))))
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
  (forge-sql1 [:select :distinct [notification:updated]
               :from [notification repository]
               :where (and (= repository:forge $s1)
                           (= repository:id notification:repository))
               :order-by [(desc notification:updated)]]
              forge))

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
                        forge--github-repository-query
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
        (when-let*
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
  (forge-query host
    (user [(login $login String!)]
          (repositories [(:edges t) (ownerAffiliations . (OWNER))] name))
    ((login user))
    :callback (lambda (d)
                (funcall callback
                         (mapcar (##alist-get 'name %)
                                 (let-alist d .user.repositories))))))

(cl-defmethod forge--fetch-organization-repos
  ((_ (subclass forge-github-repository)) host org callback)
  (forge-query host
    (organization [(login $login String!)]
                  (repositories [(:edges t)] name))
    ((login org))
    :callback (lambda (d)
                (funcall callback
                         (mapcar (##alist-get 'name %)
                                 (let-alist d .organization.repositories))))))

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

(cl-defmethod forge--submit-create-discussion ((repo forge-github-repository) _)
  (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
    (forge-mutate repo createDiscussion
      ((repositoryId (forge--their-id repo))
       (categoryId (forge--their-id forge--buffer-category 'category repo))
       (title title)
       (body  body))
      :callback  (forge--post-submit-callback t)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-issue ((repo forge-github-repository) _)
  (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
    (forge-mutate repo createIssue
      ((repositoryId (forge--their-id repo))
       (title title)
       (body  body)
       (and forge--buffer-milestone
            (milestoneId
             (forge--their-id forge--buffer-milestone 'milestone)))
       (and forge--buffer-labels
            (labelIds
             (vconcat (forge--their-id forge--buffer-labels 'labels repo))))
       (and forge--buffer-assignees
            (assigneeIds
             (vconcat (forge--their-id forge--buffer-assignees 'assignees repo)))))
      :callback  (forge--post-submit-callback t)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--create-pullreq-from-issue
  ((repo  forge-github-repository)
   (issue forge-issue)
   source target)
  (pcase-let* ((`(,base-remote . ,base-branch)
                (magit-split-branch-name target))
               (`(,head-remote . ,head-branch)
                (magit-split-branch-name source))
               (head-repo (forge-get-repository :stub head-remote)))
    (forge-rest repo "POST" "/repos/:owner/:repo/pulls"
      ((issue (oref issue number))
       (base base-branch)
       (head (if (equal head-remote base-remote)
                 head-branch
               (concat (oref head-repo owner) ":" head-branch)))
       (maintainer_can_modify t))
      :callback  (lambda (&rest _)
                   (closql-delete issue)
                   (forge--pull repo))
      :errorback (lambda (&rest _) (forge--pull repo)))))

(cl-defmethod forge--submit-create-pullreq ((repo forge-github-repository) _)
  (pcase-let* ((`(,title . ,body) (forge--post-buffer-text))
               (`(,base-remote . ,base-branch)
                (magit-split-branch-name forge--buffer-base-branch))
               (`(,head-remote . ,head-branch)
                (magit-split-branch-name forge--buffer-head-branch))
               (head-repo (forge-get-repository :stub head-remote)))
    ;; Cannot use `createPullRequest' because value for
    ;; `headRepositoryId' is unavailable.
    (forge-rest repo "POST" "/repos/:owner/:repo/pulls"
      ((title title)
       (body  body)
       (base  base-branch)
       (head  (if (equal head-remote base-remote)
                  head-branch
                (concat (oref head-repo owner) ":" head-branch)))
       (draft forge--buffer-draft-p)
       (maintainer_can_modify t))
      :callback  (forge--post-submit-callback t)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-post
  ((repo forge-github-repository)
   (post forge-post))
  (forge-mutate repo addComment
    ((subjectId (oref post their-id))
     (body      (string-trim (buffer-str))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-create-post
  ((repo forge-github-repository)
   (post forge-discussion-post))
  (forge-mutate repo addDiscussionComment
    ((discussionId (oref (forge-get-discussion post) their-id))
     (replyToId    (oref post their-id))
     (body         (string-trim (buffer-str))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-create-post
  ((repo forge-github-repository)
   (post forge-discussion))
  (forge-mutate repo addDiscussionComment
    ((discussionId (oref post their-id))
     (body         (string-trim (buffer-str))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-edit-post
  ((repo forge-github-repository)
   (post forge-post))
  (cl-typecase post
    ((or forge-issue-post forge-pullreq-post)
     ;; Cannot use GraphQL because we made the mistake to derive our ID
     ;; from the number instead of their ID.  `updatePullRequestComment'
     ;; (or something equivalent under an inconsistent name) does not
     ;; exist, so for that we would have to continue to use REST anyway.
     (forge-rest post "PATCH" "/repos/:owner/:repo/issues/comments/:number"
       ((body (string-trim (buffer-str))))
       :callback  (forge--post-submit-callback)
       :errorback (forge--post-submit-errorback)))
    (t
     (forge--query repo
       `(mutation (,(cl-etypecase post
                      (forge-discussion       'updateDiscussion)
                      (forge-issue            'updateIssue)
                      (forge-pullreq          'updatePullRequest)
                      (forge-discussion-post  'updateDiscussionComment)
                      (forge-discussion-reply 'updateDiscussionComment)
                      (forge-issue-post       'updateIssueComment)
                      (forge-pullreq-post     'updatePullRequestComment))
                   [(input
                     $input
                     ,(cl-etypecase post
                        (forge-discussion       'UpdateDiscussionInput!)
                        (forge-issue            'UpdateIssueInput!)
                        (forge-pullreq          'UpdatePullRequestInput!)
                        (forge-discussion-post  'UpdateDiscussionCommentInput!)
                        (forge-discussion-reply 'UpdateDiscussionCommentInput!)
                        (forge-issue-post       'UpdateIssueCommentInput!)
                        (forge-pullreq-post     'UpdatePullRequestCommentInput!)))]
                   clientMutationId))
       `((input (,(cl-etypecase post
                    (forge-discussion       'discussionId)
                    (forge-issue            'id)
                    (forge-pullreq          'pullRequestId)
                    (forge-discussion-post  'commentId)
                    (forge-discussion-reply 'commentId)
                    (forge-issue-post       'id)
                    (forge-pullreq-post     'id))
                 . ,(forge--their-id post))
                ,@(if (cl-typep post 'forge-topic)
                      (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
                        `((title . ,title)
                          (body  . ,body)))
                    `((body . ,(string-trim (buffer-str)))))))
       :callback  (forge--post-submit-callback)
       :errorback (forge--post-submit-errorback)))))

(cl-defmethod forge--submit-approve-pullreq
  ((_repo forge-github-repository)
   (topic forge-pullreq))
  (let ((body (string-trim (buffer-str))))
    (forge-rest topic "POST" "/repos/:owner/:repo/pulls/:number/reviews"
      ((event "APPROVE")
       (and (not (equal body "")) (body body)))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-request-changes
  ((_repo forge-github-repository)
   (topic forge-pullreq))
  (let ((body (string-trim (buffer-str))))
    (forge-rest topic "POST" "/repos/:owner/:repo/pulls/:number/reviews"
      ((event "REQUEST_CHANGES")
       (and (not (equal body "")) (body body)))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--set-topic-title
  ((_repo forge-github-repository)
   (topic forge-discussion)
   title)
  (forge--mutate-field topic updateDiscussion
    ((discussionId (oref topic their-id))
     (title        title))))

(cl-defmethod forge--set-topic-title
  ((_repo forge-github-repository)
   (topic forge-issue)
   title)
  (forge--mutate-field topic updateIssue
    ((id    (oref topic their-id))
     (title title))))

(cl-defmethod forge--set-topic-title
  ((_repo forge-github-repository)
   (topic forge-pullreq)
   title)
  (forge--mutate-field topic updatePullRequest
    ((pullRequestId (oref topic their-id))
     (title         title))))

(cl-defmethod forge--set-topic-state
  ((_repo forge-github-repository)
   (topic forge-topic)
   state)
  (forge--rest topic "PATCH" "/repos/:owner/:repo/issues/:number"
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
  (cond ((eq state 'open)
         (forge--mutate-field topic reopenDiscussion
           ((discussionId (oref topic their-id)))))
        ((forge--mutate-field topic closeDiscussion
           ((discussionId (oref topic their-id))
            (reason (pcase-exhaustive state
                      ('completed "RESOLVED")
                      ('duplicate "DUPLICATE")
                      ('outdated  "OUTDATED"))))))))

(cl-defmethod forge--set-topic-draft
  ((_repo forge-github-repository)
   (topic forge-topic)
   value)
  (cond (value
         (forge--mutate-field topic convertPullRequestToDraft
           ((pullRequestId (oref topic their-id)))))
        ((forge--mutate-field topic markPullRequestReadyForReview
           ((pullRequestId (oref topic their-id)))))))

(cl-defmethod forge--set-topic-category
  ((repo  forge-github-repository)
   (topic forge-discussion)
   category)
  (forge--mutate-field topic updateDiscussion
    ((discussionId (oref topic their-id))
     (categoryId (forge--their-id category 'category repo)))))

(cl-defmethod forge--set-topic-answer
  ((repo  forge-github-repository)
   (topic forge-discussion)
   answer)
  (let* ((old (oref topic answer))
         (old (and old (forge--their-id old)))
         (new (and answer (oref answer their-id))))
    (forge--query repo
      `(mutation
        ,@(and old '((unmarkDiscussionCommentAsAnswer
                      [(input $old UnmarkDiscussionCommentAsAnswerInput!)]
                      clientMutationId)))
        ,@(and new '((markDiscussionCommentAsAnswer
                      [(input $new MarkDiscussionCommentAsAnswerInput!)]
                      clientMutationId))))
      `(,@(and old `((old (id . ,old))))
        ,@(and new `((new (id . ,new)))))
      :callback (forge--set-field-callback topic))))

(cl-defmethod forge--set-topic-milestone
  ((repo  forge-github-repository)
   (topic forge-topic)
   milestone)
  (forge-rest topic "POST" "/repos/:owner/:repo/issues/:number"
    ((milestone (if milestone
                    (forge-sql1 [:select [number]
                                 :from milestone
                                 :where (and (= repository $s1)
                                             (= title $s2))]
                                (oref repo id)
                                milestone)
                  :null)))
    :callback (forge--set-field-callback topic)))

(cl-defmethod forge--set-topic-labels
  ((repo  forge-github-repository)
   (topic forge-topic)
   labels)
  (let* ((topic-id (oref topic their-id))
         (their-id ; I really messed up IDs! :(
          (pcase-lambda (`(,id))
            (let* ((parts (split-string (base64-decode-string id) ":"))
                   (maybe-id (car (last parts))))
              (if (string-prefix-p "Label" maybe-id)
                  (base64-encode-string (string-join (last parts 2) ":"))
                maybe-id))))
         (old (mapcar their-id (oref topic labels)))
         (new (mapcar their-id
                      (forge-sql [:select [id] :from label
                                  :where (and (= repository $s1)
                                              (in name $v2))]
                                 (oref repo id)
                                 (vconcat labels))))
         (add (cl-set-difference new old :test #'equal))
         (del (cl-set-difference old new :test #'equal)))
    (when (or add del)
      (forge--query repo
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
  ((repo  forge-github-repository)
   (topic forge-topic)
   assignees)
  (let* ((topic-id (oref topic their-id))
         (old (mapcar (##nth 3 %) (oref topic assignees)))
         (new (forge--their-id assignees 'assignees repo))
         (add (cl-set-difference new old :test #'equal))
         (del (cl-set-difference old new :test #'equal)))
    (when (or add del)
      (forge--query repo
        `(mutation
          ,@(and add '((addAssigneesToAssignable
                        [(input $add AddAssigneesToAssignableInput!)]
                        clientMutationId)))
          ,@(and del '((removeAssigneesFromAssignable
                        [(input $del RemoveAssigneesFromAssignableInput!)]
                        clientMutationId))))
        `(,@(and add `((add (assignableId . ,topic-id)
                            (assigneeIds . ,(vconcat add)))))
          ,@(and del `((del (assignableId . ,topic-id)
                            (assigneeIds . ,(vconcat del))))))
        :callback (forge--set-field-callback topic)))))

(cl-defmethod forge--set-topic-review-requests
  ((repo  forge-github-repository)
   (topic forge-topic)
   reviewers)
  (let ((users (forge--their-id (seq-remove (##string-match "/" %) reviewers)
                                'assignees repo))
        (teams nil)) ;TODO Investigate #742, track id, then use it here.
    (forge--mutate-field topic requestReviews
      ((pullRequestId (oref topic their-id))
       (and users (userIds (vconcat users)))
       (and teams (teamIds (vconcat teams)))))))

(cl-defmethod forge--delete-comment
  ((_    forge-github-repository)
   (post forge-post))
  (forge-rest post "DELETE" "/repos/:owner/:repo/issues/comments/:number")
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
  (let ((branch (forge--get-default-branch repo))
        (case-fold-search t))
    (seq-some (lambda (file)
                (and (string-match-p "\
\\`\\(.github/\\|docs/\\)?pull_request_template\\(\\.[a-zA-Z0-9]+\\)?\\'" file)
                     (list (concat branch ":" file))))
              (magit-git-items "ls-tree" "-z" "--full-tree" "--name-only"
                               "-r" branch))))

(cl-defmethod forge--set-default-branch ((repo forge-github-repository) branch)
  (forge-rest repo "PATCH" "/repos/:owner/:repo"
    ((default_branch branch)))
  (message "Waiting 5 seconds for GitHub to complete update...")
  (sleep-for 5)
  (message "Waiting 5 seconds for GitHub to complete update...done")
  (let ((remote (oref repo remote)))
    (magit-call-git "fetch" "--prune" remote)
    (magit-call-git "remote" "set-head" "--auto" remote)))

(cl-defmethod forge--rename-branch ((repo forge-github-repository)
                                    newname oldname)
  (forge-rest repo "POST"
    (format "/repos/:owner/:name/branches/%s/rename" oldname)
    ((new_name newname)))
  (message "Waiting 5 seconds for GitHub to complete rename...")
  (sleep-for 5)
  (message "Waiting 5 seconds for GitHub to complete rename...done")
  (magit-call-git "fetch" "--prune" (oref repo remote)))

(cl-defmethod forge--fork-repository ((repo forge-github-repository) fork all)
  (with-slots (name apihost) repo
    (forge-rest repo "POST" "/repos/:owner/:name/forks"
      ((and (not (equal fork (ghub--username apihost)))
            (organization fork))
       (default-branch-only (not all))))
    (ghub-wait (format "/repos/%s/%s" fork name)
               nil :auth 'forge :host apihost)))

(cl-defmethod forge--merge-pullreq
  ((repo  forge-github-repository)
   (topic forge-pullreq)
   hash method)
  (forge-mutate topic mergePullRequest
    ((pullRequestId (oref topic their-id))
     (mergeMethod (upcase (symbol-name method)))
     (and hash (expectedHeadOid hash)))
    :callback
    (lambda (_)
      (forge--pull
       repo
       (lambda ()
         (when-let* ((branch (or (forge--pullreq-branch-active topic)
                                 (forge--branch-pullreq topic)))
                     (upstream (magit-get-local-upstream-branch branch))
                     (remote (oref repo remote)))
           (magit-call-git "checkout" upstream)
           (magit-call-git "pull" "--ff-only" remote (magit-pull-arguments))
           (magit-call-git "branch" "-d" branch)
           (forge-refresh-buffer)))))))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"          . "cond-let--and$")
;;   ("and-let"       . "cond-let--and-let")
;;   ("if-let"        . "cond-let--if-let")
;;   ("when-let"      . "cond-let--when-let")
;;   ("buffer-string" . "buffer-string")
;;   ("buffer-str"    . "forge--buffer-substring-no-properties")
;;   ("partial"       . "llama--left-apply-partially"))
;; End:
(provide 'forge-github)
;;; forge-github.el ends here
