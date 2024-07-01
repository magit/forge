;;; forge-github.el --- Github support  -*- lexical-binding:t -*-

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

(require 'ghub)

(require 'forge)
(require 'forge-issue)
(require 'forge-pullreq)

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
           (forge--update-repository repo data)
           (forge--update-assignees  repo .assignableUsers)
           (forge--update-forks      repo .forks)
           (forge--update-labels     repo .labels)
           (forge--update-milestones repo .milestones)
           (forge--update-issues     repo .issues t)
           (forge--update-pullreqs   repo .pullRequests t)
           (forge--update-revnotes   repo .commitComments))
         (oset repo condition :tracked))
       (forge--msg repo t t   "Storing REPO")
       (cond
        ((oref repo selective-p))
        (callback (funcall callback))
        ((forge--maybe-git-fetch repo buf))))
     `((issues-until       . ,(or since (oref repo issues-until)))
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
    (oset repo wiki-p         .hasWikiEnabled)
    (oset repo stars          .stargazers.totalCount)
    (oset repo watchers       .watchers.totalCount)))

(cl-defmethod forge--update-revnotes ((repo forge-github-repository) data)
  (closql-with-transaction (forge-db)
    (mapc (apply-partially #'forge--update-revnote repo) data)))

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

;;;; Topics

(cl-defmethod forge--pull-topic ((repo forge-github-repository)
                                 (number number))
  (let ((id (oref repo id)))
    (forge--pull-topic
     repo
     (forge-issue :repository id :number number)
     :errorback (lambda (err _headers _status _req)
                  (when (equal (cdr (assq 'type (cadr err))) "NOT_FOUND")
                    (forge--pull-topic
                     repo
                     (forge-pullreq :repository id :number number)))))))

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
      (cond ((not .isReadByViewer)
             (oset topic status 'unread))
            (initial-pull
             (oset topic status 'done))
            ((null current-status)
             (oset topic status 'pending))
            ((string> updated (oref topic updated))
             (oset topic status 'pending)))
      (oset topic updated updated)
      (when bump
        (let* ((slot (if (forge-issue-p topic) 'issues-until 'pullreqs-until))
               (until (eieio-oref repo slot)))
          (when (or (not until) (string> updated until))
            (eieio-oset repo slot updated)))))))

;;;; Issues

(cl-defmethod forge--update-issues ((repo forge-github-repository) data
                                    &optional bump)
  (closql-with-transaction (forge-db)
    (let ((initial-pull (not (oref repo issues-until))))
      (mapc (lambda (e) (forge--update-issue repo e bump initial-pull)) data))))

(cl-defmethod forge--update-issue ((repo forge-github-repository) data
                                   &optional bump initial-pull)
  (let (issue-id issue)
    (let-alist data
      (closql-with-transaction (forge-db)
        (setq issue-id (forge--object-id 'forge-issue repo .number))
        (setq issue (or (forge-get-issue repo .number)
                        (closql-insert
                         (forge-db)
                         (forge-issue :id         issue-id
                                      :repository (oref repo id)
                                      :number     .number))))
        (oset issue their-id   .id)
        (oset issue slug       (format "#%s" .number))
        (oset issue state
              (pcase-exhaustive (list .stateReason .state)
                (`("COMPLETED"   ,_) 'completed)
                (`("NOT_PLANNED" ,_) 'unplanned)
                (`(,_      "CLOSED") 'completed)
                (`(,_        "OPEN") 'open)))
        (oset issue author     .author.login)
        (oset issue title      .title)
        (oset issue created    .createdAt)
        (oset issue closed     .closedAt)
        (oset issue locked-p   .locked)
        (oset issue milestone  (and .milestone.id
                                    (forge--object-id (oref repo id)
                                                      .milestone.id)))
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
      (ignore-errors
        (forge--set-id-slot repo issue 'assignees .assignees))
      (ignore-errors
        (unless (magit-get-boolean "forge.kludge-for-issue-294")
          (forge--set-id-slot repo issue 'labels .labels))))
    issue))

;;;; Pullreqs

(cl-defmethod forge--update-pullreqs ((repo forge-github-repository) data
                                      &optional bump)
  (closql-with-transaction (forge-db)
    (let ((initial-pull (not (oref repo pullreqs-until))))
      (mapc (lambda (e) (forge--update-pullreq repo e bump initial-pull)) data))))

(cl-defmethod forge--update-pullreq ((repo forge-github-repository) data
                                     &optional bump initial-pull)
  (let (pullreq-id pullreq)
    (let-alist data
      (closql-with-transaction (forge-db)
        (setq pullreq-id (forge--object-id 'forge-pullreq repo .number))
        (setq pullreq (or (forge-get-pullreq repo .number)
                          (closql-insert
                           (forge-db)
                           (forge-pullreq :id         pullreq-id
                                          :repository (oref repo id)
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
        (oset pullreq milestone    (and .milestone.id
                                        (forge--object-id (oref repo id)
                                                          .milestone.id)))
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
      (ignore-errors
        (forge--set-id-slot repo pullreq 'assignees .assignees))
      (ignore-errors
        (forge--set-id-slot repo pullreq 'review-requests
                            (--map (cdr (cadr (car it)))
                                   .reviewRequests)))
      (ignore-errors
        (unless (magit-get-boolean "forge.kludge-for-issue-294")
          (forge--set-id-slot repo pullreq 'labels .labels))))
    pullreq))

;;;; Notifications

(cl-defmethod forge--pull-notifications
  ((_class (subclass forge-github-repository)) githost &optional callback)
  ;; The GraphQL API doesn't support notifications and support in the
  ;; REST API is abysmal -- forcing us to perform a major rain dance.
  (let ((spec (forge--get-forge-host githost t)))
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
         (groups (-partition-all 50 notifs))
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
                                   (setq query (cl-delete-if
                                                (lambda (e) (memq e notfound))
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
           (type (if (eq type 'pullrequest) 'pullreq type)))
      (and (memq type '(pullreq issue))
           (let* ((number (and (string-match "[0-9]*\\'" .subject.url)
                               (string-to-number (match-string 0 .subject.url))))
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
             (list alias id
                   `((,alias repository)
                     [(name ,name)
                      (owner ,owner)]
                     ,@(cddr
                        (caddr
                         (ghub--graphql-prepare-query
                          ghub-fetch-repository
                          (if (eq type 'issue)
                              `(repository issues (issue . ,number))
                            `(repository pullRequest (pullRequest . ,number)))
                          ))))
                   repo type data))))))

(defun forge--ghub-update-notifications (notifs topics initial-pull)
  (closql-with-transaction (forge-db)
    (pcase-dolist (`(,alias ,id ,_ ,repo ,type ,data) notifs)
      (let-alist data
        (and-let*
            ((topic-data (cdr (cadr (assq alias topics))))
             (topic (funcall (if (eq type 'issue)
                                 #'forge--update-issue
                               #'forge--update-pullreq)
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
   (apply-partially #'forge--batch-add-callback (forge--as-githost host) user)))

(cl-defmethod forge--add-organization-repos
  ((class (subclass forge-github-repository)) host org)
  (forge--fetch-organization-repos
   class (forge--as-apihost host) org
   (apply-partially #'forge--batch-add-callback (forge--as-githost host) org)))

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
              (--map (alist-get 'name it)
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
              (--map (alist-get 'name it)
                     (let-alist d .organization.repositories))))
   nil :auth 'forge :host host))

(defun forge--batch-add-callback (host owner names)
  (let ((repos (cl-mapcan
                (lambda (name)
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

(cl-defmethod forge--submit-create-issue ((_ forge-github-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (forge--ghub-post repo "/repos/:owner/:repo/issues"
      `((title . , .title)
        (body  . , .body)
        ,@(and .labels    (list (cons 'labels    .labels)))
        ,@(and .assignees (list (cons 'assignees .assignees))))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

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
  (let-alist (forge--topic-parse-buffer)
    (when (and .yaml (local-variable-p 'forge-buffer-draft-p))
      (user-error "Cannot use yaml frontmatter and set `%s' at the same time"
                  'forge-buffer-draft-p))
    (pcase-let* ((`(,base-remote . ,base-branch)
                  (magit-split-branch-name forge--buffer-base-branch))
                 (`(,head-remote . ,head-branch)
                  (magit-split-branch-name forge--buffer-head-branch))
                 (head-repo (forge-get-repository :stub head-remote))
                 (url-mime-accept-string
                  ;; Support draft pull-requests.
                  "application/vnd.github.shadow-cat-preview+json"))
      (forge--ghub-post repo "/repos/:owner/:repo/pulls"
        `((title . , .title)
          (body  . , .body)
          (base  . ,base-branch)
          (head  . ,(if (equal head-remote base-remote)
                        head-branch
                      (concat (oref head-repo owner) ":"
                              head-branch)))
          (draft . ,(if (local-variable-p 'forge-buffer-draft-p)
                        forge-buffer-draft-p
                      .draft))
          (maintainer_can_modify . t))
        :callback  (forge--post-submit-callback)
        :errorback (forge--post-submit-errorback)))))

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
        (let-alist (forge--topic-parse-buffer)
          `((title . , .title)
            (body  . , .body)))
      `((body . ,(string-trim (buffer-string)))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

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
      ('completed '((state . "closed") (state_reason . "completed")))
      ('unplanned '((state . "closed") (state_reason . "not_planned")))
      ('rejected  '((state . "closed")))
      ('open      '((state . "open"))))
    :callback (forge--set-field-callback topic)))

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
  ((_repo forge-github-repository) topic labels)
  (funcall (if labels #'forge--ghub-put #'forge--ghub-delete)
           topic "/repos/:owner/:repo/issues/:number/labels" nil
           :payload labels
           :callback (forge--set-field-callback topic)))

(cl-defmethod forge--set-topic-assignees
  ((_repo forge-github-repository) topic assignees)
  (let ((value (mapcar #'cadr (oref topic assignees))))
    ;; FIXME Only refresh once.
    (when-let ((add (cl-set-difference assignees value :test #'equal)))
      (forge--ghub-post topic "/repos/:owner/:repo/issues/:number/assignees"
        `((assignees . ,add))
        :callback (forge--set-field-callback topic)))
    (when-let ((remove (cl-set-difference value assignees :test #'equal)))
      (forge--ghub-delete topic "/repos/:owner/:repo/issues/:number/assignees"
        `((assignees . ,remove))
        :callback (forge--set-field-callback topic)))))

(cl-defmethod forge--set-topic-review-requests
  ((_repo forge-github-repository) topic reviewers)
  (let ((value (mapcar #'cadr (oref topic review-requests))))
    ;; FIXME Only refresh once.
    (when-let ((add (cl-set-difference reviewers value :test #'equal)))
      (forge--ghub-post topic
        "/repos/:owner/:repo/pulls/:number/requested_reviewers"
        `((reviewers . ,add))
        :callback (forge--set-field-callback topic)))
    (when-let ((remove (cl-set-difference value reviewers :test #'equal)))
      (forge--ghub-delete topic
        "/repos/:owner/:repo/pulls/:number/requested_reviewers"
        `((reviewers . ,remove))
        :callback (forge--set-field-callback topic)))))

(cl-defmethod forge--delete-comment
  ((_repo forge-github-repository) post)
  (forge--ghub-delete post "/repos/:owner/:repo/issues/comments/:number")
  (closql-delete post)
  (forge-refresh-buffer))

(cl-defmethod forge--topic-template-files ((repo forge-github-repository)
                                           (_ (subclass forge-issue)))
  (and-let* ((files (magit-revision-files (oref repo default-branch))))
    (let ((case-fold-search t))
      (if-let ((file (--first (string-match-p "\
\\`\\(\\|docs/\\|\\.github/\\)issue_template\\(\\.[a-zA-Z0-9]+\\)?\\'" it)
                              files)))
          (list file)
        (setq files
              (--filter (string-match-p "\\`\\.github/ISSUE_TEMPLATE/[^/]*" it)
                        files))
        (if-let ((conf (cl-find-if
                        (lambda (f)
                          (equal (file-name-nondirectory f) "config.yml"))
                        files)))
            (nconc (delete conf files)
                   (list conf))
          files)))))

(cl-defmethod forge--topic-template-files ((repo forge-github-repository)
                                           (_ (subclass forge-pullreq)))
  (and-let* ((files (magit-revision-files (oref repo default-branch))))
    (let ((case-fold-search t))
      (if-let ((file (--first (string-match-p "\
\\`\\(\\|docs/\\|\\.github/\\)pull_request_template\\(\\.[a-zA-Z0-9]+\\)?\\'" it)
                              files)))
          (list file)
        ;; Unlike for issues, the web interface does not support
        ;; multiple pull-request templates.  The API does though,
        ;; but due to this limitation I doubt many people use them,
        ;; so Forge doesn't support them either.
        ))))

(cl-defmethod forge--set-default-branch ((repo forge-github-repository)
                                         newname oldname)
  (forge--ghub-post repo
    (format "/repos/:owner/:name/branches/%s/rename" oldname)
    `((new_name . ,newname)))
  (message "Waiting 5 seconds for GitHub to complete rename...")
  (sleep-for 5)
  (message "Waiting 5 seconds for GitHub to complete rename...done")
  (magit-call-git "fetch" "--prune" (oref repo remote))
  (magit--set-default-branch newname oldname))

(cl-defmethod forge--fork-repository ((repo forge-github-repository) fork)
  (with-slots (owner name) repo
    (forge--ghub-post repo
      (format "/repos/%s/%s/forks" owner name)
      (and (not (equal fork (ghub--username (ghub--host nil))))
           `((organization . ,fork))))
    (ghub-wait (format "/repos/%s/%s" fork name) nil :auth 'forge)))

(cl-defmethod forge--merge-pullreq ((_repo forge-github-repository)
                                    topic hash method)
  (forge--ghub-put topic
    "/repos/:owner/:repo/pulls/:number/merge"
    `((merge_method . ,(symbol-name method))
      ,@(and hash `((sha . ,hash))))))

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
(provide 'forge-github)
;;; forge-github.el ends here
