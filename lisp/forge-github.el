;;; forge-github.el --- Github support            -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Jonas Bernoulli

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

(require 'ghub)

(require 'forge)
(require 'forge-issue)
(require 'forge-pullreq)

;;; Variables

(defvar forge-github-token-scopes '(repo user read:org)
  "The Github API scopes needed by Forge.

+Visit https://github.com/settings/tokens to change the scopes
+of existing tokens and for a list of all available scopes see
+https://developer.github.com/apps/building-oauth-apps/understanding-scopes-for-oauth-apps.")

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

(cl-defmethod forge--pull ((repo forge-github-repository) until
                           &optional callback)
  (let ((buf (current-buffer))
        (dir default-directory))
    (ghub-fetch-repository
     (oref repo owner)
     (oref repo name)
     (lambda (data)
       (forge--msg repo t t   "Pulling REPO")
       (forge--msg repo t nil "Storing REPO")
       (emacsql-with-transaction (forge-db)
         (let-alist data
           (forge--update-repository repo data)
           (forge--update-assignees  repo .assignableUsers)
           (forge--update-forks      repo .forks)
           (forge--update-labels     repo .labels)
           (forge--update-issues     repo .issues t)
           (forge--update-pullreqs   repo .pullRequests t)
           (forge--update-revnotes   repo .commitComments))
         (setf (oref repo sparse-p) nil))
       (forge--msg repo t t   "Storing REPO")
       (cond
        (callback (funcall callback))
        (forge-pull-notifications
         (forge--pull-notifications (eieio-object-class repo)
                                    (oref repo githost)
                                    (lambda () (forge--git-fetch buf dir repo))))
        (t (forge--git-fetch buf dir repo))))
     `((issues-until       . ,(forge--topics-until repo until 'issue))
       (pullRequests-until . ,(forge--topics-until repo until 'pullreq)))
     :host (oref repo apihost)
     :auth 'forge)))

(cl-defmethod forge--pull-topic ((repo forge-github-repository) n
                                 &optional pullreqp)
  (unless pullreqp
    (setq pullreqp (forge-get-pullreq repo n)))
  (let ((buffer (current-buffer)))
    (funcall
     (if pullreqp #'ghub-fetch-pullreq #'ghub-fetch-issue)
     (oref repo owner)
     (oref repo name)
     n
     (lambda (data)
       (funcall (if pullreqp #'forge--update-pullreq #'forge--update-issue)
                repo data nil)
       (with-current-buffer
           (if (buffer-live-p buffer) buffer (current-buffer))
         (magit-refresh)))
     nil
     :errorback
     (and (not pullreqp)
          (lambda (err _headers _status _req)
            (when (equal (cdr (assq 'type (cadr err))) "NOT_FOUND")
              (forge--pull-topic repo n t))))
     :host (oref repo apihost)
     :auth 'forge)))

(cl-defmethod forge--update-repository ((repo forge-github-repository) data)
  (let-alist data
    (setf (oref repo created)        .createdAt)
    (setf (oref repo updated)        .updatedAt)
    (setf (oref repo pushed)         .pushedAt)
    (setf (oref repo parent)         .parent.nameWithOwner)
    (setf (oref repo description)    .description)
    (setf (oref repo homepage)       (and (not (equal .homepageUrl "")) .homepageUrl))
    (setf (oref repo default-branch) .defaultBranchRef.name)
    (setf (oref repo archived-p)     .isArchived)
    (setf (oref repo fork-p)         .isFork)
    (setf (oref repo locked-p)       .isLocked)
    (setf (oref repo mirror-p)       .isMirror)
    (setf (oref repo private-p)      .isPrivate)
    (setf (oref repo issues-p)       .hasIssuesEnabled)
    (setf (oref repo wiki-p)         .hasWikiEnabled)
    (setf (oref repo stars)          .stargazers.totalCount)
    (setf (oref repo watchers)       .watchers.totalCount)))

(cl-defmethod forge--update-issues ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (mapc (lambda (e) (forge--update-issue repo e bump)) data)))

(cl-defmethod forge--update-issue ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let* ((issue-id (forge--object-id 'forge-issue repo .number))
             (issue (or (forge-get-issue repo .number)
                        (closql-insert
                         (forge-db)
                         (forge-issue :id         issue-id
                                      :repository (oref repo id)
                                      :number     .number)))))
        (setf (oref issue state)      (pcase-exhaustive .state
                                        ("CLOSED" 'closed)
                                        ("OPEN"   'open)))
        (setf (oref issue author)     .author.login)
        (setf (oref issue title)      .title)
        (setf (oref issue created)    .createdAt)
        (setf (oref issue updated)    (cond (bump (or .updatedAt .createdAt))
                                            ((slot-boundp issue 'updated)
                                             (oref issue updated))
                                            (t "0")))
        (setf (oref issue closed)     .closedAt)
        (setf (oref issue locked-p)   .locked)
        (setf (oref issue milestone)  .milestone)
        (setf (oref issue body)       (forge--sanitize-string .body))
        .databaseId ; Silence Emacs 25 byte-compiler.
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
        (when bump
          (forge--set-id-slot repo issue 'assignees .assignees)
          (forge--set-id-slot repo issue 'labels .labels))
        issue))))

(cl-defmethod forge--update-pullreqs ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (mapc (lambda (e) (forge--update-pullreq repo e bump)) data)))

(cl-defmethod forge--update-pullreq ((repo forge-github-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .number))
             (pullreq (or (forge-get-pullreq repo .number)
                          (closql-insert
                           (forge-db)
                           (forge-pullreq :id           pullreq-id
                                          :repository   (oref repo id)
                                          :number       .number)))))
        (setf (oref pullreq state)        (pcase-exhaustive .state
                                            ("MERGED" 'merged)
                                            ("CLOSED" 'closed)
                                            ("OPEN"   'open)))
        (setf (oref pullreq author)       .author.login)
        (setf (oref pullreq title)        .title)
        (setf (oref pullreq created)      .createdAt)
        (setf (oref pullreq updated)      (cond (bump (or .updatedAt .createdAt))
                                                ((slot-boundp pullreq 'updated)
                                                 (oref pullreq updated))
                                                (t "0")))
        (setf (oref pullreq closed)       .closedAt)
        (setf (oref pullreq merged)       .mergedAt)
        (setf (oref pullreq locked-p)     .locked)
        (setf (oref pullreq editable-p)   .maintainerCanModify)
        (setf (oref pullreq cross-repo-p) .isCrossRepository)
        (setf (oref pullreq base-ref)     .baseRef.name)
        (setf (oref pullreq base-repo)    .baseRef.repository.nameWithOwner)
        (setf (oref pullreq head-ref)     .headRef.name)
        (setf (oref pullreq head-user)    .headRef.repository.owner.login)
        (setf (oref pullreq head-repo)    .headRef.repository.nameWithOwner)
        (setf (oref pullreq milestone)    .milestone)
        (setf (oref pullreq body)         (forge--sanitize-string .body))
        .databaseId ; Silence Emacs 25 byte-compiler.
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
        (when bump
          (forge--set-id-slot repo pullreq 'assignees .assignees)
          (forge--set-id-slot repo pullreq 'review-requests
                              (--map (cdr (cadr (car it)))
                                     .reviewRequests))
          (forge--set-id-slot repo pullreq 'labels .labels))
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
  (setf (oref repo assignees)
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .login
                            .name
                            .id)))
                  (delete-dups data)))))

(cl-defmethod forge--update-forks ((repo forge-github-repository) data)
  (setf (oref repo forks)
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
  (setf (oref repo labels)
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .name
                            (concat "#" (downcase .color))
                            .description)))
                  (delete-dups data)))))

;;;; Notifications

(cl-defmethod forge--pull-notifications
  ((_class (subclass forge-github-repository)) githost &optional callback)
  ;; The GraphQL API doesn't support notifications and also likes to
  ;; timeout for handcrafted requests, forcing us to perform a major
  ;; rain dance.
  (let ((spec (assoc githost forge-alist)))
    (unless spec
      (error "No entry for %S in forge-alist" githost))
    (forge--msg nil t nil "Pulling notifications")
    (pcase-let*
        ((`(,_ ,apihost ,forge ,_) spec)
         (notifs (-keep (lambda (data)
                          ;; Github may return notifications for repos
                          ;; the user no longer has access to.  Trying
                          ;; to retrieve information for such a repo
                          ;; leads to an error, which we suppress.  See #164.
                          (with-demoted-errors "forge--pull-notifications: %S"
                            (forge--ghub-massage-notification
                             data forge githost)))
                        (forge--ghub-get nil "/notifications"
                                         '((all . "true"))
                                         :host apihost :unpaginate t)))
         (groups (-partition-all 50 notifs))
         (pages  (length groups))
         (page   0)
         (result nil))
      (cl-labels
          ((cb (&optional data _headers _status _req)
               (when data
                 (setq result (nconc result (cdr data))))
               (if groups
                   (progn (cl-incf page)
                          (forge--msg nil t nil
                                      "Pulling notifications (page %s/%s)"
                                      page pages)
                          (ghub--graphql-vacuum
                           (cons 'query (-keep #'caddr (pop groups)))
                           nil #'cb nil :auth 'forge))
                 (forge--msg nil t t   "Pulling notifications")
                 (forge--msg nil t nil "Storing notifications")
                 (emacsql-with-transaction (forge-db)
                   (forge-sql [:delete-from notification
                               :where (= forge $s1)] forge)
                   (pcase-dolist (`(,key ,repo ,query ,obj) notifs)
                     (closql-insert (forge-db) obj)
                     (when query
                       (setf (oref (funcall (if (eq (oref obj type) 'issue)
                                                #'forge--update-issue
                                              #'forge--update-pullreq)
                                            repo (cdr (cadr (assq key result))) nil)
                                   unread-p) (oref obj unread-p)))))
                 (forge--msg nil t t "Storing notifications")
                 (when callback
                   (funcall callback)))))
        (cb)))))

(defun forge--ghub-massage-notification (data forge githost)
  (let-alist data
    (let* ((type   (intern (downcase .subject.type)))
           (type   (if (eq type 'pullrequest) 'pullreq type))
           (number (and (string-match "[0-9]*\\'" .subject.url)
                        (string-to-number (match-string 0 .subject.url))))
           (repo   (forge-get-repository
                    (list githost
                          .repository.owner.login
                          .repository.name)
                    nil 'create))
           (repoid (oref repo id))
           (owner  (oref repo owner))
           (name   (oref repo name))
           (id     (forge--object-id repoid (string-to-number .id)))
           (alias  (intern (concat "_" (replace-regexp-in-string "=" "_" id)))))
      (and (memq type '(pullreq issue))
           (list alias repo
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
                 (forge-notification
                  :id           id
                  :thread-id    .id
                  :repository   repoid
                  :forge        forge
                  :reason       (intern (downcase .reason))
                  :unread-p     .unread
                  :last-read    .last_read_at
                  :updated      .updated_at
                  :title        .subject.title
                  :type         type
                  :topic        number
                  :url          .subject.url))))))

(cl-defmethod forge-topic-mark-read ((_ forge-github-repository) topic)
  (when (oref topic unread-p)
    (setf (oref topic unread-p) nil)
    (when-let ((notif (forge-get-notification topic)))
      (setf (oref topic unread-p) nil)
      (forge--ghub-patch notif "/notifications/threads/:thread-id"))))

;;;; Miscellaneous

(cl-defmethod forge--add-user-repos
  ((class (subclass forge-github-repository)) host user)
  (forge--fetch-user-repos
   class (forge--as-apihost host) user
   (apply-partially 'forge--batch-add-callback (forge--as-githost host) user)))

(cl-defmethod forge--add-organization-repos
  ((class (subclass forge-github-repository)) host org)
  (forge--fetch-organization-repos
   class (forge--as-apihost host) org
   (apply-partially 'forge--batch-add-callback (forge--as-githost host) org)))

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
   nil :host host))

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
   nil :host host))

(defun forge--batch-add-callback (host owner names)
  (let ((repos (cl-mapcan (lambda (name)
                            (let ((repo (forge-get-repository
                                         (list host owner name)
                                         nil 'create)))
                              (and (oref repo sparse-p)
                                   (list repo))))
                          names))
        cb)
    (setq cb (lambda ()
               (when-let ((repo (pop repos)))
                 (message "Adding %s..." (oref repo name))
                 (forge--pull repo nil cb))))
    (funcall cb)))

;;; Mutations

(cl-defmethod forge--create-pullreq-from-issue ((repo forge-github-repository)
                                                issue source target)
  (pcase-let* ((`(,base-remote . ,base-branch)
                (magit-split-branch-name target))
               (`(,head-remote . ,head-branch)
                (magit-split-branch-name source))
               (head-repo (forge-get-repository 'stub head-remote))
               (issue-obj (forge-get-issue repo issue)))
    (forge--ghub-post repo "/repos/:owner/:repo/pulls"
                      `((issue . ,issue)
                        (base  . ,base-branch)
                        (head  . ,(if (equal head-remote base-remote)
                                      head-branch
                                    (concat (oref head-repo owner) ":"
                                            head-branch)))
                        (maintainer_can_modify . t))
                      :callback  (lambda (&rest _)
                                   (closql-delete issue-obj)
                                   (forge-pull))
                      :errorback (lambda (&rest _) (forge-pull)))))

(cl-defmethod forge--submit-create-pullreq ((_ forge-github-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (pcase-let* ((`(,base-remote . ,base-branch)
                  (magit-split-branch-name forge--buffer-base-branch))
                 (`(,head-remote . ,head-branch)
                  (magit-split-branch-name forge--buffer-head-branch))
                 (head-repo (forge-get-repository 'stub head-remote))
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
                          (draft . ,(and (member .draft '("t" "true" "yes"))
                                         t))
                          (maintainer_can_modify . t))
                        :callback  (forge--post-submit-callback)
                        :errorback (forge--post-submit-errorback)))))

(cl-defmethod forge--submit-create-issue ((_ forge-github-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (forge--ghub-post repo "/repos/:owner/:repo/issues"
                      `((title . , .title)
                        (body  . , .body)
                        ,@(and .labels    (list (cons 'labels    .labels)))
                        ,@(and .assignees (list (cons 'assignees .assignees))))
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
                     :callback (forge--set-field-callback)))

(cl-defmethod forge--set-topic-state
  ((_repo forge-github-repository) topic)
  (forge--ghub-patch topic
                     "/repos/:owner/:repo/issues/:number"
                     `((state . ,(cl-ecase (oref topic state)
                                   (closed "OPEN")
                                   (open   "CLOSED"))))
                     :callback (forge--set-field-callback)))

(cl-defmethod forge--set-topic-labels
  ((_repo forge-github-repository) topic labels)
  (forge--ghub-put topic "/repos/:owner/:repo/issues/:number/labels" nil
                   :payload labels
                   :callback (forge--set-field-callback)))

(cl-defmethod forge--delete-comment
  ((_repo forge-github-repository) post)
  (forge--ghub-delete post "/repos/:owner/:repo/issues/comments/:number")
  (closql-delete post)
  (magit-refresh))

(cl-defmethod forge--set-topic-assignees
  ((_repo forge-github-repository) topic assignees)
  (let ((value (mapcar #'car (closql--iref topic 'assignees))))
    (when-let ((add (cl-set-difference assignees value :test #'equal)))
      (forge--ghub-post
       topic "/repos/:owner/:repo/issues/:number/assignees"
       `((assignees . ,add))))
    (when-let ((remove (cl-set-difference value assignees :test #'equal)))
      (forge--ghub-delete
       topic "/repos/:owner/:repo/issues/:number/assignees"
       `((assignees . ,remove)))))
  (forge-pull))

(cl-defmethod forge--set-topic-review-requests
  ((_repo forge-github-repository) topic reviewers)
  (let ((value (mapcar #'car (closql--iref topic 'review-requests))))
    (when-let ((add (cl-set-difference reviewers value :test #'equal)))
      (forge--ghub-post
       topic "/repos/:owner/:repo/pulls/:number/requested_reviewers"
       `((reviewers . ,add))))
    (when-let ((remove (cl-set-difference value reviewers :test #'equal)))
      (forge--ghub-delete
       topic "/repos/:owner/:repo/pulls/:number/requested_reviewers"
       `((reviewers . ,remove)))))
  (forge-pull))

(cl-defmethod forge--topic-templates ((repo forge-github-repository)
                                      (_ (subclass forge-issue)))
  (when-let ((files (magit-revision-files (oref repo default-branch))))
    (let ((case-fold-search t))
      (if-let ((file (--first (string-match-p "\
\\`\\(\\|docs/\\|\\.github/\\)issue_template\\(\\.[a-zA-Z0-9]+\\)?\\'" it)
                              files)))
          (list file)
        (--filter (string-match-p "\\`\\.github/ISSUE_TEMPLATE/[^/]*" it)
                  files)))))

(cl-defmethod forge--topic-templates ((repo forge-github-repository)
                                      (_ (subclass forge-pullreq)))
  (when-let ((files (magit-revision-files (oref repo default-branch))))
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

(cl-defmethod forge--fork-repository ((repo forge-github-repository) fork)
  (with-slots (owner name) repo
    (forge--ghub-post repo
                      (format "/repos/%s/%s/forks" owner name)
                      (and (not (equal fork (ghub--username (ghub--host nil))))
                           `((organization . ,fork))))
    (ghub-wait (format "/repos/%s/%s" fork name) nil :auth 'forge)))

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

;;; _
(provide 'forge-github)
;;; forge-github.el ends here
