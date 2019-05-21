;;; forge-github.el --- Github support            -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Jonas Bernoulli

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

(defvar forge-github-token-scopes '(repo)
  "The Github API scopes needed by Forge.

`repo' is the only required scope.  Without this scope none of
Forge's features that use the API work.  Instead of this scope
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

(cl-defmethod forge--pull ((repo forge-github-repository) until)
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
         (oset repo sparse-p nil))
       (forge--msg repo t t   "Storing REPO")
       (if forge-pull-notifications
           (forge--pull-notifications (eieio-object-class repo)
                                      (oref repo githost)
                                      (lambda () (forge--git-fetch buf dir repo)))
         (forge--git-fetch buf dir repo)))
     `((issues-until       . ,(forge--topics-until repo until 'issue))
       (pullRequests-until . ,(forge--topics-until repo until 'pullreq)))
     :auth 'forge)))

(cl-defmethod forge--pull-pullreq ((repo forge-github-repository) pullreq)
  ;; This function is only required because of a Github bug:
  ;; https://platform.github.community/t/7284
  (let ((buffer (current-buffer)))
    (ghub-fetch-pullreq
     (oref repo owner)
     (oref repo name)
     (oref pullreq number)
     (lambda (data)
       (forge--update-pullreq repo data nil)
       (with-current-buffer
           (if (buffer-live-p buffer) buffer (current-buffer))
         (magit-refresh)))
     nil :auth 'forge)))

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
        (oset issue state      (pcase-exhaustive .state
                                 ("CLOSED" 'closed)
                                 ("OPEN"   'open)))
        (oset issue author     .author.login)
        (oset issue title      .title)
        (oset issue created    .createdAt)
        (oset issue updated    (cond (bump (or .updatedAt .createdAt))
                                     ((slot-boundp issue 'updated)
                                      (oref issue updated))
                                     (t "0")))
        (oset issue closed     .closedAt)
        (oset issue locked-p   .locked)
        (oset issue milestone  .milestone)
        (oset issue body       (forge--sanitize-string .body))
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
        (oset pullreq state        (pcase-exhaustive .state
                                     ("MERGED" 'merged)
                                     ("CLOSED" 'closed)
                                     ("OPEN"   'open)))
        (oset pullreq author       .author.login)
        (oset pullreq title        .title)
        (oset pullreq created      .createdAt)
        (oset pullreq updated      (cond (bump (or .updatedAt .createdAt))
                                         ((slot-boundp pullreq 'updated)
                                          (oref pullreq updated))
                                         (t "0")))
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
         (notifs (--keep (forge--ghub-massage-notification it forge githost)
                         (forge--ghub-get nil "/notifications"
                                          '((all . "true"))
                                          :host apihost :unpaginate t)))
         (groups (-partition-all 100 notifs))
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
                       (oset (funcall (if (eq (oref obj type) 'issue)
                                          #'forge--update-issue
                                        #'forge--update-pullreq)
                                      repo (cdr (cadr (assq key result))) nil)
                             unread-p (oref obj unread-p)))))
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
           (key    (intern (concat "_" (replace-regexp-in-string "=" "_" id)))))
      (and (memq type '(pullreq issue))
           (list key repo
                 `(,key
                   [(:alias t)]
                   (repository
                    [(name ,name)
                     (owner ,owner)]
                    ,@(cddr
                       (caddr
                        (ghub--graphql-prepare-query
                         ghub-fetch-repository
                         (if (eq type 'issue)
                             `(repository issues (issue . ,number))
                           `(repository pullRequest (pullRequest . ,number)))
                         )))))
                 (forge-notification
                  :id           id
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

;;; Mutations

(cl-defmethod forge--submit-create-pullreq ((_ forge-github-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (pcase-let* ((`(,base-remote . ,base-branch)
                  (magit-split-branch-name forge--buffer-base-branch))
                 (`(,head-remote . ,head-branch)
                  (magit-split-branch-name forge--buffer-head-branch))
                 (head-repo (forge-get-repository 'stub head-remote)))
      (forge--ghub-post repo "/repos/:owner/:repo/pulls"
                        `((title . , .title)
                          (body  . , .body)
                          (base  . ,base-branch)
                          (head  . ,(if (equal head-remote base-remote)
                                        head-branch
                                      (concat (oref head-repo owner) ":"
                                              head-branch)))
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

(cl-defmethod forge--topic-templates ((repo forge-github-repository)
                                      (_ (subclass forge-issue)))
  (when-let ((files (magit-revision-files (oref repo default-branch))))
    (if-let ((file (--first (string-match-p "\
\\`\\(\\|docs/\\|\\.github/\\)issue_template\\(\\.[a-zA-Z0-9]+\\)?\\'"
                                            (forge--topic-file-name-downcase it))
                            files)))
        (list file)
      (--filter (string-match-p "\\`\\.github/ISSUE_TEMPLATE/[^/]*" it)
                files))))

(cl-defmethod forge--topic-templates ((repo forge-github-repository)
                                      (_ (subclass forge-pullreq)))
  (when-let ((files (magit-revision-files (oref repo default-branch))))
    (if-let ((file (--first (string-match-p "\
\\`\\(\\|docs/\\|\\.github/\\)pull_request_template\\(\\.[a-zA-Z0-9]+\\)?\\'"
                                            (forge--topic-file-name-downcase it))
                              files)))
        (list file)
      ;; Unlike for issues, the web interface does not support
      ;; multiple pull-request templates.  The API does though,
      ;; but due to this limitation I doubt many people use them,
      ;; so Forge doesn't support them either.
      )))

;;; Utilities

(cl-defun forge--topic-file-name-downcase (filename)
  "Return the FILENAME with its nondirectory component downcased."
  (let ((directory (file-name-directory filename)))
    (if directory
        (concat (file-name-as-directory directory)
                (downcase (file-name-nondirectory filename)))
      (downcase filename))))

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
