;;; forge-gitlab.el --- Gitlab support  -*- lexical-binding:t -*-

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
(require 'forge-issue)
(require 'forge-pullreq)

;;; Class

(defclass forge-gitlab-repository (forge-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#note_%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/merge_requests")
   (pullreq-url-format        :initform "https://%h/%o/%n/merge_requests/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/merge_requests/%i#note_%I")
   (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format         :initform "https://%h/%o/%n/commits/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   (blob-url-format           :initform "https://%h/%o/%n/-/blob/%r/%f")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/merge_requests/new")
   (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))

;;; Pull
;;;; Repository

(cl-defmethod forge--pull ((repo forge-gitlab-repository)
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
                   ((not (assq 'forks val))
                    (forge--fetch-forks repo cb))
                   ((not (assq 'labels val))
                    (forge--fetch-labels repo cb))
                   ((and .issues_enabled
                         (not (assq 'issues val)))
                    (forge--fetch-issues repo cb since))
                   ((and .merge_requests_enabled
                         (not (assq 'pullreqs val)))
                    (forge--fetch-pullreqs repo cb since))
                   (t
                    (forge--msg repo t t   "Pulling REPO")
                    (forge--msg repo t nil "Storing REPO")
                    (closql-with-transaction (forge-db)
                      (forge--update-repository repo val)
                      (forge--update-assignees  repo .assignees)
                      (forge--update-labels     repo .labels)
                      (dolist (v .issues)   (forge--update-issue repo v))
                      (dolist (v .pullreqs) (forge--update-pullreq repo v))
                      (oset repo condition :tracked))
                    (forge--msg repo t t "Storing REPO")
                    (cond
                     ((oref repo selective-p))
                     (callback (funcall callback))
                     ((forge--maybe-git-fetch repo buf))))))))))
    (funcall cb cb)))

(cl-defmethod forge--fetch-repository ((repo forge-gitlab-repository) callback)
  (forge--glab-get repo "/projects/:project" nil
    :callback (lambda (value _headers _status _req)
                (cond ((oref repo selective-p)
                       (setq value (append '((assignees) (forks) (labels)
                                             (issues) (pullreqs))
                                           value)))
                      ((magit-get-boolean "forge.omitExpensive")
                       (setq value (append '((assignees) (forks) (labels))
                                           value))))
                (funcall callback callback value))))

(cl-defmethod forge--update-repository ((repo forge-gitlab-repository) data)
  (let-alist data
    (oset repo created        .created_at)
    (oset repo updated        .last_activity_at)
    (oset repo pushed         nil)
    (oset repo parent         .forked_from_project.path_with_namespace)
    (oset repo description    .description)
    (oset repo homepage       nil)
    (oset repo default-branch .default_branch)
    (oset repo archived-p     .archived)
    (oset repo fork-p         (and .forked_from_project.path_with_namespace t))
    (oset repo locked-p       nil)
    (oset repo mirror-p       .mirror)
    (oset repo private-p      (equal .visibility "private"))
    (oset repo issues-p       .issues_enabled)
    (oset repo wiki-p         .wiki_enabled)
    (oset repo stars          .star_count)
    (oset repo watchers       .star_count)))

;;;; Topics

(cl-defmethod forge--pull-topic ((repo forge-gitlab-repository) _topic
                                 &key callback _errorback)
  (forge--pull repo callback)) ; TODO Pull only the one topic.

;;;; Issues

(cl-defmethod forge--fetch-issues ((repo forge-gitlab-repository) callback since)
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
    (forge--glab-get repo "/projects/:project/issues"
      `((per_page . 100)
        (order_by . "updated_at")
        ,@(and-let* ((after (or since (oref repo issues-until))))
            `((updated_after . ,after))))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (funcall cb cb value)))))

(cl-defmethod forge--fetch-issue-posts ((repo forge-gitlab-repository) cur cb)
  (let-alist (car cur)
    (forge--glab-get repo
      (format "/projects/%s/issues/%s/notes" .project_id .iid)
      '((per_page . 100))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'notes (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-issue ((repo forge-gitlab-repository) data)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((issue-id (forge--object-id 'forge-issue repo .iid))
             (issue
              (forge-issue
               :id           issue-id
               :their-id     .iid
               :number       .iid
               :slug         (format "#%s" .iid)
               :repository   (oref repo id)
               ;; Gitlab doesn't make a distinction between completed
               ;; and unplanned issues.  Treat them all as completed.
               :state        (pcase-exhaustive .state
                               ("closed" 'completed)
                               ("opened" 'open))
               :author       .author.username
               :title        .title
               :created      .created_at
               :updated      .updated_at
               ;; `.closed_at' may be nil even though the issues is
               ;; closed.  In such cases use 1, so that this slot
               ;; at least can serve as a boolean.
               :closed       (or .closed_at (and (equal .state "closed") 1))
               :locked-p     .discussion_locked
               :milestone    .milestone.iid
               :body         (forge--sanitize-string .description))))
        (closql-insert (forge-db) issue t)
        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-connections repo issue 'assignees .assignees)
          (forge--set-connections repo issue 'labels .labels))
        (dolist (c .notes)
          (let-alist c
            (let ((post
                   (forge-issue-post
                    :id      (forge--object-id issue-id .id)
                    :issue   issue-id
                    :number  .id
                    :author  .author.username
                    :created .created_at
                    :updated .updated_at
                    :body    (forge--sanitize-string .body))))
              (closql-insert (forge-db) post t))))
        (let ((until (oref repo issues-until)))
          (when (or (not until) (string> .updated_at until))
            (oset repo issues-until .updated_at)))
        issue))))

;;;; Pullreqs

(cl-defmethod forge--fetch-pullreqs ((repo forge-gitlab-repository) callback since)
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
                 ((not (assq 'source_project (car cur)))
                  (forge--fetch-pullreq-source-repo repo cur cb))
                 ((not (assq 'target_project (car cur)))
                  (forge--fetch-pullreq-target-repo repo cur cb))
                 (t
                  (if (setq cur (cdr cur))
                      (progn
                        (cl-incf pos)
                        (forge--msg nil nil nil "Pulling pullreq %s/%s" pos cnt)
                        (forge--fetch-pullreq-posts repo cur cb))
                    (forge--msg repo t t "Pulling REPO pullreqs")
                    (funcall callback callback (cons 'pullreqs val)))))))))
    (forge--msg repo t nil "Pulling REPO pullreqs")
    (forge--glab-get repo "/projects/:project/merge_requests"
      `((per_page . 100)
        (order_by . "updated_at")
        ,@(and-let* ((after (or since (oref repo pullreqs-until))))
            `((updated_after . ,after))))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (funcall cb cb value)))))

(cl-defmethod forge--fetch-pullreq-posts
  ((repo forge-gitlab-repository) cur cb)
  (let-alist (car cur)
    (forge--glab-get repo
      (format "/projects/%s/merge_requests/%s/notes" .target_project_id .iid)
      '((per_page . 100))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'notes (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--fetch-pullreq-source-repo
  ((repo forge-gitlab-repository) cur cb)
  ;; If the fork no longer exists, then `.source_project_id' is nil.
  ;; This will lead to difficulties later on but there is nothing we
  ;; can do about it.
  (let-alist (car cur)
    (if .source_project_id
        (forge--glab-get repo (format "/projects/%s" .source_project_id) nil
          :errorback (lambda (_err _headers _status _req)
                       (setf (alist-get 'source_project (car cur)) nil)
                       (funcall cb cb))
          :callback (lambda (value _headers _status _req)
                      (setf (alist-get 'source_project (car cur)) value)
                      (funcall cb cb)))
      (setf (alist-get 'source_project (car cur)) nil)
      (funcall cb cb))))

(cl-defmethod forge--fetch-pullreq-target-repo
  ((repo forge-gitlab-repository) cur cb)
  (let-alist (car cur)
    (forge--glab-get repo (format "/projects/%s" .target_project_id) nil
      :errorback (lambda (_err _headers _status _req)
                   (setf (alist-get 'target_project (car cur)) nil)
                   (funcall cb cb))
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'target_project (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-pullreq ((repo forge-gitlab-repository) data)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .iid))
             (pullreq
              (forge-pullreq
               :id           pullreq-id
               :their-id     .iid
               :number       .iid
               :slug         (format "!%s" .iid)
               :repository   (oref repo id)
               :state        (pcase-exhaustive .state
                               ("merged" 'merged)
                               ("closed" 'rejected)
                               ("opened" 'open))
               :author       .author.username
               :title        .title
               :created      .created_at
               :updated      .updated_at
               ;; `.merged_at' and `.closed_at' may both be nil even
               ;; though the pullreq is merged or otherwise closed.
               ;; In such cases use 1, so that these slots at least
               ;; can serve as booleans.
               :closed       (or .closed_at
                                 (and (member .state '("closed" "merged")) 1))
               :merged       (or .merged_at
                                 (and (equal .state "merged") 1))
               :draft-p      .draft
               :locked-p     .discussion_locked
               :editable-p   .allow_maintainer_to_push
               :cross-repo-p (not (equal .source_project_id
                                         .target_project_id))
               :base-ref     .target_branch
               :base-rev     .diff_refs.start_sha
               :base-repo    .target_project.path_with_namespace
               :head-ref     .source_branch
               :head-rev     .diff_refs.head_sha
               :head-user    .source_project.owner.username
               :head-repo    .source_project.path_with_namespace
               :milestone    .milestone.iid
               :body         (forge--sanitize-string .description))))
        (closql-insert (forge-db) pullreq t)
        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-connections repo pullreq 'assignees .assignees)
          (forge--set-connections repo pullreq 'review-requests .reviewers)
          (forge--set-connections repo pullreq 'labels .labels))
        (dolist (c .notes)
          (let-alist c
            (let ((post
                   (forge-pullreq-post
                    :id      (forge--object-id pullreq-id .id)
                    :pullreq pullreq-id
                    :number  .id
                    :author  .author.username
                    :created .created_at
                    :updated .updated_at
                    :body    (forge--sanitize-string .body))))
              (closql-insert (forge-db) post t))))
        (let ((until (oref repo pullreqs-until)))
          (when (or (not until) (string> .updated_at until))
            (oset repo pullreqs-until .updated_at)))
        pullreq))))

;;;; Other

;; The extend of the documentation for "GET /projects/:id/users" is
;; "Get the users list of a project."  I don't know what that means,
;; but it stands to reason that this must at least overlap with the
;; set of users that can be assigned to topics.

(cl-defmethod forge--fetch-assignees ((repo forge-gitlab-repository) callback)
  (forge--glab-get repo "/projects/:project/users"
    '((per_page . 100))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'assignees value)))))

(cl-defmethod forge--update-assignees ((repo forge-gitlab-repository) data)
  (oset repo assignees
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      ;; For other forges we don't need to store `id'
                      ;; but here we do because that's what has to be
                      ;; used when assigning issues.
                      (list (forge--object-id id .id)
                            .username
                            .name
                            .id)))
                  data))))

(cl-defmethod forge--fetch-forks ((repo forge-gitlab-repository) callback)
  (forge--glab-get repo "/projects/:project/forks"
    '((per_page . 100)
      (simple . t))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'forks value)))))

(cl-defmethod forge--update-forks ((repo forge-gitlab-repository) data)
  (oset repo forks
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (nconc (forge--repository-ids
                              (eieio-object-class repo)
                              (oref repo githost)
                              .namespace.path
                              .path)
                             (list .namespace.path
                                   .path))))
                  data))))

(cl-defmethod forge--fetch-labels ((repo forge-gitlab-repository) callback)
  (forge--glab-get repo "/projects/:project/labels"
    '((per_page . 100))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'labels value)))))

(cl-defmethod forge--update-labels ((repo forge-gitlab-repository) data)
  (oset repo labels
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      ;; We should use the label's `id' instead of its
                      ;; `name' but a topic's `labels' field is a list
                      ;; of names instead of a list of ids or an alist.
                      ;; As a result of this we cannot recognize when
                      ;; a label is renamed and a topic continues to be
                      ;; tagged with the old label name until it itself
                      ;; is modified somehow.  Additionally it leads to
                      ;; name conflicts between group and project
                      ;; labels.  See #160.  Also see the comment in
                      ;; `forge--set-connections'.
                      (list (forge--object-id id .name)
                            .name
                            (downcase .color)
                            .description)))
                  ;; For now simply remove one of the duplicates.
                  (cl-delete-duplicates data
                                        :key (##alist-get 'name %)
                                        :test #'equal)))))

;;;; Notifications

;; The closest to notifications that Gitlab provides are "events" as
;; described at https://docs.gitlab.com/ee/api/events.html.  This
;; allows us to see the last events that took place, but that is not
;; good enough because we are mostly interested in events we haven't
;; looked at yet.  Gitlab doesn't make a distinction between unread
;; and read events, so this is rather useless and we don't use it for
;; the time being.

;;; Mutations

(cl-defmethod forge--submit-create-issue ((_ forge-gitlab-repository) repo)
  (forge--glab-post repo "/projects/:project/issues"
    (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
      `((title        . ,title)
        (description  . ,body)))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-create-pullreq ((_ forge-gitlab-repository) base-repo)
  (pcase-let* ((`(,title . ,body) (forge--post-buffer-text))
               (`(,_base-remote . ,base-branch)
                (magit-split-branch-name forge--buffer-base-branch))
               (`(,head-remote . ,head-branch)
                (magit-split-branch-name forge--buffer-head-branch))
               (head-repo (forge-get-repository :stub head-remote)))
    (forge--glab-post head-repo "/projects/:project/merge_requests"
      `((title . ,(if forge--buffer-draft-p
                      (concat "Draft: " title)
                    title))
        (description . ,body)
        ;; ,@(and (not (equal head-remote base-remote))
        (target_project_id . ,(oref base-repo forge-id))
        (target_branch . ,base-branch)
        (source_branch . ,head-branch)
        (allow_collaboration . t))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-post
  ((_     forge-gitlab-repository)
   (topic forge-topic))
  (forge--glab-post topic
    (if (forge-issue-p topic)
        "/projects/:project/issues/:number/notes"
      "/projects/:project/merge_requests/:number/notes")
    `((body . ,(string-trim (buffer-str))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-edit-post
  ((_    forge-gitlab-repository)
   (post forge-post))
  (forge--glab-put post
    (cl-etypecase post
      (forge-pullreq      "/projects/:project/merge_requests/:number")
      (forge-issue        "/projects/:project/issues/:number")
      (forge-issue-post   "/projects/:project/issues/:topic/notes/:number")
      (forge-pullreq-post "/projects/:project/merge_requests/:topic/notes/:number"))
    (if (cl-typep post 'forge-topic)
        (pcase-let ((`(,title . ,body) (forge--post-buffer-text)))
          `((title . ,title)
            ;; Keep Gitlab from claiming that the user changed
            ;; the description when that isn't true.  The same
            ;; isn't necessary for the title; for that, Gitlab
            ;; performs the necessary check itself.
            ,@(and (not (equal body (oref post body)))
                   `((description . ,body)))))
      `((body . ,(string-trim (buffer-str)))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--set-topic-field
  ((_repo forge-gitlab-repository)
   (topic forge-topic)
   field value)
  (forge--glab-put topic
    (cl-typecase topic
      (forge-pullreq "/projects/:project/merge_requests/:number")
      (forge-issue   "/projects/:project/issues/:number"))
    `((,field . ,(if (and value (listp value)) (vconcat value) value)))
    :callback (forge--set-field-callback topic)))

(cl-defmethod forge--set-topic-title
  ((repo  forge-gitlab-repository)
   (topic forge-topic)
   title)
  (forge--set-topic-field repo topic 'title title))

(cl-defmethod forge--set-topic-state
  ((repo  forge-gitlab-repository)
   (topic forge-topic)
   state)
  (forge--set-topic-field repo topic 'state_event
                          (pcase-exhaustive state
                            ;; Merging isn't done through here.
                            ('completed "close")
                            ('unplanned "close")
                            ('rejected  "close")
                            ('open      "reopen"))))

(cl-defmethod forge--set-topic-draft
  ((repo  forge-gitlab-repository)
   (topic forge-topic)
   value)
  (forge--mutate-field topic mergeRequestSetDraft
    ((projectPath (oref repo slug))
     (iid (number-to-string (oref topic number)))
     (draft value))))

(cl-defmethod forge--set-topic-labels
  ((repo  forge-gitlab-repository)
   (topic forge-topic)
   labels)
  (forge--set-topic-field repo topic 'labels
                          (string-join labels ",")))

(cl-defmethod forge--set-topic-assignees
  ((repo  forge-gitlab-repository)
   (topic forge-topic)
   assignees)
  (let ((users (mapcar #'cdr (oref repo assignees))))
    (cl-typecase topic
      (forge-pullreq ; Can only be assigned to a single user.
       (forge--set-topic-field repo topic 'assignee_id
                               (or (caddr (assoc (car assignees) users))
                                   0)))
      (forge-issue
       (forge--set-topic-field repo topic 'assignee_ids
                               (or (mapcar (##caddr (assoc % users)) assignees)
                                   0))))))

(cl-defmethod forge--set-topic-review-requests
  ((repo  forge-gitlab-repository)
   (topic forge-pullreq)
   reviewers)
  (let ((users (mapcar #'cdr (oref repo assignees))))
    (forge--set-topic-field repo topic 'reviewer_ids
                            (or (mapcar (##caddr (assoc % users)) reviewers)
                                0))))

(cl-defmethod forge--delete-comment
  ((_    forge-gitlab-repository)
   (post forge-post))
  (forge--glab-delete post
    (cl-etypecase post
      (forge-pullreq-post
       "/projects/:project/merge_requests/:topic/notes/:number")
      (forge-issue-post
       "/projects/:project/issues/:topic/notes/:number")))
  (closql-delete post)
  (forge-refresh-buffer))

(cl-defmethod forge--topic-template-files ((repo forge-gitlab-repository)
                                           (_ (subclass forge-issue)))
  (forge--topic-template-files-1 repo "md" ".gitlab/issue_templates"))

(cl-defmethod forge--topic-template-files ((repo forge-gitlab-repository)
                                           (_ (subclass forge-pullreq)))
  (forge--topic-template-files-1 repo "md" ".gitlab/merge_request_templates"))

(cl-defmethod forge--fork-repository ((repo forge-gitlab-repository) fork)
  (with-slots (name apihost) repo
    (forge--glab-post repo "/projects/:project/fork"
      (and (not (equal fork (ghub--username apihost 'gitlab)))
           `((namespace . ,fork)))
      :noerror t)
    (ghub-wait (format "/projects/%s%%2F%s" (string-replace "/" "%2F" fork) name)
               nil :auth 'forge :host apihost :forge 'gitlab)))

(cl-defmethod forge--merge-pullreq
  ((_repo forge-gitlab-repository)
   (topic forge-topic)
   hash method)
  (forge--glab-put topic
    "/projects/:project/merge_requests/:number/merge"
    `((squash . ,(eq method 'squash))
      ,@(and hash `((sha . ,hash))))))

;;; Wrappers

(cl-defun forge--glab-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host callback errorback)
  (declare (indent defun))
  (ghub-request "GET" (if obj (forge--format-resource obj resource) resource)
                params
                :forge 'gitlab
                :host (or host (oref (forge-get-repository obj) apihost))
                :auth 'forge
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :callback callback
                :errorback (or errorback (and callback t))))

(cl-defun forge--glab-put (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host callback errorback)
  (declare (indent defun))
  (ghub-request "PUT" (if obj (forge--format-resource obj resource) resource)
                params
                :forge 'gitlab
                :host (or host (oref (forge-get-repository obj) apihost))
                :auth 'forge
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :callback callback
                :errorback (or errorback (and callback t))))

(cl-defun forge--glab-post (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                host callback errorback)
  (declare (indent defun))
  (ghub-request "POST" (forge--format-resource obj resource)
                params
                :forge 'gitlab
                :host (or host (oref (forge-get-repository obj) apihost))
                :auth 'forge
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :callback callback
                :errorback (or errorback (and callback t))))

(cl-defun forge--glab-delete (obj resource
                                  &optional params
                                  &key query payload headers
                                  silent unpaginate noerror reader
                                  host callback errorback)
  (declare (indent defun))
  (ghub-request "DELETE" (forge--format-resource obj resource)
                params
                :forge 'gitlab
                :host (or host (oref (forge-get-repository obj) apihost))
                :auth 'forge
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :callback callback
                :errorback (or errorback (and callback t))))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("buffer-string" . "buffer-string")
;;   ("buffer-str" . "forge--buffer-substring-no-properties"))
;; End:
(provide 'forge-gitlab)
;;; forge-gitlab.el ends here
