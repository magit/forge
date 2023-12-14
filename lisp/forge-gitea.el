;;; forge-gitea.el --- Gitea support  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Matija Obid

;; Author: Matija Obid <matija.obid@posteo.net>
;; Maintainer: Matija Obid <matija.obid@posteo.net>

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

(require 'gtea)

(require 'forge)
(require 'forge-issue)
(require 'forge-pullreq)

;; FIXME: error in process filter: json-read: Unrecognized keyword: "f51"
;; FIXME: initial commit don't pull all topics (same for github)


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
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/pulls") ; sic
   (pullreq-refspec :initform "+refs/pull/*/head:refs/pullreqs/*")))

;;; Pull

(defvar forge--gtea-batch-size 240
  "Number of pullreqs/issues to be fetched in one page.")

;;;; Repository

(cl-defmethod forge--pull ((repo forge-gitea-repository) until)
  (setq until (or until (oref repo updated)))
  (let ((cb (let ((buf (and (derived-mode-p 'magit-mode)
                            (current-buffer)))
                  (dir default-directory)
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
                    (forge--fetch-issues repo cb until))
                   ((and .has_pull_requests
                         (not (assq 'pullreqs val)))
                    (forge--fetch-pullreqs repo cb until))
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
                      (oset repo sparse-p nil))
                    (forge--msg repo t t "Storing REPO")
                    (unless (oref repo selective-p)
                      (forge--git-fetch buf dir repo)))))))))
    (funcall cb cb)))

(cl-defmethod forge--fetch-repository ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo" nil
    :callback (lambda (value _headers _status _req)
                (when (oref repo selective-p)
                  (setq value (append '((assignees) (labels)
                                        (issues) (pullreqs))
                                      value)))
                (funcall callback callback value))))

(cl-defmethod forge--update-repository ((repo forge-gitea-repository) data)
  (let-alist data
    (oset repo created        .created_at)
    (oset repo updated        .updated_at)
    (oset repo pushed         nil)
    (oset repo parent         .parent)
    (oset repo description    .description)
    (oset repo homepage       .html_url)
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

(cl-defmethod forge--fetch-assignees ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo/assignees" nil
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'assignees value)))))

(cl-defmethod forge--update-assignees ((repo forge-gitea-repository) data)
  (oset repo assignees
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      ;; For other forges we don't need to store `id'
                      ;; but here we do because that's what has to be
                      ;; used when assigning issues.
                      (list (forge--object-id id .id)
                            .login
                            .full_name
                            .id)))
                  data))))

(cl-defmethod forge--fetch-milestones ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo/milestones" nil
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
                            .updated_at
                            .due_on
                            .closed_at
                            .description)))
                  (delete-dups data)))))

(cl-defmethod forge--fetch-labels ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo/labels" nil
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'labels value)))))

(cl-defmethod forge--update-labels ((repo forge-gitea-repository) data)
  (oset repo labels
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .name
                            (concat "#" (downcase .color))
                            .description)))
                  data))))


;;;; Issues

(defun forge--gtea-fetch-topics-cb (field repo callback)
  (let ((typ (cl-ecase field
               (issues "issues")
               (pullreqs "PRs")))
        i cnt)
    (lambda (cb topics &optional val)
      (unless cnt
        (setq i 0
              cnt (length topics)))
      (cl-incf i)
      (if topics
          (progn
            (forge--msg nil nil nil "Pulling %s %s/%s" typ i cnt)
            (forge--fetch-issue-posts
             repo topics
             (lambda (cbb)
               (if (or (eq field 'issues)
                       (not cbb))
                   (funcall cb cb (cdr topics) (cons (car topics) val))
                 ;; Load reviews for pull requests:
                 (forge--fetch-issue-reviews repo topics
                                             (lambda (_)
                                               (funcall cbb nil))
                                             t)))))
        (forge--msg nil nil t "Pulling %s %s/%s" typ i cnt)
        (funcall callback callback (cons field (nreverse val)))))))

(cl-defmethod forge--fetch-issues ((repo forge-gitea-repository) callback until)
  (let ((cb (forge--gtea-fetch-topics-cb 'issues repo callback)))
    (forge--msg repo t nil "Pulling REPO issues")
    (forge--gtea-get repo "repos/:owner/:repo/issues"
      `((limit . ,forge--gtea-batch-size)
        (type . "issues")
        (state . "all")
        ,@(and-let* ((after (forge--topics-until repo until 'issue)))
            `((updated_after . ,after))))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (if until
                      (funcall cb cb value)
                    (funcall callback callback (cons 'issues value)))))))

(cl-defmethod forge--fetch-issue-posts ((repo forge-gitea-repository) cur cb)
  (let-alist (car cur)
    (forge--gtea-get repo (format "repos/:owner/:repo/issues/%s/comments" .number) nil
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'notes (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-issue ((repo forge-gitea-repository) data)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((issue-id (forge--object-id 'forge-issue repo .id))
             (issue (or (forge-get-issue repo .number)
                        (closql-insert
                         (forge-db)
                         (forge-issue :id           issue-id
                                      :repository   (oref repo id)
                                      :number       .number)))))
        (oset issue id           issue-id)
        (oset issue slug         (format "#%s" .number))
        (oset issue their-id     .id)
        (oset issue number       .number)
        (oset issue repository   (oref repo id))
        (oset issue state        (pcase-exhaustive .state
                             ("closed" 'closed)
                             ("open" 'open)))
        (oset issue author       .user.username)
        (oset issue title        .title)
        (oset issue created      .created_at)
        (oset issue updated      .updated_at)
        (oset issue closed       (or .closed_at (and (equal .state "closed") 1)))
        (oset issue locked-p     .is_locked)
        (oset issue milestone    .milestone.title)
        (oset issue labels       (mapcar (lambda (label)
                                           (forge--object-id (oref repo id) (cdr (assoc 'id label))))
                                         .labels))
        (oset issue body         (forge--sanitize-string .body))

        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-id-slot repo issue 'assignees .assignees)
          (forge--set-id-slot repo issue 'labels .labels))
        (dolist (comment .notes)
          (let-alist comment
            (closql-insert
             (forge-db)
             (forge-issue-post
              :id      (forge--object-id issue-id .id)
              :issue    issue-id
              :number  .id
              :author  .user.login
              :created .created_at
              :updated .updated_at
              :body (let ((body (forge--sanitize-string .body))
                          (hunk .diff_hunk))
                      (if (not hunk)
                          body
                        (concat "```diff\n"
                                (string-trim hunk)
                                "\n```\n--\n"
                                body))))
             t)))
        issue))))

(cl-defmethod forge--pull-topic ((repo forge-gitea-repository)
                                 (topic forge-topic))
  (condition-case _
      (let ((data (forge--gtea-get topic "repos/:owner/:repo/pulls/:number"))
            (cb (forge--gtea-fetch-topics-cb 'pullreqs repo
                                             (lambda (_ data)
                                               (forge--update-pullreq repo (cadr data))))))
        (funcall cb cb (list data)))
    (error
     (let ((data (forge--gtea-get topic "repos/:owner/:repo/issues/:number"))
           (cb (forge--gtea-fetch-topics-cb 'issues repo
                                            (lambda (_ data)
                                              (forge--update-issue repo (cadr data))))))
       (funcall cb cb (list data))))))

;;;; Pull requests

(cl-defmethod forge--fetch-pullreqs ((repo forge-gitea-repository) callback until)
  (let ((cb (forge--gtea-fetch-topics-cb 'pullreqs repo callback))
        (until (and until (date-to-time until))))
    (forge--msg repo t nil "Pulling REPO PRs")
    (forge--gtea-get* repo "repos/:owner/:repo/pulls"
      `((limit . ,forge--gtea-batch-size)
        (sort . "recentupdate"))
      :callback (lambda (value _headers _status _req)
                  (if until
                      (funcall cb cb value)
                    (funcall callback callback (cons 'pullreqs value))))
      :while (lambda (res)
               (let-alist res
                 (or (not until) (time-less-p until (date-to-time .updated_at))))))))

(cl-defmethod forge--fetch-pullreq-posts ((repo forge-gitea-repository) cur cb)
  (let-alist (car cur)
    (forge--gtea-get repo (format "repos/:owner/:repo/pulls/%s/comments" .number) nil
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'notes (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-pullreq ((repo forge-gitea-repository) data)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .number))
             (pullreq (or (forge-get-pullreq repo .number)
                          (closql-insert
                           (forge-db)
                           (forge-pullreq :id           pullreq-id
                                          :repository   (oref repo id)
                                          :number       .number)))))
        (oset pullreq their-id     .id)
        (oset pullreq slug         (format "#%s" .number))
        (oset pullreq author       .user.login)
        (oset pullreq title        .title)
        (oset pullreq created      .created_at)
        (oset pullreq updated      .updated_at)
        (oset pullreq merged       .merged)
        (oset pullreq closed       (or .merged (string= .state "closed")))
        (oset pullreq body         (forge--sanitize-string .body))
        (oset pullreq state        (if .merged
                                       'merged
                                     (pcase-exhaustive .state
                                       ("closed" 'closed)
                                       ("open"   'open))))
        (oset pullreq base-repo    "unknown")
        (oset pullreq head-repo    "unknown")
        (oset pullreq draft-p      nil)
        (oset pullreq cross-repo-p nil)
        (oset pullreq base-ref     .base.ref)
        (oset pullreq base-rev     .merge_base)
        (oset pullreq head-ref     .head.ref)
        (oset pullreq head-rev     .head.sha)
        (oset pullreq milestone    (and .milestone.id
                                        (forge--object-id (oref repo id)
                                                          .milestone.id)))
        (oset pullreq labels       (mapcar (lambda (label)
                                             (forge--object-id (oref repo id) (cdr (assoc 'id label))))
                                           .labels))
        (forge--set-id-slot repo pullreq 'assignees .assignees)

        (forge--set-id-slot repo pullreq 'review-requests
                            (mapcar (lambda (review)
                                      (alist-get 'user review))
                                    .reviews))
        (let ((reviews (mapcan (lambda (review)
                                 (unless (string-empty-p (alist-get 'body review))
                                   (list review)))
                               .reviews))
              (review-notes (mapcan (lambda (review)
                                      (alist-get 'notes review))
                                    .reviews)))
          (dolist (comment (append .notes reviews review-notes))
            (let-alist comment
              (closql-insert
               (forge-db)
               (forge-pullreq-post
                :id      (forge--object-id pullreq-id .id)
                :pullreq pullreq-id
                :number  .id
                :author  .user.login
                :created (or .created_at .submitted_at)
                :updated (or .updated_at .submitted_at)
                :body (let ((body (forge--sanitize-string .body))
                            (hunk .diff_hunk))
                        (if (not hunk)
                            body
                          (concat "```diff\n"
                                  (string-trim hunk)
                                  "\n```\n--\n"
                                  body))))
               t))))
        pullreq))))

;;;; Reviews:
(cl-defmethod forge--fetch-issue-reviews ((repo forge-gitea-repository) cur callback &optional load-notes)
  (let-alist (car cur)
    (let ((cb (lambda (cb reviews)
                ;; TODO Add main comment of review.
                (if reviews
                    (forge--fetch-issue-review-notes repo .number reviews
                                                     (lambda (_)
                                                       (funcall cb cb (cdr reviews))))
                  (funcall callback callback)))))
      (forge--gtea-get repo (format "repos/:owner/:repo/pulls/%s/reviews" .number) nil
        :callback (lambda (value &rest _args)
                    (setf (alist-get 'reviews (car cur)) value)
                    (if load-notes
                        (funcall cb cb value)
                      (funcall callback callback)))))))

(cl-defmethod forge--fetch-issue-review-notes ((repo forge-gitea-repository) number cur callback)
  (let-alist (car cur)
    (if (string= .state "REQUEST_REVIEW") ; Review requests does not have a comments.
        (funcall callback callback)
      (forge--gtea-get repo (format "repos/:owner/:repo/pulls/%s/reviews/%s/comments" number .id)
        nil
        :callback (lambda (value &rest _args)
                    (setf (alist-get 'notes (car cur)) value)
                    (funcall callback callback))))))


;;;; Notifications:

(defun forge--gtea-massage-notification (data _forge githost callback)
  (let-alist data
    (let* ((number (and (string-match "[0-9]*\\'" .subject.url)
                        (string-to-number (match-string 0 .subject.url))))
           (type (pcase-exhaustive .subject.type
                                     ("Pull" 'pullreq)
                                     ("Issue" 'issue)))
           (url-part (if (eq type 'pullreq) "pulls" "issues"))
           (repo   (forge-get-repository
                    (list githost
                          .repository.owner.login
                          .repository.name)
                    nil 'create))
           (repoid (oref repo id))
           (id     (forge--object-id repoid .id))
           (cb (forge--gtea-fetch-topics-cb
                'pullreqs repo
                (lambda (_cb pulls)
                  (funcall
                   callback
                   (list
                    repo
                    (cadr pulls)
                    (forge-notification
                     :id           id
                     :thread-id    .id
                     :repository   repoid
                     :reason       (intern (downcase .subject.state))
                     :last-read    .last_read_at
                     :updated      .updated_at
                     :title        (concat "[" .subject.state "] - " .subject.title)
                     :type         type
                     :topic        number
                     :url          .subject.url)))))))
      (forge--gtea-get nil (format "repos/%s/%s/%s" .repository.full_name url-part number)
        nil
        :callback (lambda (pull &rest _args)
                    (funcall cb cb (list pull)))))))

(setq last-notifications nil)

(cl-defmethod forge--pull-notifications ((repo (subclass forge-gitea-repository)) githost &optional callback)
  (let ((spec (assoc githost forge-alist)))
    (unless spec
      (error "No entry for %S in forge-alist" githost))
    (forge--msg nil t nil "Pulling notifications")
    (let ((cb (lambda (cb data &optional n)
                (setq last-notifications (or last-notifications data))
                (if data
                    (forge--gtea-massage-notification
                     (car data) repo githost
                     (lambda (noti)
                       (pcase-let ((`(_ _ ,obj) noti))
                         (closql-with-transaction (forge-db)
                           (closql-insert (forge-db) obj t)
                           (forge--zap-repository-cache (forge-get-repository obj)))
                         (funcall cb cb (cdr data) (or n (length data))))))
                  (forge--gtea-put nil "notifications")
                  (forge--msg nil t t "Pulled %s notifications" (or n 0))
                  (when callback
                    (funcall callback callback))))))
      (funcall cb cb (forge--gtea-get nil "notifications")))))

;;; Mutations
(cl-defmethod forge--submit-create-pullreq ((_ forge-gitea-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (when (and .yaml (local-variable-p 'forge-buffer-draft-p))
      (user-error "Cannot use yaml frontmatter and set `%s' at the same time"
                  'forge-buffer-draft-p))
    (pcase-let* ((`(,base-remote . ,base-branch)
                  (magit-split-branch-name forge--buffer-base-branch))
                 (`(,head-remote . ,head-branch)
                  (magit-split-branch-name forge--buffer-head-branch))
                 (head-repo (forge-get-repository 'stub head-remote))
                 (url-mime-accept-string
                  ;; Support draft pull-requests.
                  "application/vnd.github.shadow-cat-preview+json"))
      (forge--gtea-post repo "repos/:owner/:repo/pulls"
        `((title . ,.title)
          (body . ,.body)
          (base . ,base-branch)
          (head  . ,(if (equal head-remote base-remote)
                        head-branch
                      (concat (oref head-repo owner) ":"
                              head-branch))))
        :callback  (forge--post-submit-callback)
        :errorback (forge--post-submit-errorback)))))

(cl-defmethod forge--submit-create-issue ((_ forge-gitea-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (forge--gtea-post repo "repos/:owner/:repo/issues"
      `((title       . , .title)
        (description . , .body))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-post ((_ forge-gitea-repository) topic)
  (forge--gtea-post topic "repos/:owner/:repo/issues/:number/comments"
    `((body . ,(string-trim (buffer-string))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-edit-post ((_ forge-gitea-repository) post)
  (forge--gtea-patch
    nil
    (cl-typecase post
      (forge-pullreq (forge--format-resource post "repos/:owner/:repo/pulls/:number"))
      (forge-issue   (forge--format-resource post "repos/:owner/:repo/issues/:number"))
      (forge-post    (forge--format-resource post "repos/:owner/:repo/issues/comments/:number")))
    (if (cl-typep post 'forge-topic)
        (let-alist (forge--topic-parse-buffer)
          `((title . , .title)
            (body  . , .body)))
      `((body . ,(string-trim (buffer-string)))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--delete-comment ((_repo forge-gitea-repository) post)
  (forge--gtea-delete post "repos/:owner/:repo/issues/comments/:number")
  (closql-delete post)
  (magit-refresh))

(cl-defmethod forge--set-topic-labels ((repo forge-gitea-repository) topic labels)
  (let ((callback (forge--set-field-callback)))
    (forge--fetch-labels
     repo (lambda (_cb data)
            (let ((ids (mapcan (lambda (label)
                                 (let-alist label
                                   (when (member .name labels)
                                     (list .id))))
                               (cdr data))))
              (forge--gtea-put topic "repos/:owner/:repo/issues/:number/labels"
                `((labels . ,ids))
                :callback callback))))))

(cl-defmethod forge--set-topic-field
  ((_repo forge-gitea-repository) topic field value)
  (forge--gtea-patch topic
    (cl-typecase topic
      (forge-pullreq "repos/:owner/:repo/pulls/:number")
      (forge-issue   "repos/:owner/:repo/issues/:number"))
    `((,field . ,value))
    :callback (forge--set-field-callback)))

(cl-defmethod forge--set-topic-milestone ((repo forge-gitea-repository) topic milestone)
  (forge--set-topic-field repo topic 'milestone (caar (forge-sql [:select [number]
                                                                          :from milestone
                                                                          :where (and (= repository $s1)
                                                                                      (= title $s2))]
                                                                 (oref repo id)
                                                                 milestone))))

(cl-defmethod forge--set-topic-title ((repo forge-gitea-repository) topic title)
  (forge--set-topic-field repo topic 'title title))

(cl-defmethod forge--set-topic-state ((repo forge-gitea-repository) topic value)
  (forge--set-topic-field repo topic 'state (cl-ecase value
                                              (closed "closed")
                                              (open   "open"))))

(cl-defmethod forge--set-topic-review-requests ((_repo forge-gitea-repository) topic reviewers)
  (let ((value (mapcar #'car (closql--iref topic 'review-requests))))
    (when-let ((add (cl-set-difference reviewers value :test #'equal)))
      (forge--gtea-post topic "repos/:owner/:repo/pulls/:number/requested_reviewers"
        `((reviewers . ,add))))
    (when-let ((remove (cl-set-difference value reviewers :test #'equal)))
      (forge--gtea-delete topic "repos/:owner/:repo/pulls/:number/requested_reviewers"
        `((reviewers . ,remove)))))
  ;; (oset topic review-requests (forge--gitea-map-logins reviewers))
  (forge-pull))

(cl-defmethod forge--set-topic-assignees ((repo forge-gitea-repository) topic assignees)
  (forge--set-topic-field repo topic 'assignees assignees))

(defun forge--gitea-map-logins (logins)
  (mapcan (lambda (user)
            (let ((id (cl-first user))
                  (login (cl-second user)))
              (when (cl-member login logins :test 'string=)
                (list id))))
          (oref (forge-get-repository nil) assignees)))

;;;; Templates:
(cl-defmethod forge--topic-templates ((repo forge-gitea-repository)
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

(cl-defmethod forge--topic-templates ((repo forge-gitea-repository)
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

;;;; Merge

(defun forge--pullreq-commit-msg (hash)
  (let* ((msg (magit-git-output "log" "--format=%B" "-n" "1" hash))
         (lines (string-lines msg))
         (txt (string-trim (string-join (cddr lines) "\n"))))
    txt))

(cl-defmethod forge--merge-pullreq ((_repo forge-gitea-repository)
                                    topic hash method)
  (let (merged)
    (cl-labels ((merge-pr (msg)
                  (forge--gtea-post topic "repos/:owner/:repo/pulls/:number/merge"
                    `((delete_branch_after_merge . t)
                      (MergeMessageField . ,msg)
                      (do . ,(symbol-name method))
                      ,@(and hash `((head_commit_id . ,hash)))))
                  (setq merged t)))
      (if (= 1 (car (magit-rev-diff-count (oref topic head-rev) (oref topic base-rev))))
          (let ((txt (forge--pullreq-commit-msg hash))
                (title (oref topic title)))
            (string-edit
             title
             txt
             (lambda (comment)
               (merge-pr comment)
               (exit-recursive-edit))
             :abort-callback (lambda ()
                               (exit-recursive-edit)
                               (error "Aborted edit")))
            (recursive-edit))
        (merge-pr ""))
      (unless merged
        (error "Merge aborted")))))

;;;; Approve

(defun forge-gitea-approve (pullreq &optional message)
  "Approve pullrequest.  Currently done only for gitea hosting."
  (interactive
   (list (forge-read-pullreq "Approve pull-request" t)))
  (let ((pullreq (forge-get-pullreq pullreq)))
    (unless message
      (setq message (read-string-from-buffer (oref pullreq title) "")))
    `((body . ,message)
      ;; (comments . ,[])
      (event . "APPROVE")
      (commit_id .
                 ,(magit-rev-hash
                   (concat "origin/"
                           (forge--pullreq-branch-internal pullreq)))))
    (forge--pullreq-branch-internal pullreq)
    (forge--gtea-post pullreq "repos/:owner/:repo/pulls/:number/reviews"
      `((body . ,message)
        ;; (comments . ,nil)
        (event . "APPROVED")
        (commit_id . ,(magit-rev-hash
                       (concat "origin/"
                               (forge--pullreq-branch-internal pullreq)))))))
  (forge-pull))

;;; Wrappers

(cl-defun forge--gtea-get* (obj resource
                                &optional params
                                &key callback while)
  "Unpaginate list of resources until WHILE condition or end of list
is met."
  (declare (indent defun))
  (forge--msg nil nil nil "Fetch page %s" 1)
  (let* ((result '())
         (url (if obj (forge--format-resource obj resource) resource))
         (cb (lambda (cb)
               (forge--gtea-get nil url params
                 :callback
                 (lambda (value headers status req)
                   (while (and value
                               (or (null while)
                                   (funcall while (car value))))
                     (setq result (cons (car value) result)
                           value (cdr value)))
                   (if-let* ((no-next-page (not value))
                             ;; Get next page:
                             (rels (cdr (assoc "Link" headers)))
                             (ok (string-match "<\\([^>]+\\)>; rel=\"next\"" rels))
                             (next-page (match-string 1 rels))
                             (ok (string-match "&page=\\([0-9]+\\)" next-page))
                             (page (match-string 1 next-page)))
                       (progn
                         ;; Set next page to params:
                         (setq params
	                       (if-let ((pair (assoc 'page params)))
	                           (progn (setcdr pair page) params)
	                         (cons `(page . ,page) params)))
                         (forge--msg nil nil nil "Fetch page %s" page)
                         (funcall cb cb))
                     (forge--msg nil nil t "Fetch pages")
                     (funcall callback result headers status req)))))))
    (funcall cb cb)))

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
  (gtea-patch (if obj (forge--format-resource obj resource) resource)
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
