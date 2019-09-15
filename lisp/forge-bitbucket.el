;;; forge-bitbucket.el --- Bitbucket support      -*- lexical-binding: t -*-

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

(require 'buck)
(require 'forge)
(require 'seq)

;;; Class

(defclass forge-bitbucket-repository (forge-noapi-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   ;; The anchor for the issue itself is .../%i#issue-%i
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#comment-%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/pull-requests")
   (pullreq-url-format        :initform "https://%h/%o/%n/pull-requests/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/pull-requests/%i#comment-%I")
   (commit-url-format         :initform "https://%h/%o/%n/commits/%r")
   (branch-url-format         :initform "https://%h/%o/%n/branch/%r")
   (remote-url-format         :initform "https://%h/%o/%n/src")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/pull-requests/new")))

;;; Pull
;;;; Repository

(cl-defmethod forge--pull ((repo forge-bitbucket-repository) until)
  "Pull REPO data no older than UNTIL."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (let ((cb (let ((buf (and (derived-mode-p 'magit-mode)
                            (current-buffer)))
                  (dir default-directory)
                  (val nil))
              (lambda (cb &optional v)
                (when v (if val (push v val) (setq val v)))
                (cond
                 ((not val)
                  (forge--fetch-repository repo cb)
                  (when (magit-get-boolean "forge.omitExpensive")
                    (setq val (nconc `((assignees) (forks) (labels)) val)))
                  )
                 ((not (assq 'issues    val)) (forge--fetch-issues     repo cb until))
                 (t
                  (forge--msg repo t t   "Pulling REPO")
                  (forge--msg repo t nil "Extracting assignees from REPO")
                  (let (assignees)
                    (dolist (issue (alist-get 'issues val))
                      (setq assignees
                            (seq-uniq (append (forge--bitbucket-issue-users issue)
                                              assignees))))
                    (setq val (cons (cons 'assignees assignees) val)))
                  (forge--msg repo t t   "Extracting assignees from REPO")
                  (forge--msg repo t nil "Storing REPO")
                  (emacsql-with-transaction (forge-db)
                    (let-alist val
                      (forge--update-repository repo val)
                      (forge--update-assignees  repo .assignees)
                      (dolist (v .issues) (forge--update-issue repo v))
                      )
                    (oset repo sparse-p nil))
                  (forge--msg repo t t "Storing REPO")
                  (forge--git-fetch buf dir repo)))))))
    (funcall cb cb)))

(defun forge--bitbucket-issue-users (issue)
  "Return a list of unique users associated with ISSUE."
  (let (users)
    (let-alist issue
      (when .reporter (push (forge--bitbucket-make-assignee .reporter) users))
      (when .assignee (push (forge--bitbucket-make-assignee .assignee) users))
      (when .comments
        (dolist (comment .comments)
          (let-alist comment
            (when .user (push (forge--bitbucket-make-assignee .user) users))))))
    (seq-uniq users (lambda (u1 u2)
                      (string= (cdr (assq 'uuid u1))
                               (cdr (assq 'uuid u2)))))))

(cl-defmethod forge--fetch-repository ((repo forge-bitbucket-repository) callback)
  "Fetch basic data for REPO.
Return data through CALLBACK."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (forge--buck-get repo "/repositories/:project" nil
    ;; Use the fields query to exclude unused data from the response
    ;; https://developer.atlassian.com/bitbucket/api/2/reference/meta/partial-response
    :query '((fields . "-links,-scm,-language,-mainbranch.type,-owner.links"))
    :callback (lambda (value _headers _status _req)
                (funcall callback callback value))))

(cl-defmethod forge--update-repository ((repo forge-bitbucket-repository) data)
  "Store forge DATA in REPO."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (let-alist data
    (oset repo created        (forge--bitbucket-hack-iso8601 .created_on))
    (oset repo updated        (forge--bitbucket-hack-iso8601 .updated_on))
    (oset repo pushed         nil)
    (oset repo parent         nil)
    (oset repo description    .description)
    (oset repo homepage       .website)
    (oset repo default-branch .mainbranch.name)
    (oset repo archived-p     nil)
    (oset repo fork-p         nil)
    (oset repo locked-p       nil)
    (oset repo mirror-p       nil)
    (oset repo private-p      .is_private)
    (oset repo issues-p       .has_issues)
    (oset repo wiki-p         .has_wiki)
    (oset repo stars          nil)
    (oset repo watchers       nil)))

;;;; Issues

(cl-defmethod forge--fetch-issues ((repo forge-bitbucket-repository) callback until)
  "Fetch issues for a bitbucket REPO."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (let ((query '((sort . "updated_on")
                 (fields . "-values.repository.links,-values.reporter.links,-values.assignee.links,+values.links.comments.*")
                 ))
        (cb (let (val cur cnt pos)
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
    (when until
      (push (cons 'q (concat "updated_on>" (forge--topics-until repo until 'issue)))
            query))
    (forge--msg repo t nil "Pulling REPO issues")
    (forge--buck-get repo "/repositories/:project/issues" nil
      :query query
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (funcall cb cb value)))))

(cl-defmethod forge--fetch-issue-posts ((repo forge-bitbucket-repository) cur cb)
  "Fetch comments for REPO issue CUR.
Callback function CB should accept itself as argument."
  ;; checkdoc-params: (forge-bitbucket-repository)
  ;;(message "%s" (pp-to-string (car cur)))
  (let-alist (car cur)
    (forge--buck-get repo
      (format "/repositories/%s/issues/%s/comments" .repository.full_name .id)
      nil
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'comments (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-issue ((repo forge-bitbucket-repository) data)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let* ((issue-id (forge--object-id 'forge-issue repo .id))
             (issue
              (forge-issue
               :id           issue-id
               :repository   (oref repo id)
               :number       .id
               :state        (pcase-exhaustive .state
                               ("new" 'open)
                               ("open" 'open)
                               ("resolved" 'closed)
                               ("on hold" 'open)
                               ("invalid" 'closed)
                               ("duplicate" 'closed)
                               ("wontfix" 'closed)
                               ("closed" 'closed))
               :author       .reporter.nickname
               :title        .title
               :created      (forge--bitbucket-hack-iso8601 .created_on)
               :updated      (forge--bitbucket-hack-iso8601 .updated_on)
               ;; Bitbucket does not list when the issue was closed.
               ;; Just set it to 1 if it is in one of the closed
               ;; states, so that this slot at least can serve as a
               ;; boolean.
               :closed       (and (member .state
                                          '("resolved" "invalid" "duplicate" "wontfix" "closed"))
                                  1)
               :locked-p     nil
               :milestone    .milestone.name
               :body         (forge--sanitize-string .content.raw))))
        (closql-insert (forge-db) issue t)
        (when .assignee
          (forge--set-id-slot repo issue 'assignees
                              (list (forge--bitbucket-make-assignee .assignee))))
        (dolist (c .comments)
          (let-alist c
            (let ((post
                   (forge-issue-post
                    :id      (forge--object-id issue-id .id)
                    :issue   issue-id
                    :number  .id
                    :author  .user.nickname
                    :created (forge--bitbucket-hack-iso8601 .created_on)
                    :updated (forge--bitbucket-hack-iso8601 .updated_on)
                    :body    (forge--sanitize-string .content.raw))))
              (closql-insert (forge-db) post t))))))))

;;; Assignees

(defun forge--bitbucket-make-assignee (assignee)
  "Create an assigne alist from a bitbucket ASSIGNEE json alist."
  (let-alist assignee
    (list (cons 'id .uuid)
          (cons 'name .display_name)
          (cons 'username .nickname))))

(cl-defmethod forge--update-assignees ((repo forge-bitbucket-repository) data)
  "Store assignee DATA in REPO."
  ;; checkdoc-params: (forge-bitbucket-repository)
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

;;; Mutations

(cl-defmethod forge--submit-create-issue ((_ forge-bitbucket-repository) repo)
  ;; checkdoc-params: (forge-bitbucket-repository)
  (let-alist (forge--topic-parse-buffer)
    (forge--buck-post repo "/repositories/:project/issues/"
      nil
      :payload `((title    . , .title)
                 (priority . "major") ;TODO set other priorities in buffer
                 (kind     . "bug") ;TODO set other kinds in buffer
                 (content  . ((raw    . , .body)
                              (markup . "markdown"))) ; TODO enable other markups
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback)))))

(cl-defmethod forge--submit-create-post ((_repo forge-bitbucket-repository) topic)
  (forge--buck-post topic
                    (if (forge-issue-p topic)
                        "/repositories/:project/issues/:number/comments/"
                      "/repositories/:project/pullrequests/:number/comments/")
                    nil
                    :payload `((content . ((raw . ,(string-trim (buffer-string))))))
                    :callback  (forge--post-submit-callback)
                    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-edit-post ((_ forge-bitbucket-repository) post)
  "Edit a topic or POST."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (forge--buck-put post
    (cl-etypecase post
      (forge-pullreq "/repositories/:project/pullrequests/:number")
      (forge-issue   "/repositories/:project/issues/:number")
      (forge-pullreq-post "/repositories/:project/pullrequests/:topic/comments/:number")
      (forge-issue-post "/repositories/:project/issues/:topic/comments/:number"))
    (if (cl-typep post 'forge-topic)
        (let-alist (forge--topic-parse-buffer)
          `((title . , .title)
            (content . ((raw . , .body)))))
      `((content . ((raw . ,(string-trim (buffer-string)))))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(defun forge--put-topic-json (topic json)
  "TOPIC JSON."
  (forge--buck-put topic
    (cl-typecase topic
      (forge-pullreq "/repositories/:project/pullrequests/:number") ; TODO: Not sure this works
      (forge-issue   "/repositories/:project/issues/:number"))
    json
    :callback (forge--set-field-callback))) ; TODO: The callback does not have to pull everything

(cl-defmethod forge--set-topic-field
  ((_repo forge-bitbucket-repository) topic field value)
  "Set a TOPIC FIELD to VALUE."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (forge--put-topic-json topic `((,field . ,value))))

(cl-defmethod forge--set-topic-title
  ((repo forge-bitbucket-repository) topic title)
  "Set the bitbucket REPO TOPIC TITLE."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (forge--set-topic-field repo topic 'title title))

(cl-defmethod forge--set-topic-state
  ((repo forge-bitbucket-repository) topic)
  "Change the state of bitbucket REPO TOPIC."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (forge--set-topic-field repo topic 'state
                          (cl-ecase (oref topic state) ; TODO: Handle bitbucket states better
                            (closed "open")
                            (open   "closed"))))

(cl-defmethod forge--set-topic-labels
  ((_repo forge-bitbucket-repository) _topic _labels)
  "Bitbucket does not support labels."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (user-error "Bitbucket does not support labels"))

(cl-defmethod forge--set-topic-assignees
  ((repo forge-bitbucket-repository) topic assignees)
  "Assign a bitbucket REPO TOPIC to ASSIGNEES."
  ;; checkdoc-params: (forge-bitbucket-repository)
  (let* ((users (mapcar #'cdr (oref repo assignees)))
         (user (caddr (assoc (car assignees) users))))
    (forge--put-topic-json topic `((assignee . ((username . ,(car assignees))))))))

(cl-defmethod forge--topic-templates ((_repo forge-bitbucket-repository)
                                      (_topic (subclass forge-issue)))
  "Bitbucket does not support issue templates."
  ;; checkdoc-params: (forge-bitbucket-repository subclass forge-issue)
  nil)

(cl-defmethod forge--topic-templates ((repo forge-bitbucket-repository)
                                      (_topic (subclass forge-pullreq)))
  "Return PULL_REQUEST_TEMPLATE.md files as `refined-bitbake'.
bitbake.org does not support pull request templates, but the
`refined-bitbake' browser extension (
https://github.com/refined-bitbucket/refined-bitbucket) does;
 /PULL_REQUEST_TEMPLATE.md
 /docs/PULL_REQUEST_TEMPLATE.md
 /.github/PULL_REQUEST_TEMPLATE.md
 /.bitbucket/PULL_REQUEST_TEMPLATE.md
Return the same list of files, if present in REPO's default branch."
  ;; checkdoc-params: (forge-bitbucket-repository subclass forge-pullreq)
  (list
   (car
    (sort
     (cl-remove-if-not
      (apply-partially #'string-match-p
                       "\\`\\(\\(docs\\|.github\\|.bitbucket\\)/\\)?PULL_REQUEST_TEMPLATE.md\\'")
      (magit-revision-files (oref repo default-branch)))
     (lambda (a b)
       ;; by ridiculous coincidence the shortest filename is the best match.
       (< (length a) (length b)))))))

;;; Utilities

(defun forge--bitbucket-hack-iso8601 (time)
  "Return TIME - an ISO8601 timestamp - with no more than three second decimals."
  (and time
    (if (string-match "T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]\\{3\\}\\([0-9]+\\)" time)
        (replace-match "" nil t time 1)
      time)))

(cl-defun forge--buck-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               username host
                               callback errorback extra)
  "Perform a GET request to the bibucket API.
OBJ is any forge struct, used to transform RESOURCE."
  (declare (indent defun))
  (buck-get (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :username username
            :callback callback
            :errorback (or errorback (and callback t))
            :extra extra))

(cl-defun forge--buck-put (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host callback errorback)
  "Perform a PUT request to the bitbucket API.
OBJ is any forge struct, used to transform RESOURCE.  The API
call is done using the `buck' API of ghub.el.  PARAMS, QUERY,
PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER, HOST,
CALLBACK, and ERRORBACK is passed unmodified to `buck-put'.
If HOST is nil, use the `apihost' of the OBJ repository.
RESOURCE is not transformed if OBJ is nil.  If ERRORBACK is nil,
use CALLBACK for errors too."
  (declare (indent defun))
  (buck-put (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback
            :errorback (or errorback (and callback t))))

(cl-defun forge--buck-post (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                username host
                                callback errorback extra)
  "Perform a POST request to the bitbucket API.
OBJ is any forge struct, used to transform RESOURCE.  The API
call is done using the `buck' API of ghub.el.  PARAMS,
QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, HOST, CALLBACK, ERRORCALLBACK, and EXTRA is passed
unmodified to `buck-post'.
If HOST is nil, use the `apihost' of the OBJ repository.
RESOURCE is not transformed if OBJ is nil.
If ERRORBACK is nil, use CALLBACK for errors too."
  (declare (indent defun))
  (setq host (or host (oref (forge-get-repository obj) apihost)))
  (buck-post (if obj (forge--format-resource obj resource) resource)
             params
             :host host
             :auth 'forge
             :query query :payload payload :headers headers
             :silent silent :unpaginate unpaginate
             :noerror noerror :reader reader
             :username username
             :callback callback
             :errorback (or errorback (and callback t))
             :extra extra))

(cl-defun forge--buck-delete (obj resource
                                  &optional params
                                  &key query payload headers
                                  silent unpaginate noerror reader
                                  host callback errorback)
  (declare (indent defun))
  (buck-delete (if obj (forge--format-resource obj resource) resource)
               params
               :host (or host (oref (forge-get-repository obj) apihost))
               :auth 'forge
               :query query :payload payload :headers headers
               :silent silent :unpaginate unpaginate
               :noerror noerror :reader reader
               :callback callback
               :errorback (or errorback (and callback t))))

;;; _
(provide 'forge-bitbucket)
;;; forge-bitbucket.el ends here
