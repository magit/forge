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
                 (t
                  (forge--msg repo t t   "Pulling REPO")
                  (forge--msg repo t nil "Storing REPO")
                  (emacsql-with-transaction (forge-db)
                    (let-alist val
                      (forge--update-repository repo val)
                      )
                    (oset repo sparse-p nil))
                  (forge--msg repo t t "Storing REPO")
                  (forge--git-fetch buf dir repo)))))))
    (funcall cb cb)))

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
