;;; forge-repo.el --- Repository support          -*- lexical-binding: t -*-

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

(require 'forge)
(require 'eieio)

;;; Classes

(defclass forge-repository (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-class-suffix       :initform "-repository")
   (closql-table              :initform repository)
   (closql-primary-key        :initform id)
   (issues-url-format         :initform nil :allocation :class)
   (issue-url-format          :initform nil :allocation :class)
   (issue-post-url-format     :initform nil :allocation :class)
   (pullreqs-url-format       :initform nil :allocation :class)
   (pullreq-url-format        :initform nil :allocation :class)
   (pullreq-post-url-format   :initform nil :allocation :class)
   (commit-url-format         :initform nil :allocation :class)
   (branch-url-format         :initform nil :allocation :class)
   (remote-url-format         :initform nil :allocation :class)
   (create-issue-url-format   :initform nil :allocation :class)
   (create-pullreq-url-format :initform nil :allocation :class)
   (pullreq-refspec           :initform nil :allocation :class)
   (id                        :initarg  :id)
   (forge-id                  :initarg  :forge-id)
   (forge                     :initarg  :forge)
   (owner                     :initarg  :owner)
   (name                      :initarg  :name)
   (apihost                   :initarg  :apihost)
   (githost                   :initarg  :githost)
   (remote                    :initarg  :remote)
   (sparse-p                  :initform t)
   (created)
   (updated)
   (pushed)
   (parent)
   (description)
   (homepage)
   (default-branch)
   (archived-p)
   (fork-p)
   (locked-p)
   (mirror-p)
   (private-p)
   (issues-p)
   (wiki-p)
   (stars)
   (watchers)
   (assignees       :closql-table assignee)
   (forks           :closql-table fork)
   (issues          :closql-class forge-issue)
   (labels          :closql-table label)
   (pullreqs        :closql-class forge-pullreq)
   (revnotes        :closql-class forge-revnote))
  :abstract t)

(defclass forge-unusedapi-repository (forge-repository) () :abstract t)

(defclass forge-noapi-repository (forge-repository) () :abstract t)

;;; Core

(cl-defmethod forge--repository-ids ((class (subclass forge-repository))
                                     host owner name &optional stub)
  "Return (OUR-ID . THEIR-ID) of the specified repository.
If optional STUB is non-nil, then the IDs are not guaranteed to
be unique.  Otherwise this method has to make an API request to
retrieve THEIR-ID, the repository's ID on the forge.  In that
case OUR-ID derives from THEIR-ID and is unique across all
forges and hosts.  "
  (pcase-let* ((`(,_githost ,apihost ,id ,_class)
                (or (assoc host forge-alist)
                    (error "No entry for %S in forge-alist" host)))
               (path (format "%s/%s" owner name))
               (their-id (and (not stub)
                              (ghub-repository-id
                               owner name
                               :host apihost
                               :auth 'forge
                               :forge (cl-ecase class
                                        (forge-github-repository    'ghub)
                                        (forge-gitlab-repository    'glab)
                                        (forge-gitea-repository     'gtea)
                                        (forge-gogs-repository      'gogs)
                                        (forge-bitbucket-repository 'buck))))))
    (cons (base64-encode-string
           (format "%s:%s" id
                   (cond (stub path)
                         ((eq class 'forge-github-repository)
                          (base64-decode-string their-id))
                         (t their-id)))
           t)
          (or their-id path))))

(cl-defmethod forge--repository-ids ((_class (subclass forge-noapi-repository))
                                     host owner name &optional _stub)
  (let ((their-id (if owner (concat owner "/" name) name)))
    (cons (base64-encode-string
           (format "%s:%s"
                   (nth 3 (or (assoc host forge-alist)
                              (error "No entry for %S in forge-alist" host)))
                   their-id)
           t)
          their-id)))

(defconst forge--signal-no-entry '(t stub create))

(cl-defmethod forge-get-repository ((demand symbol) &optional remote)
  "Return the forge repository for the current Git repository."
  (magit--with-refresh-cache
      (list default-directory 'forge-get-repository demand)
    (let* ((remotes (magit-list-remotes))
           (remote (or remote
                       (if (cdr remotes)
                           (car (member (or (magit-get "forge.remote") "origin")
                                        remotes))
                         (car remotes)))))
      (if-let ((url (and remote (magit-git-string "remote" "get-url" remote))))
          (forge-get-repository url remote demand)
        (when (memq demand forge--signal-no-entry)
          (error "Cannot determine forge repository.  %s"
                 (cond (remote  (format "No url configured for %S" remote))
                       (remotes "Cannot decide on remote to use")
                       (t       "No remote configured"))))))))

(cl-defmethod forge-get-repository ((url string) &optional remote demand)
  "Return the repository at URL."
  (if-let ((parts (forge--split-url url)))
      (forge-get-repository parts remote demand)
    (when (memq demand forge--signal-no-entry)
      (error "Cannot determine forge repository.  %s isn't a forge url" url))))

(cl-defmethod forge-get-repository (((host owner name) list)
				    &optional remote demand)
  "Return the repository identified by HOST, OWNER and NAME."
  (if-let ((spec (assoc host forge-alist)))
      (pcase-let ((`(,githost ,apihost ,forge ,class) spec))
        (let* ((row (car (forge-sql [:select * :from repository
                                     :where (and (= forge $s1)
                                                 (= owner $s2)
                                                 (= name  $s3))]
                                    forge owner name)))
               (obj (and row (closql--remake-instance class (forge-db) row))))
          (when obj
            (oset obj apihost apihost)
            (oset obj githost githost)
            (oset obj remote  remote))
          (cond ((and (eq demand t)
                      (or (not obj)
                          (oref obj sparse-p)))
                 (error "Cannot use `%s' in %S yet.\n%s"
                        this-command (magit-toplevel)
                        "Use `M-x forge-pull' before trying again."))
                ((and (eq demand 'full) obj
                      (oref obj sparse-p))
                 (setq obj nil)))
          (when (and (memq demand '(stub create))
                     (not obj))
            (pcase-let ((`(,id . ,forge-id)
                         (forge--repository-ids class host owner name
                                                (eq demand 'stub))))
              (setq obj (funcall class
                                 :id       id
                                 :forge-id forge-id
                                 :forge    forge
                                 :owner    owner
                                 :name     name
                                 :apihost  apihost
                                 :githost  githost
                                 :remote   remote)))
            (when (eq demand 'create)
              (closql-insert (forge-db) obj)))
          obj))
    (when (memq demand forge--signal-no-entry)
      (error "Cannot determine forge repository.  No entry for %S in %s"
             host 'forge-alist))))

(cl-defmethod forge-get-repository ((repo forge-repository))
  repo)

;;; Utilities

(cl-defmethod forge--topics-until ((repo forge-repository) table)
  (and (not (oref repo sparse-p))
       (caar (forge-sql [:select [updated] :from $s1
                         :where (= repository $s2)
		         :order-by [(desc updated)]
                         :limit 1]
                        table (oref repo id)))))

(cl-defmethod forge--format-url ((repo forge-repository) slot &optional spec)
  (format-spec
   (eieio-oref repo slot)
   (with-slots (githost owner name) repo
     (let ((path (if owner (concat owner "/" name) name)))
       `(,@spec
         (?h . ,githost)
         (?o . ,owner)
         (?n . ,name)
         (?p . ,path)
         (?P . ,(replace-regexp-in-string "/" "%2F" path)))))))

(defun forge--set-field-callback ()
  (let ((buf (current-buffer)))
    (lambda (&rest _)
      (with-current-buffer
          (or buf (current-buffer))
        (forge-pull)))))

(defvar forge--mode-line-buffer nil)

(defun forge--msg (repo echo done format &rest args)
  (let ((msg (apply #'format format args)))
    (when repo
      (setq msg (replace-regexp-in-string
                 "REPO"
                 (concat (oref repo owner) "/" (oref repo name))
                 msg t)))
    (when (and echo msg)
      (message "%s%s" msg (if done "...done" "...")))
    (when (buffer-live-p forge--mode-line-buffer)
      (with-current-buffer forge--mode-line-buffer
        (setq mode-line-process
              (concat " " (propertize msg 'face 'magit-mode-line-process))))
      (force-mode-line-update t))))

;;; _
(provide 'forge-repo)
;;; forge-repo.el ends here
