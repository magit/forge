;;; forge-repo.el ---                             -*- lexical-binding: t -*-

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

;;; Class

(defclass forge-repository (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-class-suffix       :initform "-repository")
   (closql-table              :initform repository)
   (closql-primary-key        :initform id)
   (issues-url-format         :allocation :class)
   (issue-url-format          :allocation :class)
   (issue-post-url-format     :allocation :class)
   (pullreqs-url-format       :allocation :class)
   (pullreq-url-format        :allocation :class)
   (pullreq-post-url-format   :allocation :class)
   (commit-url-format         :allocation :class)
   (branch-url-format         :allocation :class)
   (remote-url-format         :allocation :class)
   (create-issue-url-format   :allocation :class)
   (create-pullreq-url-format :allocation :class)
   (pullreq-refspec           :allocation :class)
   (id                        :initarg :id)
   (forge                     :initarg :forge)
   (owner                     :initarg :owner)
   (name                      :initarg :name)
   (apihost                   :initarg :apihost)
   (githost                   :initarg :githost)
   (remote                    :initarg :remote)
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
   (forks           :closql-columns [parent id owner name])
   (issues          :closql-class   forge-issue)
   (labels          :closql-columns [repository id name color description])
   (pullreqs        :closql-class   forge-pullreq)
   (revnotes        :closql-class   forge-revnote)))

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
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/merge_requests/new")
   (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))

(cl-defmethod forge--object-id ((class (subclass forge-repository))
                                host owner name &optional id-type)
  "Return the id of the specified Github repository.
This method has to make an API request."
  (pcase-let ((`(,_githost ,apihost ,id ,_class)
               (or (assoc host forge-alist)
                   (error "No entry for %S in forge-alist" host))))
    (base64-encode-string
     (format
      "%s:%s" id
      (pcase (or id-type forge--object-id-type)
        ('forge
         (base64-decode-string
          (ghub-repository-id owner name
                              :host apihost
                              :auth 'forge
                              :forge (cl-ecase class
                                       (forge-github-repository    'ghub)
                                       (forge-gitlab-repository    'glab)
                                       (forge-gitea-repository     'gtea)
                                       (forge-bitbucket-repository 'buck)))))
        ('name
         (format "%s/%s" owner name))
        (_
         (error "Unknown forge--object-id-type: %s" forge--object-id-type)))))))

;;; Core

(cl-defmethod forge-get-repository ((demand symbol) &optional remote)
  "Return the forge repository for the current Git repository.

If DEMAND is nil and the forge repository for the current Git
repository cannot be determined or the corresponding object does
not exist in the forge database, then return nil.

If DEMAND is non-nil and the repositorty object does not exist
in the forge database yet, then create and return a new object.
If the required information cannot be determined, then raise an
error.  Unless DEMAND is `stub', this involves an API call and
results in the object being stored in the database.

If optional REMOTE is non-nil, then use that instead of trying
to guess the remote."
  (magit--with-refresh-cache
      (list default-directory 'forge-get-repository demand)
    (let* ((remotes (magit-list-remotes))
           (remote  (or remote
                        (magit-get "forge.remote")
                        (cond ((and (not (cdr remotes)) (car remotes)))
                              ((member "origin" remotes) "origin")))))
      (if-let ((url (or (magit-get "forge.url")
                        (and remote
                             (magit-git-string "remote" "get-url" remote)))))
          (forge-get-repository url remote demand)
        (when demand
          (error "Cannot determine forge repository.  %s"
                 (cond (remote  (format "No url configured for %s" remote))
                       (remotes "Cannot decide on remote to use")
                       (t       "No remote or explicit configuration"))))))))

(cl-defmethod forge-get-repository ((url string) &optional remote demand)
  "Return the repository at URL."
  (if (string-match forge--url-regexp url)
      (forge-get-repository (list (match-string 1 url)
                                  (match-string 3 url)
                                  (match-string 4 url))
                            remote demand)
    (when demand
      (error "Cannot determine forge repository.  %s isn't a forge url" url))))

(cl-defmethod forge-get-repository (((host owner name) list)
				    &optional remote demand)
  "Return the repository identified by HOST, OWNER and NAME."
  (if-let ((spec (assoc host forge-alist)))
      (pcase-let ((`(,githost ,apihost ,forge ,class) spec))
        (if-let ((row (car (forge-sql [:select * :from repository
                                       :where (and (= forge $s1)
                                                   (= owner $s2)
                                                   (= name  $s3))]
                                      forge owner name))))
            (let ((repo (closql--remake-instance class (forge-db) row)))
              (oset repo apihost apihost)
              (oset repo githost githost)
              (oset repo remote  remote)
              repo)
          (and demand
               (if-let ((id (forge--object-id class host owner name
                                              (and (eq demand 'stub) 'name))))
                   (let ((repo (funcall class
                                        :id      id
                                        :forge   forge
                                        :owner   owner
                                        :name    name
                                        :apihost apihost
                                        :githost githost
                                        :remote  remote)))
                     (unless (eq demand 'stub)
                       (closql-insert (forge-db) repo))
                     repo)
                 (error "Cannot determine forge repository.  %s"
                        "Cannot retrieve repository id")))))
    (when demand
      (error "Cannot determine forge repository.  No entry for %S in %s"
             host 'forge-alist))))

(cl-defmethod forge-get-repository ((repo forge-repository))
  repo)

;;; Utilities

(cl-defmethod forge--format-url ((repo forge-repository) slot &optional spec)
  (format-spec
   (eieio-oref-default repo slot)
   `(,@spec
     (?h . ,(oref repo githost))
     (?o . ,(oref repo owner))
     (?n . ,(oref repo name)))))

;;; _
(provide 'forge-repo)
;;; forge-repo.el ends here
