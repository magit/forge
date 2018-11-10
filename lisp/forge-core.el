;;; forge-core.el --- Core functionality           -*- lexical-binding: t -*-

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

(require 'magit)
(require 'magit-popup)
(require 'eieio)

(require 'forge-db)

;;; Options

(defgroup forge nil
  "Options concerning Git forges."
  :group 'magit)

(defgroup forge-faces nil
  "Faces concerning Git forges."
  :group 'forge
  :group 'magit-faces)

(defcustom forge-alist
  '(("github.com" "api.github.com"
     "github.com" forge-github-repository)
    ("gitlab.com" "gitlab.com/api/v4"
     "gitlab.com" forge-gitlab-repository)
    ("bitbucket.org" "api.bitbucket.org/2.0"
     "bitbucket.org" forge-bitbucket-repository)
    ("code.orgmode.org" "code.orgmode.org/api/v1"
     "code.orgmode.org" forge-gogs-repository)
    ("gitea.local" "localhost:3000/api/v1"
     "gitea.local" forge-gitea-repository))
  "List of Git forges.

Each entry has the form (GITHOST APIHOST ID CLASS).

GITHOST is matched against the host part of Git remote urls
  using `forge--url-regexp' to identify the forge.
APIHOST is the api endpoint of the forge's api.
ID is used to identify the forge in the local database.
CLASS is the class to be used for repository from the forge.

GITHOST and APIHOST can be changed, but ID and CLASS are final.
If you change ID, then the identity of every repository from
that forge changes.  If you change CLASS, then things start
falling apart.

There can be multiple elements that only differ in GITHOST.
Among those, the canonical element should come first.  Any
elements that have the same APIHOST must also have the same
ID, and vice-versa."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type '(repeat (list (string :tag "Git host")
                       (string :tag "ID")
                       (string :tag "API endpoint")
                       (symbol :tag "Repository class"))))

;;; Core

(defclass forge-object (closql-object) () :abstract t)

(cl-defgeneric forge-get-parent (object)
  "Return the parent object of OBJECT.
The hierarchy is repository > topic > post.
For other objects return nil.")

(cl-defgeneric forge-get-repository ()
  "Return a forge repository object.")

(cl-defgeneric forge-get-topic ()
  "Return a forge issue or pullreq object.")

(cl-defgeneric forge-get-issue ()
  "Return a forge issue object.")

(cl-defgeneric forge-get-pullreq ()
  "Return a forge pullreq object.")

(cl-defgeneric forge--object-id (class &rest args)
  "Return the database id for the CLASS object specified by ARGS.")

(defvar forge--object-id-type 'forge)

;;; Utilities

(defun forge--set-id-slot (repo object slot rows)
  (let ((repo-id (oref repo id)))
    (closql-oset
     object slot
     (mapcar (lambda (alist)
               (forge--object-id repo-id (cdr (assq 'id alist))))
             rows))))

(cl-defgeneric forge--format-url (object slot &optional spec))

(cl-defmethod forge--format-url ((remote string) slot &optional spec)
  (if-let ((parts (forge--split-remote-url remote)))
      (forge--format-url (forge-get-repository 'stub remote) slot
                         `(,@spec
                           (?h . ,(nth 0 parts))
                           (?o . ,(nth 1 parts))
                           (?n . ,(nth 2 parts))))
    (user-error "Cannot browse non-forge remote %s" remote)))

(defconst forge--url-regexp "\
\\`\\(?:git://\\|git@\\|ssh://git@\\|https://\\)\
\\(.*?\\)[/:]\
\\(\\([^:/]+\\)/\\([^/]+?\\)\\)\
\\(?:\\.git\\)?\\'")

(defun forge--split-remote-url (remote)
  (when-let ((url (magit-git-string "remote" "get-url" remote)))
    (and (string-match forge--url-regexp url)
         (list (match-string 1 url)
               (match-string 3 url)
               (match-string 4 url)))))

(defun forge--url-p (url)
  (save-match-data
    (and (string-match forge--url-regexp url)
         (cl-caddr (assoc (match-string 1 url) forge-alist)))))

(defun forge--forge-remote-p (remote)
  (when-let ((url (magit-git-string "remote" "get-url" remote)))
    (forge--url-p url)))

(defun forge--url-equal (urlA urlB)
  (or (equal urlA urlB)
      (save-match-data
        (let (hostA repoA hostB repoB)
          (and (when (string-match forge--url-regexp urlA)
                 (setq hostA (match-string 1 urlA))
                 (setq repoA (match-string 2 urlA)))
               (when (string-match forge--url-regexp urlB)
                 (setq hostB (match-string 1 urlB))
                 (setq repoB (match-string 2 urlB)))
               (equal repoA repoB)
               (equal (cl-caddr (assoc hostA forge-alist))
                      (cl-caddr (assoc hostB forge-alist))))))))

(cl-defmethod forge--format-resource ((object forge-object) resource)
  (save-match-data
    (setq resource
          (replace-regexp-in-string
           ":\\([^/]+\\)"
           (lambda (str)
             (let ((slot (intern (substring str 1))))
               (or (ignore-errors
                     (format "%s" (eieio-oref object
                                              (if (eq slot 'repo) 'name slot))))
                   str)))
           resource t t))
    (if (string-match ":[^/]*" resource)
        (if-let ((parent (forge-get-parent object)))
            (forge--format-resource parent resource)
          (error "Cannot resolve %s for a %s"
                 (match-string 0 resource)
                 (eieio-object-class object)))
      resource)))

;;; _
(provide 'forge-core)
;;; forge-core.el ends here
