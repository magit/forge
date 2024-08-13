;;; forge-core.el --- Core functionality  -*- lexical-binding:t -*-

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

(require 'magit)

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'eieio)
(require 'seq)
(require 'subr-x)

(require 'transient)

(require 'forge-db)

(eval-when-compile
  (cl-pushnew 'id       eieio--known-slot-names)
  (cl-pushnew 'name     eieio--known-slot-names)
  (cl-pushnew 'number   eieio--known-slot-names)
  (cl-pushnew 'owner    eieio--known-slot-names)
  (cl-pushnew 'worktree eieio--known-slot-names))

;;; Options

(defgroup forge nil
  "Options concerning Git forges."
  :group 'magit)

(defgroup forge-faces nil
  "Faces concerning Git forges."
  :group 'forge
  :group 'magit-faces)

(defcustom forge-alist
  '(;; Forges
    ("github.com" "api.github.com"
     "github.com" forge-github-repository)
    ("gitlab.com" "gitlab.com/api/v4"
     "gitlab.com" forge-gitlab-repository)
    ("salsa.debian.org" "salsa.debian.org/api/v4"
     "salsa.debian.org" forge-gitlab-repository)
    ("framagit.org" "framagit.org/api/v4"
     "framagit.org" forge-gitlab-repository)
    ("gitlab.gnome.org" "gitlab.gnome.org/api/v4"
     "gitlab.gnome.org" forge-gitlab-repository)
    ;; Forges (API unsupported)
    ("codeberg.org" "codeberg.org/api/v1"
     "codeberg.org" forge-gitea-repository)
    ("bitbucket.org" "api.bitbucket.org/2.0"
     "bitbucket.org" forge-bitbucket-repository)
    ;; Semi-Forges
    ("git.savannah.gnu.org" nil
     "git.savannah.gnu.org" forge-cgit**-repository)
    ("git.kernel.org" nil
     "git.kernel.org" forge-cgit-repository)
    ("repo.or.cz" nil
     "repo.or.cz" forge-repoorcz-repository)
    ("git.suckless.org" nil
     "git.suckless.org" forge-stagit-repository)
    ("git.sr.ht" nil
     "git.sr.ht" forge-srht-repository))
  "List of Git forges.

Each entry has the form (GITHOST APIHOST WEBHOST CLASS).

- GITHOST is the host used to access repositories on the forge using
  Git.

- APIHOST is the host used to access the forge's API.  For some forges
  the isn't just a host, but a host followed by the path to the API's
  endpoint.

- WEBHOST is the host used to access repositories on this forge using
  a browser.  The IDs used to identify repositories from the forge in
  the local database also derives from this value.

- CLASS is the class to be used for repositories from the forge.

Complications:

- When connecting to a Github Enterprise edition whose REST API's
  end point is \"<host>/v3\" and whose GraphQL API's end point is
  \"<host>/graphql\", then use \"<host>/v3\" as APIHOST.  This is a
  historic accident.  See issue #174.

- WEBHOST and CLASS cannot be changed once you have added one or
  more repositories from a forge.  Changing GITHOST and/or APIHOST
  may be possible, but should seldom be necessary."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type '(repeat (list (string :tag "Git host")
                       (choice (string :tag "API endpoint")
                               (const  :tag "No API" nil))
                       (string :tag "ID")
                       (symbol :tag "Repository class"))))

;;; Class

(defclass forge-object (closql-object) () :abstract t)

(defmacro forge--childp (obj type)
  "Somewhat similar to `cl-typep' but only for (possibly unknown) classes.
TYPE is evaluated at macro-expansion time but, unlike with
`cl-typep', the respective class does not have to be defined
at that time."
  (let ((fn (intern (concat (symbol-name (eval type)) "--eieio-childp"))))
    `(and (fboundp ',fn) (,fn ,obj))))

;;; Query

(cl-defgeneric forge-get-parent (object)
  "Return the parent object of OBJECT.
The hierarchy is repository > topic > post.
For other objects return nil.")

(cl-defgeneric forge-get-repository (demand)
  "Return a forge repository object or nil, or signal an error.

A forge repository is a repository hosted on a forge.  The local clone
is also a \"repository\", but it is a \"Git\" repository, not a \"Forge\"
repository.  (Forge repositories are also Git repositories, but not the
other way around.)

A `:known' repository has an entry in the local database.  All other
repositories are unknown.  `:known' repositories are divided into two
subgroups: `:tracked' and \"untracked\" repositories.

A `:tracked' repository was previously explicitly added to the database
by the user.

When Forge encounters a repository, without being instructed by the user
to track it, it may nevertheless add limited information about it to the
database.  Such a repository is `:known' but it is not `:tracked'.

Other repositories are \"unknown\".  Most commands can only deal with
repositories that are stored in the database.  Of these, some can deal
with any `:known' repositories, others require that they are `:tracked'.

Some other commands exist — such as the browse commands — that have no
such requirement.  While such commands also require a repository object,
they do not care whether that is stored in the database.  Instead they
are happy to use a `:stub' repository; a repository that is not stored
in the database.

The DEMAND argument specifies what kind of repository object the caller
requires, at least.  `:tracked' is greater than `:known', which is
greater than `:stub'.  For example, if the caller requests a `:known'
repository, a `:tracked' repository will do, while a `:stub' repository
will not.

The valid values for DEMAND are:

- `:tracked' and `:tracked?' request a repository that the user added
  to the database.  If there is no such repository, the former causes
  an error to be signaled, while for the latter nil is returned.

- `:known?' and `:insert!' request a repository from the database.
  Whether the user explicitly added it does not matter.  If there is no
  such repository, nil is returned for the former, while for the latter
  a new repository is inserted into the repository and then returned.

- `:stub' and `:stub?' request the Forge repository corresponding to
  the current Git repository.  It does not matter whether it is known.
  This fails if `default-directory' is not inside a Git repository, if
  there is no matching entry in `forge-alist', or if it is unclear which
  remote to use.  If the repository cannot be determined, the former
  causes an error to be signaled, while for the latter nil is returned.

  Stub repository objects are created without making an API request, so
  we lack access to the upstream ID, which the IDs used in out database,
  derive from.  Stub repositories are \"unknown\" in the sense that their
  IDs are not `:known'.  This is done to allow offline operations.

- `:valid?' requests the Forge repository corresponding to the current
  Git repository.  It does not matter whether it is known.  If it is
  unknown, an API request is made to verify that the repository exists
  on the forge.  If it does, an object with a valid upstream ID is
  returned, but that isn't inserted into the database.  If not, nil is
  returned.

Given a repository object, you can query its `condition' slot to learn
whether it is `:tracked', `:known' (i.e., has a valid ID and is stored
in the database), or merely a `:stub'.

You can also use (forge-get repository OBJECT nil DEMAND) to check the
condition of a repository object, or even to ensure a repository object
has a valid upstream ID (using `:valid?'), or that it is tracked in the
database (using `:insert!').

Use `forge-repository-equal' to check if two objects refer to the same
repository.

Also see info node `(forge) Repository Detection'.")

(cl-defgeneric forge-get-topic ()
  "Return a forge issue or pullreq object.")

(cl-defgeneric forge-get-issue ()
  "Return a forge issue object.")

(cl-defgeneric forge-get-pullreq ()
  "Return a forge pullreq object.")

(defun forge--get-forge-host (host &optional demand)
  "Return `forge-alist' entry matching HOST.

Entries have the form (GITHOST APIHOST WEBHOST CLASS).

- If HOST matches a GITHOST, return the corresponding entry.
- Else, if HOST is an ssh alias and the canonical hostname matches a
  GITHOST, return the corresponding entry.
- Finally, if HOST matches a WEBHOST, return the corresponding entry.

If no entry matches, return nil, or signal an error if optional DEMAND
is non-nil."
  (or (assoc host forge-alist)
      (assoc (seq-some (lambda (line)
                         (and (string-prefix-p "hostname" line)
                              (substring line 9)))
                       (ignore-errors
                         (process-lines-ignore-status "ssh" "-G" host)))
             forge-alist)
      (car (cl-member host forge-alist :test #'equal :key #'caddr))
      (and demand
           (error "No entry for \"%s\" in `forge-alist'" host))))

(defun forge--split-forge-url (url &optional relax)
  (save-match-data
    (cond
     ((string-match
       (concat "\\`"
               "\\(?:git://\\|"
               "[^/@]+@\\|"
               "\\(?:ssh\\|ssh\\+git\\|git\\+ssh\\)://\\(?:[^/@]+@\\)?\\|"
               "https?://\\(?:[^/@]+@\\)?\\)?"
               (if relax
                   "\\(?1:[^:/]+\\)"
                 (regexp-opt (mapcar #'car forge-alist) t))
               "\\(?::[0-9]+\\)?"
               "\\(?:/\\|:/?\\)"
               "~?\\(?2:.+?\\)/"
               "\\(?3:[^/]+?\\)"
               "\\(?:\\.git\\|/\\)?"
               "\\'")
       url)
      (and-let* ((elt (forge--get-forge-host (match-string 1 url) (not relax))))
        ;; Return the WEBHOST (not the GITHOST, URLs passed to this
        ;; function usually contain a GITHOST) because the IDs used to
        ;; identify a repository in the database are based on WEBHOSTs.
        (list (caddr elt)
              (match-string 2 url)
              (match-string 3 url))))
     ((not relax)
      ;; The host part didn't match any GITHOST in `forge-alist', but it
      ;; might be a ssh host alias.  We have to relax strictness; in the
      ;; extremely unlikely case that there is a common path between the
      ;; HOST and the OWNER for this forge, we would incorrectly end up
      ;; making that path part of the owner.
      (forge--split-forge-url url t)))))

;;; Identity

(cl-defgeneric forge--object-id (class &rest args)
  "Return the database id for the CLASS object specified by ARGS.")

(cl-defgeneric forge--repository-ids ( class host owner name
                                       &optional stub noerror)
  "Return the database and forge ids for the specified CLASS object.")

(cl-defmethod magit-section-ident-value ((obj forge-object))
  "Return the value ob OBJ's `id' slot.
Using OBJ itself would not be appropriate because multiple
non-equal objects may exist, representing the same thing."
  (oref obj id))

(defun forge--set-id-slot (repo object slot rows)
  "Set the value in OBJECT for SLOT to VALUE, actually storing foreign keys."
  ;; TODO Should CloSQL advice `oset' to make this unnecessary?
  (let ((repo-id (oref repo id)))
    (closql-oset
     object slot
     (mapcar (lambda (val)
               (forge--object-id repo-id
                                 (if (atom val) val (alist-get 'id val))))
             rows))))

;;; Format

(cl-defgeneric forge--format (object slot &optional spec)
  "Return a string based on SPEC and the format-string in OBJECT's SLOT.
The available `format'-like specs depend on the type of OBJECT.
SPEC can be used to add additional specs, as for `format-spec'.
The latter override the former.  SLOT is expected to be class-
allocated.  Some methods also accept a format string in place
of SLOT.")

(cl-defmethod forge--format-resource ((object forge-object) resource)
  "Return an API resource based on RESOURCE and slots of OBJECT.
For use in `forge--FORGE-METHOD' such as `forge--ghub-get'.
RESOURCE is a string separated by slashes.  Each part that begins
with a colon is replaced with a value from OBJECT.  `:repo' is a
synonym for `:name'.  `:project' is a like `:owner/:name', but the
slash is quoted on Gitlab.  `:topic' is a synonym for `:number'
but only if OBJECT is a topic.  Any other `:SLOT' means to use
the value of that slot in OBJECT, or if that doesn't exist in its
parent object (determined using `forge-get-parent')."
  (save-match-data
    (setq resource
          (replace-regexp-in-string
           ":\\([^/]+\\)"
           (lambda (str)
             (let ((slot (intern (substring str 1))))
               (or (and-let*
                       ((v (ignore-errors
                             (pcase slot
                               ('repo    (oref object name))
                               ('project (concat (string-replace
                                                  "/" "%2F" (oref object owner))
                                                 "%2F"
                                                 (oref object name)))
                               ('topic   (and (forge--childp object 'forge-topic)
                                              (oref object number)))
                               (_        (eieio-oref object slot))))))
                     (format "%s" v))
                   str)))
           resource t t))
    (if (string-match ":[^/]*" resource)
        (if-let ((parent (ignore-errors (forge-get-parent object))))
            (forge--format-resource parent resource)
          (error "Cannot resolve %s for a %s"
                 (match-string 0 resource)
                 (eieio-object-class object)))
      resource)))

;;; Miscellaneous

(defun forge-refresh-buffer (&optional buffer)
  "Refresh the current buffer, if it is a Magit or Forge buffer.
Refresh the buffer if its major-mode derives from `magit-mode'
or `forge-repository-list-mode'.  If optional BUFFER is non-nil,
then refresh that buffer, provided it is alive and satisfies
the mode requirement."
  (interactive)
  (cond (buffer
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (forge-refresh-buffer))))
        ((derived-mode-p 'forge-topic-mode)
         (magit-refresh-buffer))
        ((derived-mode-p 'magit-mode)
         (magit-refresh-buffer))
        ((and (derived-mode-p 'forge-topic-mode)
              (boundp 'forge--buffer-topics-spec)
              (oref forge--buffer-topics-spec global))
         (revert-buffer))
        ((derived-mode-p 'forge-repository-list-mode)
         (revert-buffer))))

(defun forge--sanitize-string (string)
  ;; For Gitlab this may also be nil.
  (if string (string-replace "\r\n" "\n" string) ""))

(defun forge--uuid ()
  "Return string with random (version 4) UUID."
  ;; This is a copy of `org-id-uuid'.
  ;; Only used in `forge-create-mark'.
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (current-time)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

;;; _
(provide 'forge-core)
;;; forge-core.el ends here
