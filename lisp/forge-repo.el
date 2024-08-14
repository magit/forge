;;; forge-repo.el --- Repository support  -*- lexical-binding:t -*-

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

(require 'forge)
(require 'eieio)

;;; Classes

(defclass forge-repository (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-class-suffix       :initform "-repository")
   (closql-table              :initform 'repository)
   (closql-primary-key        :initform 'id)
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
   (id                        :initform nil :initarg :id)
   (forge-id                  :initform nil :initarg :forge-id)
   (forge                     :initform nil :initarg :forge)
   (owner                     :initform nil :initarg :owner)
   (name                      :initform nil :initarg :name)
   (apihost                   :initform nil :initarg :apihost)
   (githost                   :initform nil :initarg :githost)
   (remote                    :initform nil :initarg :remote)
   (condition                 :initform :stub)
   (created                   :initform nil)
   (updated                   :initform nil)
   (pushed                    :initform nil)
   (parent                    :initform nil)
   (description               :initform nil)
   (homepage                  :initform nil)
   (default-branch            :initform nil)
   (archived-p                :initform nil)
   (fork-p                    :initform nil)
   (locked-p                  :initform nil)
   (mirror-p                  :initform nil)
   (private-p                 :initform nil)
   (issues-p                  :initform t)
   (wiki-p                    :initform nil)
   (stars                     :initform nil)
   (watchers                  :initform nil)
   (assignees                 :closql-table assignee)
   (forks                     :closql-table fork)
   (issues                    :closql-class forge-issue)
   (labels                    :closql-table label)
   (pullreqs                  :closql-class forge-pullreq)
   (revnotes                  :closql-class forge-revnote)
   (selective-p               :initform nil)
   (worktree                  :initform nil)
   (milestones                :closql-table milestone)
   (issues-until              :initform nil)
   (pullreqs-until            :initform nil))
  :abstract t)

(defclass forge-unusedapi-repository (forge-repository) () :abstract t)

(defclass forge-noapi-repository (forge-repository) () :abstract t)

(cl-defmethod slot-missing ((object forge-repository)
                            slot-name operation &optional _new-value)
  (if (and (eq operation 'oref)
           (eq slot-name 'slug))
      (concat (oref object owner) "/"
              (oref object name))
    (cl-call-next-method)))

;;; Query
;;;; Get

(defvar-local forge-buffer-repository nil)
(put 'forge-buffer-repository 'permanent-local t)

(defvar-local forge-buffer-unassociated-p nil)

(defconst forge--signal-no-entry '(:tracked :stub :insert!))

(defun forge--get-remote (&optional warn ignore-variable)
  (let* ((remotes (magit-list-remotes))
         (config (and (not ignore-variable)
                      (magit-get "forge.remote")))
         (remote (if (cdr remotes)
                     (or (car (member config remotes))
                         (car (member "upstream" remotes))
                         (car (member "origin" remotes)))
                   (car remotes))))
    (when (and warn config remote (not (equal config remote)))
      (message "Ignored forge.remote=%s; no such remote.\nSee %s." config
               "https://magit.vc/manual/forge/How-Forge-Detection-Works.html"))
    remote))

(cl-defmethod forge-get-repository ((_(eql :id)) id)
  (closql-get (forge-db) (substring-no-properties id) 'forge-repository))

(cl-defmethod forge-get-repository ((_(eql :dir)) dir)
  (let ((default-directory dir)
        (forge-buffer-repository nil)
        (forge-buffer-topic nil))
    (forge-get-repository :stub? nil 'notatpt)))

(cl-defmethod forge-get-repository ((demand symbol) &optional remote notatpt)
  "Return the current forge repository.

First check if `forge-buffer-repository', or if that is nil, then
the repository for `forge-buffer-topic', satisfies DEMAND.  If so,
then return that repository.

Otherwise return the repository for `default-directory', if that
exists and satisfies DEMAND.  If that fails too, then return nil
or signal an error, depending on DEMAND."
  (or (and-let* ((repo (and (not notatpt)
                            (forge-repository-at-point))))
        (forge-get-repository repo 'noerror demand))
      (and-let* ((repo (or (forge-buffer-repository)
                           (and forge-buffer-topic
                                (forge-get-repository forge-buffer-topic)))))
        (forge-get-repository repo 'noerror demand))
      (magit--with-refresh-cache
          (list default-directory 'forge-get-repository demand)
        (if (not (magit-gitdir))
            (when (memq demand forge--signal-no-entry)
              (error
               "Cannot determine Forge repository outside of Git repository"))
          (unless remote
            (setq remote (forge--get-remote 'warn)))
          (if-let ((url (and remote
                             (magit-git-string "remote" "get-url" remote))))
              (and-let* ((repo (forge-get-repository url remote demand)))
                (progn ; work around debbugs#31840
                  (oset repo worktree (magit-toplevel))
                  repo))
            (when (memq demand forge--signal-no-entry)
              (error
               "Cannot determine forge repository.  %s\nSee %s."
               (cond (remote (format "No url configured for %S." remote))
                     ((and-let* ((config (magit-get "forge.remote")))
                        (format "Value of `forge.remote' is %S but %s"
                                config "that remote does not exist.")))
                     ((magit-list-remotes) "Cannot decide on remote to use.")
                     (t "No remote configured."))
               "https://magit.vc/manual/forge/How-Forge-Detection-Works.html"
               )))))))

(cl-defmethod forge-get-repository ((url string) &optional remote demand)
  "Return the repository at URL."
  (if-let ((parts (forge--split-forge-url url)))
      (forge-get-repository parts remote (or demand :known?))
    (when (memq demand forge--signal-no-entry)
      (error "Cannot determine forge repository.  %s isn't a forge URL.  %s"
             url "You might have to customize `forge-alist'."))))

(cl-defmethod forge-get-repository (((host owner name) list)
                                    &optional remote demand)
  "((HOST OWNER NAME) &optional REMOTE DEMAND)

Return the repository identified by HOST, OWNER and NAME.
See `forge-alist' for valid Git hosts."
  (setq host  (substring-no-properties host))
  (setq owner (substring-no-properties owner))
  (setq name  (substring-no-properties name))
  (unless (memq demand '( :tracked :tracked?
                          :known? :insert! :valid?
                          :stub :stub?))
    (if-let ((new (pcase demand
                    ('t      :tracked)
                    ('full   :tracked?)
                    ('nil    :known?)
                    ('create :insert!)
                    ('stub   :stub)
                    ('maybe  :stub?))))
        (progn
          (message "Obsolete value for `%s's DEMAND: `%s'; use `%s' instead"
                   'forge-get-repository demand new)
          (setq demand new))
      (error "Unknown value for `%s's DEMAND: `%s'"
             'forge-get-repository demand)))
  (if-let ((spec (forge--get-forge-host host t)))
      (pcase-let ((`(,githost ,apihost ,webhost ,class) spec))
        ;; The `webhost' is used to identify the corresponding forge.
        ;; For that reason it is stored in the `forge' slot.  The id
        ;; stored in the `id' slot also derives from that value.
        (let* ((row (car (forge-sql [:select * :from repository
                                     :where (and (= forge $s1)
                                                 (= owner $s2)
                                                 (= name  $s3))]
                                    webhost owner name)))
               (obj (and row (closql--remake-instance class (forge-db) row))))
          ;; Synchronize the object with the entry from `forge-alist'.
          ;; This only has an effect if the entry was modified, which
          ;; should rarely, if ever, happen.  Avoid confusion, by not
          ;; mentioning this detail in any docstring.
          (when obj
            (oset obj apihost apihost)
            (oset obj githost githost)
            (oset obj remote  remote))
          (pcase (list demand (and obj (eq (oref obj condition) :tracked)))
            (`(:tracked? nil) (setq obj nil))
            (`(:tracked  nil)
             (error "Cannot use `%s' in %S yet.\n%s"
                    this-command (magit-toplevel)
                    "Use `M-x forge-add-repository' before trying again.")))
          (when (and (memq demand '(:insert! :valid? :stub :stub?))
                     (not obj))
            (pcase-let ((`(,id . ,forge-id)
                         (forge--repository-ids
                          class webhost owner name
                          (memq demand '(:stub :stub?))
                          (eq demand :valid?))))
              (if (not id)
                  ;; `:valid?' was used and it turned out it is not.
                  (setq obj nil)
                ;; The repo might have been renamed on the forge.  #188
                (unless (setq obj (forge-get-repository :id id))
                  (setq obj (funcall class
                                     :id       id
                                     :forge-id forge-id
                                     :forge    webhost
                                     :owner    owner
                                     :name     name
                                     :apihost  apihost
                                     :githost  githost
                                     :remote   remote))
                  (when (eq demand :insert!)
                    (closql-insert (forge-db) obj)
                    (oset obj condition :known))))))
          obj))
    (when (memq demand forge--signal-no-entry)
      (error "Cannot determine forge repository.  No entry for %S in %s"
             host 'forge-alist))))

(cl-defmethod forge-get-repository ((repo forge-repository)
                                    &optional noerror demand)
  (setq noerror (and noerror t))
  (with-slots (condition slug) repo
    (cl-symbol-macrolet
        ((err (error "Requested %s for %s, but is %s" demand slug condition))
         (key (list (oref repo forge)
                    (oref repo owner)
                    (oref repo name)))
         (ins (forge-get-repository key nil :insert!))
         (set (forge-get-repository key nil :valid?)))
      (pcase-exhaustive (list demand condition noerror)
        (`(nil       ,_                     ,_)  repo)
        (`(:tracked? :tracked               ,_)  repo)
        (`(:tracked? ,_                     ,_)   nil)
        (`(:tracked  :tracked               ,_)  repo)
        (`(:tracked  ,_                      t)   nil)
        (`(:tracked  ,_                    nil)   err)
        (`(:known?   ,(or :tracked :known)  ,_)  repo)
        (`(:known?   ,_                     ,_)   nil)
        (`(:insert!  ,(or :tracked :known)  ,_)  repo)
        (`(:insert!  ,_                     ,_)   ins)
        (`(:valid?   ,(or :tracked :known)  ,_)  repo)
        (`(:valid?   ,_                     ,_)   set)
        (`(:stub?    ,_                     ,_)  repo)
        (`(:stub     ,_                     ,_)  repo)))))

(cl-defmethod forge-get-repository ((_ null) &optional noerror demand)
  (if (and (memq demand '(:insert! :tracked :stub))
           (not noerror))
      (error "(Maybe repository) is nil; `%s' not satisfied" demand)
    nil))

(defun forge--get-repository:tracked? ()
  (forge-get-repository :tracked?))

(defun forge-repository-at-point (&optional demand)
  "Return the repository at point.
If there is no such repository and DEMAND is non-nil, then signal
an error."
  (or (magit-section-value-if 'forge-repo)
      (and-let* ((topic (forge-topic-at-point)))
        (forge-get-repository topic))
      (and (derived-mode-p 'forge-repository-list-mode)
           (and-let* ((id (tabulated-list-get-id)))
             (forge-get-repository :id id)))
      (and demand (user-error "No repository at point"))))

(defun forge--repo-for-thingatpt ()
  (or (magit-section-value-if 'forge-repo)
      (and-let* ((topic (magit-section-value-if '(issue pullreq))))
        (forge-get-repository topic))
      (and (not forge-buffer-unassociated-p)
           (or (forge-buffer-repository)
               (forge-get-repository :known? nil 'notatpt)))))

(defun forge-buffer-repository ()
  (and-let* ((id forge-buffer-repository))
    (forge-get-repository :id id)))

(defun forge-set-buffer-repository ()
  "Initialize the value of variable `forge-buffer-repository'."
  (unless forge-buffer-repository
    (and-let* ((repo (forge-get-repository :known?)))
      (setq forge-buffer-repository (oref repo id)))))

(add-hook 'magit-mode-hook #'forge-set-buffer-repository)

(defun forge-get-worktree (repo)
  "Validate, remember and return a worktree for REPO.
If `default-directory' is within one of REPO's worktrees, record that
location in its `worktree' slot and return it.  Otherwise, if a worktree
has been recorded before, validate that.  If it still is a worktree of
REPO, return it, else set the slot to nil and return nil."
  (if-let (((forge-repository-equal
             repo (forge-get-repository :dir default-directory)))
           (current-tree (magit-toplevel)))
      (oset repo worktree current-tree)
    (and-let* ((saved-tree (oref repo worktree)))
      (and (file-accessible-directory-p saved-tree)
           (if (forge-repository-equal
                repo (forge-get-repository :dir saved-tree))
               saved-tree
             (oset repo worktree nil))))))

;;;; List

(defun forge--ls-repos ()
  (mapcar (let ((db (forge-db)))
            (lambda (row)
              (closql--remake-instance 'forge-repository db row)))
          (forge-sql [:select * :from repository
                      :order-by [(asc owner) (asc name)]])))

(defun forge--ls-owned-repos ()
  (mapcar (let ((db (forge-db)))
            (lambda (row)
              (closql--remake-instance 'forge-repository db row)))
          (forge-sql [:select * :from repository
                      :where (and (in owner $v1)
                                  (not (in name $v2)))
                      :order-by [(asc owner) (asc name)]]
                     (vconcat (mapcar #'car forge-owned-accounts))
                     (vconcat forge-owned-ignored))))

;;; Identity

(defun forge-repository-equal (repo1 repo2)
  "Return t if REPO1 and REPO2 are the same repository.
REPO1 and/or REPO2 may also be nil, in which case return nil."
  (and repo1 repo2
       (or (equal      (oref repo1 id)      (oref repo2 id))
           (and (equal (oref repo1 githost) (oref repo2 githost))
                (equal (oref repo1 owner)   (oref repo2 owner))
                (equal (oref repo1 name)    (oref repo2 name))))))

(cl-defmethod forge--repository-ids ((class (subclass forge-repository))
                                     host owner name &optional stub noerror)
  "Return (OUR-ID . THEIR-ID) of the specified repository.
If optional STUB is non-nil, then the IDs are not guaranteed to
be unique.  Otherwise this method has to make an API request to
retrieve THEIR-ID, the repository's ID on the forge.  In that
case OUR-ID derives from THEIR-ID and is unique across all
forges and hosts."
  (pcase-let* ((`(,_githost ,apihost ,id ,_class)
                (forge--get-forge-host host t))
               (path (format "%s/%s" owner name))
               (their-id (and (not stub)
                              (ghub-repository-id
                               owner name
                               :host apihost
                               :auth 'forge
                               :forge (forge--ghub-type-symbol class)
                               :noerror noerror))))
    (and (or stub their-id (not noerror))
         (cons (base64-encode-string
                (format "%s:%s" id
                        (cond (stub path)
                              ((eq class 'forge-github-repository)
                               ;; This is base64 encoded, according to
                               ;; https://docs.github.com/en/graphql/
                               ;; reference/scalars#id.  Unfortunately
                               ;; that is not always true.  E.g.,
                               ;; https://github.com/dit7ya/roamex.
                               (condition-case nil
                                   (base64-decode-string their-id)
                                 (error their-id)))
                              (t their-id)))
                t)
               (or their-id path)))))

(cl-defmethod forge--repository-ids ((_class (subclass forge-noapi-repository))
                                     host owner name &optional _stub _noerror)
  (let ((their-id (if owner (concat owner "/" name) name)))
    (cons (base64-encode-string
           (format "%s:%s"
                   (nth 3 (forge--get-forge-host host t))
                   their-id)
           t)
          their-id)))

;;; Read

(defun forge-read-repository (prompt)
  (let ((choice (magit-completing-read
                 prompt
                 (mapcar (pcase-lambda (`(,host ,owner ,name))
                           (format "%s/%s @%s" owner name host))
                         (forge-sql [:select [githost owner name]
                                     :from repository]))
                 nil t nil nil
                 (and-let* ((default (forge-get-repository :stub?)))
                   (format "%s/%s @%s"
                           (oref default owner)
                           (oref default name)
                           (oref default githost))))))
    (save-match-data
      (if (string-match "\\`\\(.+\\)/\\([^/]+\\) @\\(.+\\)\\'" choice)
          (forge-get-repository (list (match-string 3 choice)
                                      (match-string 1 choice)
                                      (match-string 2 choice)))
        (error "BUG")))))

(defun forge-read-host (prompt &optional class)
  (magit-completing-read
   prompt
   (if class
       (seq-keep (pcase-lambda (`(,githost ,_apihost ,_webhost ,c))
                   (and (child-of-class-p c class) githost))
                 forge-alist)
     (mapcar #'car forge-alist))
   nil t))

;;; Miscellaneous

(defun forge--as-githost (host)
  (or (car (car (cl-member host forge-alist :test #'equal :key #'car)))
      (car (car (cl-member host forge-alist :test #'equal :key #'cadr)))
      (car (car (cl-member host forge-alist :test #'equal :key #'caddr)))
      (user-error "Cannot determine githost for %S" host)))

(defun forge--as-apihost (host)
  (or (cadr (car (cl-member host forge-alist :test #'equal :key #'cadr)))
      (cadr (car (cl-member host forge-alist :test #'equal :key #'car)))
      (cadr (car (cl-member host forge-alist :test #'equal :key #'caddr)))
      (user-error "Cannot determine apihost for %S" host)))

(cl-defmethod forge--format ((repo forge-repository) format-or-slot &optional spec)
  (pcase-let* (((eieio (forge webhost) owner name) repo)
               (path (if owner (concat owner "/" name) name)))
    (format-spec
     (let ((format (if (symbolp format-or-slot)
                       (eieio-oref repo format-or-slot)
                     format-or-slot)))
       (if (member webhost ghub-insecure-hosts)
           (replace-regexp-in-string "\\`https://" "http://" format t t)
         format))
     `(,@spec
       (?h . ,webhost)
       (?o . ,owner)
       (?n . ,name)
       (?p . ,path)
       (?P . ,(string-replace "/" "%2F" path))))))

(defun forge--set-field-callback (topic)
  (lambda (&rest _)
    (forge--pull-topic
     (forge-get-repository topic)
     topic
     :callback (lambda ()
                 (forge-refresh-buffer)
                 (when (transient-active-prefix
                        '(forge-topic-menu
                          forge-topics-menu
                          forge-notifications-menu))
                   (transient--refresh-transient))))))

(defvar forge--mode-line-buffer nil)

(defun forge--msg (repo echo done format &rest args)
  (let ((msg (apply #'format format args)))
    (when repo
      (setq msg (string-replace
                 "REPO"
                 (concat (oref repo owner) "/" (oref repo name))
                 msg)))
    (when (and echo msg)
      (message "%s%s" msg (if done "...done" "...")))
    (when (buffer-live-p forge--mode-line-buffer)
      (with-current-buffer forge--mode-line-buffer
        (setq mode-line-process
              (if done
                  nil
                (concat " " (propertize msg 'font-lock-face
                                        'magit-mode-line-process)))))
      (force-mode-line-update t))))

(cl-defmethod ghub--host ((repo forge-repository))
  (cl-call-next-method (forge--ghub-type-symbol (eieio-object-class repo))))

(cl-defmethod ghub--username ((repo forge-repository))
  (let ((default-directory default-directory))
    (unless (forge-repository-equal (forge-get-repository :stub?) repo)
      (when-let ((worktree (forge-get-worktree repo)))
        (setq default-directory worktree)))
    (cl-call-next-method (oref repo apihost)
                         (forge--ghub-type-symbol (eieio-object-class repo)))))

(defun forge--ghub-type-symbol (class)
  (pcase-exhaustive class
    ;; This package does not define a `forge-gitlab-http-repository'
    ;; class, but we used to suggest at #9 that users define such a class
    ;; if they must connect to a Gitlab instance that uses http instead
    ;; of https.  Doing that isn't necessary anymore, but we have to keep
    ;; supporting it here.  It is now sufficient to add an entry to
    ;; `ghub-insecure-hosts'.
    ((or 'forge-gitlab-repository 'forge-gitlab-http-repository) 'gitlab)
    ('forge-github-repository    'github)
    ('forge-gitea-repository     'gitea)
    ('forge-gogs-repository      'gogs)
    ('forge-bitbucket-repository 'bitbucket)))

;;; _
(provide 'forge-repo)
;;; forge-repo.el ends here
