;;; forge-db.el --- Database implementation       -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Jonas Bernoulli

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

(require 'closql)
(require 'eieio)
(require 'emacsql)
(require 'emacsql-sqlite)

(defvar forge--db-table-schemata)

(declare-function forge-reset-database "forge")

;;; Options

(defcustom forge-database-file
  (expand-file-name "forge-database.sqlite"  user-emacs-directory)
  "The file used to store the forge database."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'file)

;;; Core

(defclass forge-database (closql-database)
  ((object-class :initform forge-repository)))

(defconst forge--db-version 6)
(defconst forge--sqlite-available-p
  (with-demoted-errors "Forge initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar forge--db-connection nil
  "The EmacSQL database connection.")

(defun forge-db ()
  (unless (and forge--db-connection (emacsql-live-p forge--db-connection))
    (make-directory (file-name-directory forge-database-file) t)
    (closql-db 'forge-database 'forge--db-connection
               forge-database-file t)
    (let* ((db forge--db-connection)
           (version (caar (emacsql db "PRAGMA user_version")))
           (version (forge--db-maybe-update forge--db-connection version)))
      (cond
       ((> version forge--db-version)
        (emacsql-close db)
        (user-error
         "The Forge database was created with a newer Forge version.  %s"
         "You need to update the Forge package."))
       ((< version forge--db-version)
        (emacsql-close db)
        (error "BUG: The Forge database scheme changed %s"
               "and there is no upgrade path")))))
  forge--db-connection)

;;; Api

(defun forge-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (forge-db) (apply #'format sql args))
    (apply #'emacsql (forge-db) sql args)))

;;; Schemata

(defconst forge--db-table-schemata
  '((repository
     [(class :not-null)
      (id :not-null :primary-key)
      forge-id
      forge
      owner
      name
      apihost
      githost
      remote
      sparse-p
      created
      updated
      pushed
      parent
      description
      homepage
      default-branch
      archived-p
      fork-p
      locked-p
      mirror-p
      private-p
      issues-p
      wiki-p
      stars
      watchers
      (assignees :default eieio-unbound)
      (forks     :default eieio-unbound)
      (issues    :default eieio-unbound)
      (labels    :default eieio-unbound)
      (revnotes  :default eieio-unbound)
      (pullreqs  :default eieio-unbound)
      selective-p
      worktree])

    (assignee
     [(repository :not-null)
      (id :not-null :primary-key)
      login
      name
      forge-id] ; Needed for Gitlab.
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (fork
     [(parent :not-null)
      (id :not-null :primary-key)
      owner
      name]
     (:foreign-key
      [parent] :references repository [id]
      :on-delete :cascade))

    (issue
     [(class :not-null)
      (id :not-null :primary-key)
      repository
      number
      state
      author
      title
      created
      updated
      closed
      unread-p
      locked-p
      milestone
      body
      (assignees    :default eieio-unbound)
      (cards        :default eieio-unbound)
      (edits        :default eieio-unbound)
      (labels       :default eieio-unbound)
      (participants :default eieio-unbound)
      (posts        :default eieio-unbound)
      (reactions    :default eieio-unbound)
      (timeline     :default eieio-unbound)
      (marks        :default eieio-unbound)]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (issue-assignee
     [(issue :not-null)
      (id :not-null)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade))

    (issue-label
     [(issue :not-null)
      (id :not-null)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references label [id]
      :on-delete :cascade))

    (issue-mark
     [(issue :not-null)
      (id :not-null)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references mark [id]
      :on-delete :cascade))

    (issue-post
     [(class :not-null)
      (id :not-null :primary-key)
      issue
      number
      author
      created
      updated
      body
      (edits :default eieio-unbound)
      (reactions :default eieio-unbound)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade))

    (label
     [(repository :not-null)
      (id :not-null :primary-key)
      name
      color
      description]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (mark
     [;; For now this is always nil because it seems more useful to
      ;; share marks between repositories.  We cannot omit this slot
      ;; though because `closql--iref' expects `id' to be the second
      ;; slot.
      repository
      (id :not-null :primary-key)
      name
      face
      description])

    (notification
     [(class :not-null)
      (id :not-null :primary-key)
      thread-id
      repository
      forge
      reason
      unread-p
      last-read
      updated
      title
      type
      topic
      url]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (pullreq
     [(class :not-null)
      (id :not-null :primary-key)
      repository
      number
      state
      author
      title
      created
      updated
      closed
      merged
      unread-p
      locked-p
      editable-p
      cross-repo-p
      base-ref
      base-repo
      head-ref
      head-user
      head-repo
      milestone
      body
      (assignees       :default eieio-unbound)
      (cards           :default eieio-unbound)
      (commits         :default eieio-unbound)
      (edits           :default eieio-unbound)
      (labels          :default eieio-unbound)
      (participants    :default eieio-unbound)
      (posts           :default eieio-unbound)
      (reactions       :default eieio-unbound)
      (review-requests :default eieio-unbound)
      (reviews         :default eieio-unbound)
      (timeline        :default eieio-unbound)
      (marks           :default eieio-unbound)]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (pullreq-assignee
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (pullreq-label
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references label [id]
      :on-delete :cascade))

    (pullreq-mark
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references mark [id]
      :on-delete :cascade))

    (pullreq-post
     [(class :not-null)
      (id :not-null :primary-key)
      pullreq
      number
      author
      created
      updated
      body
      (edits :default eieio-unbound)
      (reactions :default eieio-unbound)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (pullreq-review-request
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (revnote
     [(class :not-null)
      (id :not-null :primary-key)
      repository
      commit
      file
      line
      author
      body]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))))

(cl-defmethod closql--db-init ((db forge-database))
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) forge--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" forge--db-version))))

(defun forge--db-maybe-update (db version)
  (emacsql-with-transaction db
    (when (= version 2)
      (message "Upgrading Forge database from version 2 to 3...")
      (emacsql db [:create-table pullreq-review-request $S1]
               (cdr (assq 'pullreq-review-request forge--db-table-schemata)))
      (emacsql db "PRAGMA user_version = 3")
      (setq version 3)
      (message "Upgrading Forge database from version 2 to 3...done"))
    (when (= version 3)
      (message "Upgrading Forge database from version 3 to 4...")
      (emacsql db [:drop-table notification])
      (pcase-dolist (`(,table . ,schema) forge--db-table-schemata)
        (when (memq table '(notification
                            mark issue-mark pullreq-mark))
          (emacsql db [:create-table $i1 $S2] table schema)))
      (emacsql db [:alter-table issue   :add-column mark :default $i1] eieio-unbound)
      (emacsql db [:alter-table pullreq :add-column mark :default $i1] eieio-unbound)
      (emacsql db "PRAGMA user_version = 4")
      (setq version 4)
      (message "Upgrading Forge database from version 3 to 4...done"))
    (when (= version 4)
      (message "Upgrading Forge database from version 4 to 5...")
      (emacsql db [:alter-table repository :add-column selective-p :default nil])
      (emacsql db "PRAGMA user_version = 5")
      (setq version 5)
      (message "Upgrading Forge database from version 4 to 5...done"))
    (when (= version 5)
      (message "Upgrading Forge database from version 5 to 6...")
      (emacsql db [:alter-table repository :add-column worktree :default nil])
      (emacsql db "PRAGMA user_version = 6")
      (setq version 6)
      (message "Upgrading Forge database from version 5 to 6...done"))
    version))

;;; _
(provide 'forge-db)
;;; forge-db.el ends here
