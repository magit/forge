;;; forge-db.el --- Database implementation  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2022 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

(require 'closql)
(require 'compat)
(require 'eieio)
(require 'emacsql)
(require 'emacsql-sqlite)

(defvar forge--db-table-schemata)

;; For `forge--db-maybe-update':
(declare-function forge-get-issue "forge-core")
(declare-function forge-get-pullreq "forge-core")
(declare-function forge--object-id "forge-core")

;;; Options

(defcustom forge-database-connector 'sqlite
  "The database connector used by Forge.

This must be set before `forge' is loaded.  To use an alternative
connector you must install the respective package explicitly.

The default is `sqlite', which uses the `emacsql-sqlite' library
that is being maintained in the same repository as `emacsql'
itself.

If you are using Emacs 29, then the recommended connector is
`sqlite-builtin', which uses the new builtin support for SQLite.
You need to install the `emacsql-sqlite-builtin' package to use
this connector.

If you are using an older Emacs release, then the recommended
connector is `sqlite-module', which uses the module provided by
the `sqlite3' package.  This is very similar to the previous
connector and the built-in support in Emacs 29 derives from this
module.  You need to install the `emacsql-sqlite-module' package
to use this connector.

For the time being `libsqlight3' is still supported.  Do not use
this, it is an older version of the `sqlite-module' connector
from before the connector and the package were renamed.

For the time being `sqlite3' is also supported.  Do not use this.
This uses the third-party `emacsql-sqlite3' package, which uses
the official `sqlite3' command-line tool, which is not intended
to be used like this.  See https://nullprogram.com/blog/2014/02/06/."
  :package-version '(forge . "0.3.0")
  :group 'forge
  :type '(choice (const sqlite)
                 (const sqlite-builtin)
                 (const sqlite-module)
                 (const :tag "libsqlite3 (OBSOLETE)" libsqlite3)
                 (const :tag "sqlite3 (BROKEN)" sqlite3)))

(defcustom forge-database-file
  (expand-file-name "forge-database.sqlite"  user-emacs-directory)
  "The file used to store the forge database."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'file)

;;; Core

(declare-function forge-database--eieio-childp "forge-db.el" (obj) t)
(cl-ecase forge-database-connector
  (sqlite
   (defclass forge-database (emacsql-sqlite-connection closql-database)
     ((object-class :initform 'forge-repository))))
  (sqlite-builtin
   (require (quote emacsql-sqlite-builtin))
   (with-no-warnings
     (defclass forge-database (emacsql-sqlite-builtin-connection closql-database)
       ((object-class :initform 'forge-repository)))))
  (sqlite-module
   (require (quote emacsql-sqlite-module))
   (with-no-warnings
     (defclass forge-database (emacsql-sqlite-module-connection closql-database)
       ((object-class :initform 'forge-repository)))))
  (libsqlite3
   (require (quote emacsql-libsqlite3))
   (with-no-warnings
     (defclass forge-database (emacsql-libsqlite3-connection closql-database)
       ((object-class :initform 'forge-repository)))))
  (sqlite3
   (require (quote emacsql-sqlite3))
   (with-no-warnings
     (defclass forge-database (emacsql-sqlite3-connection closql-database)
       ((object-class :initform 'forge-repository))))))

(defconst forge--db-version 9)
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
           (version (closql--db-get-version db))
           (version (forge--db-maybe-update forge--db-connection version)))
      (cond
       ((> version forge--db-version)
        (emacsql-close db)
        (user-error
         "The Forge database was created with a newer Forge version.  %s"
         "You need to update the Forge package."))
       ((< version forge--db-version)
        (emacsql-close db)
        (error "The Forge database scheme changed %s (sql:%s var:%s)"
               "and there is no upgrade path" version forge--db-version)))))
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
      worktree
      (milestones :default eieio-unbound)])

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
      (marks        :default eieio-unbound)
      note
      their-id]
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

    (milestone
     [(repository :not-null)
      (id :not-null :primary-key)
      number
      title
      created
      updated
      due
      closed
      description]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

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
      (marks           :default eieio-unbound)
      note
      base-rev
      head-rev
      draft-p
      their-id]
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
  (message "Creating Forge database (%s)..." forge-database-file)
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) forge--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (closql--db-set-version db forge--db-version))
  (message "Creating Forge database (%s)...done" forge-database-file))

(defun forge--db-maybe-update (db version)
  (when (and (< version forge--db-version)
             (yes-or-no-p "Forge database needs to be updated.  Backup first? "))
    (forge--db-dump version))
  (emacsql-with-transaction db
    (when (= version 2)
      (message "Upgrading Forge database from version 2 to 3...")
      (emacsql db [:create-table pullreq-review-request $S1]
               (cdr (assq 'pullreq-review-request forge--db-table-schemata)))
      (closql--db-set-version db (setq version 3))
      (message "Upgrading Forge database from version 2 to 3...done"))
    (when (= version 3)
      (message "Upgrading Forge database from version 3 to 4...")
      (emacsql db [:drop-table notification])
      (pcase-dolist (`(,table . ,schema) forge--db-table-schemata)
        (when (memq table '(notification
                            mark issue-mark pullreq-mark))
          (emacsql db [:create-table $i1 $S2] table schema)))
      (emacsql db [:alter-table issue   :add-column marks :default $s1] 'eieio-unbound)
      (emacsql db [:alter-table pullreq :add-column marks :default $s1] 'eieio-unbound)
      (closql--db-set-version db (setq version 4))
      (message "Upgrading Forge database from version 3 to 4...done"))
    (when (= version 4)
      (message "Upgrading Forge database from version 4 to 5...")
      (emacsql db [:alter-table repository :add-column selective-p :default nil])
      (closql--db-set-version db (setq version 5))
      (message "Upgrading Forge database from version 4 to 5...done"))
    (when (= version 5)
      (message "Upgrading Forge database from version 5 to 6...")
      (emacsql db [:alter-table repository :add-column worktree :default nil])
      (closql--db-set-version db (setq version 6))
      (message "Upgrading Forge database from version 5 to 6...done"))
    (when (= version 6)
      (message "Upgrading Forge database from version 6 to 7...")
      (emacsql db [:alter-table issue   :add-column note :default nil])
      (emacsql db [:alter-table pullreq :add-column note :default nil])
      (emacsql db [:create-table milestone $S1]
               (cdr (assq 'milestone forge--db-table-schemata)))
      (emacsql db [:alter-table repository :add-column milestones :default $s1]
               'eieio-unbound)
      (pcase-dolist (`(,repo-id ,issue-id ,milestone)
                     (emacsql db [:select [repository id milestone]
                                  :from issue
                                  :where (notnull milestone)]))
        (unless (stringp milestone)
          (oset (forge-get-issue issue-id) milestone
                (forge--object-id repo-id (cdar milestone)))))
      (pcase-dolist (`(,repo-id ,pullreq-id ,milestone)
                     (emacsql db [:select [repository id milestone]
                                  :from pullreq
                                  :where (notnull milestone)]))
        (unless (stringp milestone)
          (oset (forge-get-pullreq pullreq-id) milestone
                (forge--object-id repo-id (cdar milestone)))))
      (closql--db-set-version db (setq version 7))
      (message "Upgrading Forge database from version 6 to 7...done"))
    (when (= version 7)
      (message "Upgrading Forge database from version 7 to 8...")
      (emacsql db [:alter-table pullreq :add-column base-rev :default nil])
      (emacsql db [:alter-table pullreq :add-column head-rev :default nil])
      (emacsql db [:alter-table pullreq :add-column draft-p  :default nil])
      (closql--db-set-version db (setq version 8))
      (message "Upgrading Forge database from version 7 to 8...done"))
    (when (= version 8)
      (message "Upgrading Forge database from version 8 to 9...")
      (emacsql db [:alter-table pullreq :add-column their-id :default nil])
      (emacsql db [:alter-table issue   :add-column their-id :default nil])
      (closql--db-set-version db (setq version 9))
      (message "Upgrading Forge database from version 8 to 9...done"))
    version))

(defun forge--db-dump (&optional version)
  (let ((dump (format "%s-v%s-%s.sql"
                      (file-name-sans-extension forge-database-file)
                      (or version forge--db-version)
                      (format-time-string "%Y%m%d-%H%M"))))
    (message "Dumping Forge database to %s..." dump)
    (with-temp-file dump
      (unless (zerop (save-excursion
                       (call-process "sqlite3" nil t nil
                                     forge-database-file ".dump")))
        (error "Failed to dump %s" forge-database-file))
      (insert (format "PRAGMA user_version=%s;\n" forge--db-version))
      (when (re-search-forward "^PRAGMA foreign_keys=\\(OFF\\);" 1000 t)
        (replace-match "ON" t t nil 1)))
    (message "Dumping Forge database to %s...done" dump)))

(defun forge--db-restore (dump)
  (when (and forge--db-connection (emacsql-live-p forge--db-connection))
    (emacsql-close forge--db-connection))
  (forge--db-dump)
  (with-temp-buffer
    (unless (zerop (call-process "sqlite3" nil t nil
                                 forge-database-file
                                 (format ".read %s" dump)))
      (error "Failed to read %s: %s" dump (buffer-string))))
  (forge-db))

;;; _
(provide 'forge-db)
;;; forge-db.el ends here
