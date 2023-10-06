;;; forge-workflow.el --- Workflow support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

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

(require 'forge)
(require 'forge-topic)

;;; Classes

(defclass forge-workflow (forge-topic)
  ((closql-table         :initform 'workflow)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc id)])
   (closql-class-prefix  :initform "forge-")
   (id                   :initarg :id)
   (commit               :initarg :commit)
   (name                 :initarg :name)
   (status)
   (conclusion)
   (runs)))

(cl-defmethod forge-get-workflow ((repo forge-repository) title run-number)
  (closql-get (forge-db)
              (forge--object-id 'forge-workflow repo
                                (format "%s:%s" title run-number))
              'forge-workflow))

(defun forge--workflow-parse-status (status-string)
  (pcase-exhaustive status-string
    ("QUEUED"      'queued)
    ("IN_PROGRESS" 'in-progress)
    ("COMPLETED"   'completed)
    ("WAITING"     'waiting)
    ("PENDING"     'pending)
    ("REQUESTED"   'requested)))

(defun forge--workflow-parse-conclusion (conclusion-string)
  (pcase-exhaustive conclusion-string
    ('nil               'in-progress)
    ("ACTION_REQUESTED" 'action-requested)
    ("TIMED_OUT"        'timed-out)
    ("CANCELLED"        'cancelled)
    ("FAILURE"          'failure)
    ("SUCCESS"          'success)
    ("NEUTRAL"          'neutral)
    ("SKIPPED"          'skipped)
    ("STARTUP_FAILURE"  'startup-failure)
    ("STALE"            'stale)))

;;; _
(provide 'forge-workflow)
;;; forge-workflow.el ends here
