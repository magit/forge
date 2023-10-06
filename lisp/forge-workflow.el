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

;;; Faces

(defface forge-workflow-success
  '((t :inherit magit-process-ok))
  "Face used for successful workflows."
  :group 'forge-faces)

(defface forge-workflow-in-progress
  '((t :inherit nil
       :foreground "#fabd2f"))
  "Face used for in-progress workflows.")

(defface forge-workflow-failure
  '((t :inherit magit-process-ng))
  "Face used for failed workflows."
  :group 'forge-faces)

(defface forge-workflow-in-progress-label
  '((t :inherit forge-topic-label
       :foreground "black"
       :background "#fabd2f"))
  "Face used for in-progress workflow labels.")

(defface forge-workflow-success-label
  '((t :inherit forge-topic-label
       :foreground "black"
       :background "#b8bb26"))
  "Face used for successful workflow labels.")

(defface forge-workflow-failure-label
  '((t :inherit forge-topic-label
       :foreground "black"
       :background "#fb4933"))
  "Face used for successful workflow labels.")
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

(defun forge--workflow-format (status-or-conclusion)
  (pcase-exhaustive status-or-conclusion
    ;; status
    ('queued           "  queued ")
    ('in-progress      " running ")
    ('completed        " complete")
    ('waiting          " waiting ")
    ('pending          " pending ")
    ('requested        "requested")
    ;; conclusion
    ('action-requested " need act")
    ('timed-out        "timed out")
    ('cancelled        "cancelled")
    ('failure          " failure ")
    ('success          " success ")
    ('neutral          " neutral ")
    ('skipped          " skipped ")
    ('startup-failure  " failure ")
    ('stale            "  stale  ")))

(defun forge--workflow-checks-info (checks)
  (let* ((total (length checks))
         (n-completed (apply #'+
                             (mapcar
                              (lambda (v)
                                (if (eq (nth 1 v) 'completed) 1 0))
                              checks)))
         (n-passed (apply #'+
                          (mapcar
                           (lambda (v)
                             (if (eq (nth 2 v) 'success) 1 0))
                           checks)))
         (in-progress-p (not (equal n-completed total)))
         (correct-p (equal n-passed total)))
    `((total . ,total)
      (n-completed . ,n-completed)
      (n-passed . ,n-passed)
      (in-progress-p . ,in-progress-p)
      (correct-p . ,correct-p))))

(defun forge--insert-workflow-status (topic)
  (when (forge--childp topic 'forge-pullreq)
    (let-alist (forge--workflow-checks-info
                (forge-sql
                 [:select [name status conclusion]
                  :from workflow
                  :where (= commit $s1)]
                 (oref topic head-rev)))
      (let* ((text (format "(%s%s/%s)"
                           (if .in-progress-p ".." "")
                           .n-passed
                           .total))
             (face (cond (.in-progress-p 'forge-workflow-in-progress-label)
                         (.correct-p 'forge-workflow-success-label)
                         (t 'forge-workflow-failure-label)))
             (closed-p (oref topic closed))
             (dimmed (when closed-p '(magit-dimmed))))
        (when (> .total 0)
          (insert " ")
          (insert text)
          (let ((o (make-overlay (- (point) (length text)) (point))))
            (overlay-put o 'priority 2)
            (overlay-put o 'evaporate t)
            (overlay-put o 'font-lock-face
                         `(,@dimmed ,face))))))))

(defun forge--insert-workflow (pullreq)
  (when-let ((workflows (forge-sql [:select [name status conclusion runs]
                                    :from workflow
                                    :where (= commit $s1)]
                                   (oref pullreq head-rev))))
    (magit-insert-section (workflow)
      (magit-insert-heading "Workflows")
      (magit-insert-section-body
        (dolist (workflow workflows)
          (pcase-exhaustive workflow
            (`(,name ,status ,conclusion ,runs)
             (magit-insert-section section (magit-section)
               (oset section heading-highlight-face
                     'magit-diff-hunk-heading-highlight)
               (magit-insert-heading
                 (concat (propertize (format "%s" name) 'face 'bold)
                         (format " (%s runs)" (length runs))
                         ": "
                         (forge--workflow-make-status-label
                          status
                          conclusion)))
               (dolist (run runs)
                 (pcase-exhaustive run
                   (`(,name ,status ,conclusion)
                    (insert (format "%s %s\n"
                                    (forge--workflow-make-status-label
                                     status conclusion)
                                    name)))))))))
        (insert "\n")))))

(defun forge--workflow-make-status-label (status conclusion)
  (if (eq status 'completed)
      (propertize (forge--workflow-format conclusion)
                  'face (pcase conclusion
                          ('success 'forge-workflow-success)
                          ('failure 'forge-workflow-failure)))
    (propertize (forge--workflow-format status) 'face 'italic)))

;;; _
(provide 'forge-workflow)
;;; forge-workflow.el ends here
