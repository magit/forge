;;; forge-client.el --- GraphQL and REST support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

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

(cl-defun forge--graphql (graphql
                          &optional variables
                          &key username host forge
                          headers
                          callback errorback)
  (ghub--graphql-vacuum graphql variables callback nil
                        :username  username
                        :auth      'forge
                        :host      host
                        :forge     forge
                        :headers   headers
                        :errorback errorback))

(defun forge--set-field-callback (topic &optional preserve-status)
  (let ((status (oref topic status)))
    (lambda (&rest _)
      (forge--pull-topic
       (forge-get-repository topic)
       topic
       :callback (lambda ()
                   ;; Necessary when setting a discussion field because
                   ;; the API provides even less information about the
                   ;; status of discussions compared to other topics and
                   ;; as a result we would otherwise always switch the
                   ;; status to `unread'.  This is not needed for every
                   ;; modification of a discussion because some of them
                   ;; (e.g., setting labels) do not cause `updated_at' to
                   ;; be bumped; this second defect cancels out the first
                   ;; when it comes to this function.
                   (when preserve-status
                     (oset topic status status))
                   (forge-refresh-buffer))))))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("partial" . "llama--left-apply-partially")
;;   ("rpartial" . "llama--right-apply-partially"))
;; End:
(provide 'forge-client)
;;; forge-client.el ends here
