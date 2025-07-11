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

(defun forge--set-field-callback (topic)
  (let ((repo (forge-get-repository topic)))
    (cond
     ((forge-gitlab-repository--eieio-childp repo)
      ;; TODO Fetch single topic for Gitlab as well.
      (lambda (&rest _)
        (forge--pull repo #'forge-refresh-buffer)))
     ((forge-discussion--eieio-childp topic)
      ;; See comment in `forge--update-status'.
      (let ((status (oref topic status)))
        (lambda (&rest _)
          (let ((buffer (current-buffer)))
            (ghub-fetch-discussion
             (oref repo owner)
             (oref repo name)
             (oref topic number)
             (lambda (data)
               (forge--update-discussion repo data)
               (oset topic status status)
               (forge-refresh-buffer buffer))
             nil
             :host (oref repo apihost)
             :auth 'forge)))))
     ((forge--pull-topic (forge-get-repository topic) topic)))))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("partial" . "llama--left-apply-partially")
;;   ("rpartial" . "llama--right-apply-partially"))
;; End:
(provide 'forge-client)
;;; forge-client.el ends here
