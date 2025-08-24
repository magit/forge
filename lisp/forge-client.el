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

;;; GraphQL

(cl-defun forge--query ( obj-or-host query variables
                         &key callback errorback noerror narrow until)
  (declare (indent defun))
  (pcase-let ((`(,host ,forge) (forge--host-arguments obj-or-host)))
    (ghub-query query variables
      :auth 'forge :host host :forge forge
      :callback callback :errorback errorback :noerror noerror
      :narrow narrow :until until)))

(cl-defmacro forge-query ( obj-or-host query variables
                           &key callback errorback noerror narrow until)
  (declare (indent defun))
  `(forge--query ,obj-or-host
     '(query ,query)
     ,(forge--prepare-variables variables)
     :callback ,callback :errorback ,errorback :noerror ,noerror
     :narrow ,narrow :until ,until))

(cl-defmacro forge-mutate ( obj-or-host mutation variables
                            &key callback errorback noerror)
  (declare (indent defun))
  `(forge--query ,obj-or-host
     ',(ghub--prepare-mutation mutation)
     (list (cons 'input ,(forge--prepare-variables variables)))
     :callback ,callback :errorback ,errorback :noerror ,noerror))

(cl-defmacro forge--mutate-field (topic mutation variables)
  (declare (indent defun))
  `(let ((topic ,topic))
     (forge--query topic
       ',(ghub--prepare-mutation mutation)
       (list (cons 'input ,(forge--prepare-variables variables)))
       :callback (forge--set-field-callback topic))))

;;; REST

(cl-defun forge--rest ( obj-or-host method resource &optional params
                        &key callback errorback noerror unpaginate)
  (declare (indent defun))
  (pcase-let ((`(,host ,forge) (forge--host-arguments obj-or-host)))
    (ghub-request method
      (if (cl-typep obj-or-host 'forge-object)
          (forge--format-resource obj-or-host resource)
        resource)
      params
      :auth 'forge :host host :forge forge
      :callback callback :errorback errorback :noerror noerror
      :unpaginate unpaginate)))

(cl-defmacro forge-rest ( obj-or-host method resource &optional params
                          &key callback errorback noerror unpaginate)
  (declare (indent defun))
  `(forge--rest ,obj-or-host ,method ,resource
     ,(forge--prepare-variables params)
     :callback ,callback :errorback ,errorback :noerror ,noerror
     :unpaginate ,unpaginate))

;;; Internal

(defun forge--host-arguments (obj-or-host)
  (let* ((repo (and (cl-typep obj-or-host 'forge-object)
                    (forge-get-repository obj-or-host)))
         (host (if (stringp obj-or-host)
                   obj-or-host
                 (oref repo apihost))))
    (list host (pcase (if repo
                          (eieio-object-class-name repo)
                        (nth 3 (cl-find host forge-alist
                                        :key #'cadr :test #'equal)))
                 ('forge-github-repository 'github)
                 ('forge-gitlab-repository 'gitlab)
                 (_ 'github)))))

(defun ghub--prepare-mutation (mutation &optional var)
  `(mutation
    (,mutation
     [(input ,(if var (intern (format "$%s" var)) '$input)
             ,(let ((name (symbol-name mutation)))
                (intern (format "%s%sInput!"
                                (upcase (substring name 0 1))
                                (substring name 1)))))]
     ;; We ignore the payload, but GraphQL requires that at least one
     ;; field is specified.  On Github this field is available for all
     ;; mutations.  On Gitlab there is no field that is available for
     ;; all mutations, but luckily using an invalid fields does not
     ;; result in an error.
     clientMutationId)))

(defun forge--prepare-variables (variables)
  `(delq nil (list ,@(mapcar (lambda (binding)
                               (pcase-exhaustive binding
                                 (`(,var ,val)
                                  `(cons ',var ,val))
                                 (`(and ,cond (,var ,val))
                                  `(and ,cond (cons ',var ,val)))))
                             variables))))

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
          (forge--query repo
            (ghub--graphql-prepare-query
             forge--github-repository-query
             `(repository discussions (discussion . ,(oref topic number))))
            `((owner . ,(oref repo owner))
              (name  . ,(oref repo name)))
            :callback (lambda (data)
                        (forge--update-discussion repo (cdr (cadr (cadr data))))
                        (oset topic status status)
                        (forge-refresh-buffer))))))
     ((lambda (&rest _)
        (forge--pull-topic (forge-get-repository topic) topic))))))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"          . "cond-let--and$")
;;   ("and-let"       . "cond-let--and-let")
;;   ("if-let"        . "cond-let--if-let")
;;   ("when-let"      . "cond-let--when-let"))
;; End:
(provide 'forge-client)
;;; forge-client.el ends here
