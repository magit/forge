;;; forge-tablist.el --- Tabulated-list interface  -*- lexical-binding:t -*-

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

(require 'tabulated-list)

(require 'forge)

(defconst forge--tablist-columns-type
  '(repeat
    (list :tag "Column"
          (string  :tag "Header Label")
          (choice  :tag "Value source"
                   function
                   (symbol :tag "Object slot"))
          (integer :tag "Column Width")
          (choice  :tag "Sort predicate"
                   (const :tag "Don't sort" nil)
                   (const :tag "Default" t)
                   function)
          (plist   :tag "Properties"
                   :key-type (choice :tag "Property"
                                     (const :right-align)
                                     (const :pad-right)
                                     symbol)
                   :value-type (sexp :tag "Value")))))

(defvar-local forge--tabulated-list-columns nil)
(put 'forge--tabulated-list-columns 'permanent-local t)

(defvar-local forge--tabulated-list-query nil)
(put 'forge--tabulated-list-query 'permanent-local t)

(defun forge--tablist-refresh ()
  (setq tabulated-list-format
        (vconcat (mapcar (pcase-lambda (`(,name ,_get ,width ,sort ,props))
                           `(,name ,width ,sort . ,props))
                         forge--tabulated-list-columns)))
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar
         (lambda (obj)
           (list (oref obj id)
                 (vconcat
                  (mapcar (pcase-lambda (`(,_name ,get ,_width ,_sort ,_props))
                            (let ((val (cond
                                        ((functionp get)
                                         (funcall get obj))
                                        ((eq (car-safe get) 'repository)
                                         (eieio-oref (forge-get-repository obj)
                                                     (cadr get)))
                                        ((eieio-oref obj get)))))
                              (cond ((stringp val) val)
                                    ((null val) "")
                                    ((format "%s" val)))))
                          forge--tabulated-list-columns))))
         (funcall forge--tabulated-list-query))))

;;; _
(provide 'forge-tablist)
;;; forge-tablist.el ends here
