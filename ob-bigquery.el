;;; ob-bigquery.el --- Babel Functions for BigQuery Databases -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Free Software Foundation, Inc.

;; Author: Luis Miguel Hernanz
;; Maintainer:
;; Keywords: literate programming, reproducible research
;; URL:

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating BigQuery source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'ob-sql)

(declare-function org-table-convert-region "org-table"
		  (beg0 end0 &optional separator))
;;(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function org-table-to-lisp "org-table" (&optional txt))

(add-to-list 'org-src-lang-modes  '("bigquery" . sql))
;;(add-to-list 'org-man-source-highlight-langs  '(bigquery "sql"))

(defvar org-babel-default-header-args:bigquery '(
                                                 (:format . "pretty")
                                                 (:headers-p . "yes")
                                                 ))

(defvar org-babel-header-args:bigquery
  '((project   . :any)
    (format    . :any)
    (headers-p . :any)
    )
  "Bigquery specific header args.")

(defun org-babel-expand-body:bigquery (body params)
  "Expand BODY according to the values of PARAMS."
  (org-babel-sql-expand-vars
   body (org-babel--get-vars params) t))

(defvar org-babel-bigquery-command "bq --headless -sync")

(defun org-babel-execute:bigquery (body params)
  "Execute a block of Bigquery code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
	      (project (cdr (assq :project params)))
	      (format (cdr (assq :format params)))
        (headers-p (cdr (assq :format params)))
        )
    (with-temp-buffer
      (insert
       (org-babel-eval
	      (org-fill-template
	       "%cmd %project %format query"
	       (list
	        (cons "cmd" org-babel-bigquery-command)
	        (cons "project" (if project (format "--project_id %s" project) ""))
	        (cons "format" (format "--format %s" (if format format "pretty")))
          ))
	      ;; body of the code block
	      (org-babel-expand-body:bigquery body params)))
      (org-babel-result-cond result-params
        (buffer-string)
        (if (equal (point-min) (point-max))
	          ""
          (delete-matching-lines "^[+]" (point-min) (point-max))
          (if (org-table-p)
              (org-babel-bigquery-table-or-scalar
	             (org-babel-bigquery-offset-colnames
	              (org-table-to-lisp) headers-p))
            (buffer-string))
          )
      ))))

(defun org-babel-bigquery-table-or-scalar (result)
  "If RESULT looks like a trivial table, then unwrap it."
  (if (and (equal 1 (length result))
	         (equal 1 (length (car result))))
      (org-babel-read (caar result) t)
    (mapcar (lambda (row)
	            (if (eq 'hline row)
		              'hline
		            (mapcar #'org-babel-bigquery--read-cell row)))
	          result)))

(defun org-babel-bigquery-offset-colnames (table headers-p)
  "If HEADERS-P is non-nil then offset the first row as column names."
  (if headers-p
      (cons (car table) (cons 'hline (cdr table)))
    table))

(defun org-babel-prep-session:bigquery (_session _params)
  "Raise an error because support for BigQuery sessions isn't implemented.
Prepare SESSION according to the header arguments specified in PARAMS."
  (error "BigQuery sessions not yet implemented"))

(defun org-babel-bigquery--read-cell (cell)
  "Process CELL to remove unnecessary characters."
  (org-babel-read cell t))

(provide 'ob-bigquery)

;;; ob-bigquery.el ends here
