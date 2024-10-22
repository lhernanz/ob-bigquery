;;; ob-bigquery.el --- Babel Functions for BigQuery Databases -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Free Software Foundation, Inc.

;; Author: Luis Miguel Hernanz
;; Maintainer:
;; Keywords: literate programming, reproducible research
;; Package-Version: 20240903.93446
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

(add-to-list 'org-src-lang-modes  '("bigquery" . sql))

(defvar org-babel-bigquery-base-command "bq --headless -sync"
  "Command to invoke the bq command line utility.")

(defvar org-babel-bigquery-number-regexp "^-?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)$"
  "Regexp that will be used to identify numbers that need to be preserved as such (vs strings) in the query.")


(defvar org-babel-default-header-args:bigquery '(
                                                 (:format . "csv")
                                                 (:maxrows . "100")
                                                 (:headers-p . "yes")
                                                 ))

(defvar org-babel-header-args:bigquery
  '((project   . :any)
    (format    . ("csv" "pretty"))
    (maxrows   . :any)
    (headers-p . ("yes" "no"))
    )
  "Bigquery specific header args.")


(defun org-babel-quote-list-field:bigquery (s)
  "Quote field for inclusion in a bigquery statement. `s' is the
field to quote. If the element is not a number, it will be
quoted. The function supports quoting strings that already have
quotes."
  (cond
   ((string-match org-babel-bigquery-number-regexp s) s) ;; Any number
   (t (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")))
  )

(defun org-babel-expand-vars:bigquery (body vars)
  "Expand the variables held in VARS in BODY. Double quoted
variables (e.g. `$$var') values are preserved as such. String are
quoted, list and horizontal tables are converted into a list of
comma separated values and their values quoted if they are
strings. Everything else is printed via `prin1'."
  (mapc
   (lambda (pair)
     (let ((name (car pair))
           (val (cdr pair)))
       (setq body
             (thread-last
               (replace-regexp-in-string (format "$$%s\\b" name) (format "%s" val) body)
               (replace-regexp-in-string (format "$%s\\b" name)
                                         (cond
                                          ((listp val)
                                           (orgtbl-to-generic
                                            (if (listp (car val))
                                                val
                                              (list val)) ;; Wrap simple lists to be handled as tables
                                            '(:sep "," :fmt org-babel-quote-list-field:bigquery)))
                                          (t (format "%S" val))))
               ))))
   vars)
  body)


(defun org-babel-expand-body:bigquery (body params)
  "Expand BODY according to the values of PARAMS."
  (org-babel-expand-vars:bigquery
   body (org-babel--get-vars params)))

(defun org-babel-execute:bigquery (body params)
  "Execute a block of Bigquery code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* (
         (processed-params (org-babel-process-params params))
         (result-params (split-string (or (cdr (assq :results processed-params)) "")))
         (project (cdr (assq :project processed-params)))
         (format (cdr (assq :format processed-params)))
         (maxrows (cdr (assq :maxrows processed-params)))
         (headers-p (cdr (assq :headers-p processed-params)))
         (command (org-fill-template
                   "%cmd %project %format query %maxrows"
                   (list
                    (cons "cmd" org-babel-bigquery-base-command)
                    (cons "project" (if project (format "--project_id %s" project) ""))
                    (cons "format" (format "--format %s" format))
                    (cons "maxrows" (format "--max_rows %s" maxrows))
                    )))
         (error-code 0)
         (table-value)
         )
    (defun ob--register-error (exit-code stderr)
      "Internal function to identify when the command returned an error
by advising the relevant error hook. Org does not support any
other mechanism to get this information"
      (setq error-code exit-code))
    (advice-add 'org-babel-eval-error-notify :before #'ob--register-error)
    (with-temp-buffer
      (insert
       (org-babel-eval
        command
        ;; body of the code block
        (org-babel-expand-body:bigquery body processed-params)))
      (advice-remove 'org-babel-eval-error-notify #'ob--register-error)
      (setq table-value
            (cond
             ;; Error conditions, no output transformation
             ((> error-code 0) (buffer-string))
             ((equal (point-min) (point-max)) "")
             ;; Transform the output according to mode and convert to table
             (t
              (when (equal format "pretty")
                ;; Pretty format has a line after headers that confuses org. Removing that line
                (delete-matching-lines "^[+]" (point-min) (point-max))
                )
              (when (equal format "csv")
                ;; Escape pipes or org will get confused about them
                (replace-string "|" "\\vert{}" nil  (point-min) (point-max))
                (org-table-convert-region (point-min) (point-max) '(4))
                )
              (if (org-at-table-p)
                  (org-babel-bigquery-table-or-scalar
                   (org-babel-bigquery-offset-colnames
                    (org-table-to-lisp) headers-p))
                (buffer-string))
              )
             ))
      (org-babel-result-cond result-params
        (buffer-string) table-value
        )
      )))

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
  (org-babel-read (org-quote-vert cell) t))

(provide 'ob-bigquery)

;;; ob-bigquery.el ends here
