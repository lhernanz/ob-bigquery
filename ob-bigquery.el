;;; ob-bigquery.el --- Babel Functions for BigQuery Databases -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Luis Miguel Hernanz

;; Author: Luis Miguel Hernanz
;; Keywords: lisp
;; Package-Version: 20240903.93446
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;; URL: https://www.github.com/lhernanz/ob-bigquery

;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'org-table)
(require 'subr-x) ;; For thread-last

(add-to-list 'org-src-lang-modes  '("bigquery" . sql))

;;; Variables
(defgroup ob-bigquery nil
  "Settings for BigQuery integration with org-babel."
  :group 'org-babel
  :prefix "ob-bigquery-")

(defcustom ob-bigquery-base-command "bq --headless -sync"
  "Command to use to invoke the BQ command line utility."
  :type 'string
  :group 'ob-bigquery)

(defcustom ob-bigquery-number-regexp "^-?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)$"
  "Regexp to identify numbers that don't need to be quoted."
  :type 'regexp
  :group 'ob-bigquery)

;;; Babel related variables
(defcustom org-babel-default-header-args:bigquery
  '((:format . "csv")
    (:maxrows . "100")
    (:headers-p . "yes"))
  "Default parameters that will be used when invoking the BQ command.
These will be added to `ob-bigquery-base-command'.  Notice that
the pretty format might not handle values that need to be quoted
in the right way.  Use with caution."
  :type '(alist :key-type symbol :value-type string)
  :group 'ob-bigquery)

(defcustom org-babel-header-args:bigquery
  '((project   . :any)
    (format    . ("csv" "pretty"))
    (maxrows   . :any)
    (headers-p . ("yes" "no")))
  "BigQuery specific header arguments."
  :type '(alist :key-type symbol :value-type (choice (const :tag "Any" :any)
                                                     (repeat :tag "Options" string)))
  :group 'ob-bigquery)

;;; Internal methods
(defun ob-bigquery--quote-field (s)
  "Quote field for inclusion in a BigQuery statement.
S is the field to quote.  If the element is not a number (as
defined by `ob-bigquery-number-regexp', it will be quoted.  The
function supports quoting strings that already have quotes."
  (cond
   ((string-match ob-bigquery-number-regexp s) s) ;; Any number
   (t (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\""))))

(defun ob-bigquery--table-or-scalar (result)
  "If RESULT is a single element table, then unwrap it.
Process cell contents by using `org-babel-read'."
  (if (and (equal 1 (length result))
           (equal 1 (length (car result))))
      (org-babel-read (caar result) t)
    (mapcar (lambda (row)
              (if (eq 'hline row)
                  'hline
                (mapcar #'ob-bigquery--read-cell row)))
            result)))

(defun ob-bigquery--quote-vert (s)
  "Replace \"|\" with \"\\vert{[]}\" in the string S."
  (while (string-match "|" s)
    (setq s (replace-match "\\vert{}" t t s)))
  s)

(defun ob-bigquery--read-cell (cell)
  "Process CELL to remove unnecessary characters."
  (org-babel-read (ob-bigquery--quote-vert cell) t))

(defun ob-bigquery--offset-colnames (table headers-p)
  "If HEADERS-P is non-nil then offset the first row as column names in TABLE."
  (if headers-p
      (cons (car table) (cons 'hline (cdr table)))
    table))

(defun ob-bigquery--expand-parameter (body name value)
  "Expand the NAME parameter to its VALUE in BODY.
Double quoted variables (e.g. `$$var') values are preserved as
such.  String are quoted, list and horizontal tables are converted
into a list of comma separated values and their values quoted if
they are strings.  Everything else is printed via `prin1'."
  (thread-last
    (replace-regexp-in-string (format "$$%s\\b" name) (format "%s" value) body)
    (replace-regexp-in-string (format "$%s\\b" name)
                              (cond
                               ((listp value)
                                (orgtbl-to-generic
                                 (if (listp (car value))
                                     value
                                   (list value)) ;; Wrap simple lists to be handled as tables
                                 '(:sep "," :fmt ob-bigquery--quote-field)))
                               (t (format "%S" value))))))


;;; Babel Interface implementation

(defun org-babel-expand-body:bigquery (body params &optional processed-params)
  "Expand BODY according to the values of PROCESSED-PARAMS (if provided) or PARAMS.
See `ob-bigquery--expand-parameter' for the types of expansion supported."
  (let ((vars (org-babel--get-vars (or processed-params
                                       (org-babel-process-params params)))))
    (mapc
     (lambda (pair)
       (let ((name (car pair))
             (val (cdr pair)))
         (setq body (ob-bigquery--expand-parameter body name val))))
     vars)
    body))

;; This is needed for the compiler to be able to find the register function
;; See https://emacs.stackexchange.com/questions/29853/defun-inside-let-with-lexical-binding-gives-byte-compile-warning-the-function-i
(declare-function ob-bigquery--register-error "ob-bigquery.el")

;;;###autoload
(defun org-babel-execute:bigquery (body params)
  "Execute a BODY of BigQuery code with Babel using PARAMS.
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
                    (cons "cmd" ob-bigquery-base-command)
                    (cons "project" (if project (format "--project_id %s" project) ""))
                    (cons "format" (format "--format %s" format))
                    (cons "maxrows" (format "--max_rows %s" maxrows)))))
         (error-code 0)
         (table-value))

    (defun ob-bigquery--register-error (exit-code stderr)
      "Internal function to identify when the command returned an error
by advising the relevant error hook. Org does not support any
other mechanism to get this information."
      (setq error-code exit-code)
      (if stderr
          (message "Error running BQ: %s" stderr)))

    (advice-add 'org-babel-eval-error-notify :before #'ob-bigquery--register-error)
    ;; Execute command
    (with-temp-buffer
      (insert
       (org-babel-eval
        command
        (org-babel-expand-body:bigquery body params processed-params)))
      (advice-remove 'org-babel-eval-error-notify #'ob-bigquery--register-error)

      ;; Process output
      (setq table-value
            (cond
             ;; Error conditions, no output transformation
             ((> error-code 0) (buffer-string))
             ((equal (point-min) (point-max)) "")
             ;; Transform the output according to mode and convert to table
             (t
              (when (equal format "pretty")
                ;; Pretty format has a line after headers that confuses org. Removing that line
                (delete-matching-lines "^[+]" (point-min) (point-max)))
              (when (equal format "csv")
                ;; Escape pipes or org will get confused about them
                (goto-char (point-min))
                (while (search-forward "|" nil t)
                  (replace-match "\\vert{}" nil t))
                (org-table-convert-region (point-min) (point-max) '(4)))
              (if (org-at-table-p)
                  (ob-bigquery--table-or-scalar
                   (ob-bigquery--offset-colnames
                    (org-table-to-lisp) headers-p))
                (buffer-string)))))
      (org-babel-result-cond result-params
        (buffer-string) table-value))))

(defun org-babel-prep-session:bigquery (_session _params)
  "Raise an error because support for BigQuery sessions isn't implemented.
Prepare SESSION according to the header arguments specified in PARAMS."
  (error "BigQuery sessions have not been implemented yet"))

(provide 'ob-bigquery)

;;; ob-bigquery.el ends here

;; LocalWords:  PARAMS
