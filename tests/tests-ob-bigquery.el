;;; ob-bigquery-tests.el --- Tests for ob-bigquery   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Luis Hernanz

;; Author: Luis Miguel Hernanz
;; Keywords: lisp
;; Package-Requires: ((emacs "29.1") (org "9.7") (buttercup))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for ob-bigquery

;;; Code:

(require 'buttercup)
(require 'rx)
(require 'ob-bigquery)

(describe "ob-bigquery"
  (describe "Field quoting."
    (it "Should quote strings"
      (expect (ob-bigquery--quote-field "test") :to-equal "\"test\"")
      (expect (ob-bigquery--quote-field "10 test") :to-equal "\"10 test\""))
    (it "It should not quote numbers"
      (expect (ob-bigquery--quote-field "0") :to-equal "0")
      (expect (ob-bigquery--quote-field "1") :to-equal "1")
      (expect (ob-bigquery--quote-field "-10") :to-equal "-10")
      (expect (ob-bigquery--quote-field "-10.01") :to-equal "-10.01")
      (expect (ob-bigquery--quote-field "10.0") :to-equal "10.0")))
  (describe "Cell quoting"
    (it "Should escape vertical lines"
      (expect (ob-bigquery--read-cell "a|b") :to-equal "a\\vert{}b")
      (expect (ob-bigquery--read-cell "a | b") :to-equal "a \\vert{} b")
      (expect (ob-bigquery--read-cell "a || b") :to-equal "a \\vert{}\\vert{} b")
      )
    )
  (describe "Params are expanded"
    (it "Should not quote numbers or literals"
      (expect (ob-bigquery--expand-parameter "$i" "i" 10) :to-equal "10")
      (expect (ob-bigquery--expand-parameter "$i" "i" 10.1) :to-equal "10.1")
      (expect (ob-bigquery--expand-parameter "$$i" "i" "value") :to-equal "value"))
    (it "Should quote strings"
      (expect (ob-bigquery--expand-parameter "$i" "i" "value") :to-equal "\"value\""))
    (it "Should convert lists and and single row tables into csv"
      (expect (ob-bigquery--expand-parameter "$i" "i" '("value" "value2")) :to-equal "\"value\",\"value2\"")
      )
    )
  (describe "Command building"
    (it "Should build command with default parameters"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx (regexp (regexp-quote ob-bigquery-base-command))
                              (* space) "--format=csv"
                              (* space) "query"
                              (* space) "--max_rows=100"
                              (* space) "--batch=true"))))
    (it "Should include project ID when specified"
      (let ((params '((:project . "my-project")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx (regexp (regexp-quote ob-bigquery-base-command))
                              (* space) "--project_id=my-project"
                              (* space) "--format=csv"
                              (* space) "query"
                              (* space) "--max_rows=100"
                              (* space) "--batch=true"))))
    (it "Should include timeout when specified and non-zero"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "5000"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx (regexp (regexp-quote ob-bigquery-base-command))
                              (* space) "--format=csv"
                              (* space) "query"
                              (* space) "--max_rows=100"
                              (* space) "--job_timeout_ms=5000"
                              (* space) "--batch=true"))))
    (it "Should not include timeout when zero"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :not :to-match (rx "--job_timeout_ms"))))
    (it "Should not include timeout when nil"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100"))))
        (expect (ob-bigquery--build-command params)
                :not :to-match (rx "--job_timeout_ms"))))
    (it "Should use pretty format when specified"
      (let ((params '((:batch . "true")
                      (:format . "pretty")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--format=pretty"))))
    (it "Should use different batch values"
      (let ((params '((:batch . "false")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--batch=false"))))
    (it "Should use different maxrows values"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "500")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--max_rows=500"))))
    (it "Should combine project ID and timeout"
      (let ((params '((:project . "test-project")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "10000"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--project_id=test-project"
                              (* space) "--format=csv"
                              (* space) "query"
                              (* space) "--max_rows=100"
                              (* space) "--job_timeout_ms=10000"
                              (* space) "--batch=true"))))
    (it "Should not include project when not specified"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :not :to-match (rx "--project_id"))))
    (it "Should handle nil batch parameter"
      (let ((params '((:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--batch="))))
    (it "Should handle nil format parameter"
      (let ((params '((:batch . "true")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--format="))))
    (it "Should handle nil maxrows parameter"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--max_rows="))))
    (it "Should handle empty string project"
      (let ((params '((:project . "")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--project_id="))))
    (it "Should handle empty string timeout"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . ""))))
        (expect (ob-bigquery--build-command params)
                :not :to-match (rx "--job_timeout_ms"))))
    (it "Should handle project ID with special characters"
      (let ((params '((:project . "my-project-123")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--project_id=my-project-123"))))
    (it "Should handle very large timeout values"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "999999999"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--job_timeout_ms=999999999"))))
    (it "Should handle very large maxrows values"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "999999")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--max_rows=999999"))))
    (it "Should include base command"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx (regexp (regexp-quote ob-bigquery-base-command))))))
    (it "Should include 'query' keyword in correct position"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--format=csv"
                              (* space) "query"
                              (* space) "--max_rows"))))
    (it "Should have correct command structure order"
      (let ((params '((:project . "test-project")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "5000"))))
        (let ((cmd (ob-bigquery--build-command params)))
          (expect cmd :to-match (rx (literal ob-bigquery-base-command)))
          (expect cmd :to-match (rx "--project_id=test-project"))
          (expect cmd :to-match (rx "--format=csv"))
          (expect cmd :to-match (rx "query"))
          (expect cmd :to-match (rx "--max_rows=100"))
          (expect cmd :to-match (rx "--job_timeout_ms=5000"))
          (expect cmd :to-match (rx "--batch=true")))))
    (it "Should handle minimal parameter set"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100"))))
        (let ((cmd (ob-bigquery--build-command params)))
          (expect cmd :to-match (rx (literal ob-bigquery-base-command)))
          (expect cmd :to-match (rx "--format=csv"))
          (expect cmd :to-match (rx "--max_rows=100"))
          (expect cmd :to-match (rx "--batch=true"))
          (expect cmd :not :to-match (rx "--project_id"))
          (expect cmd :not :to-match (rx "--job_timeout_ms")))))
    (it "Should handle zero maxrows"
      (let ((params '((:batch . "true")
                      (:format . "csv")
                      (:maxrows . "0")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--max_rows=0"))))
    (it "Should handle empty string values for required parameters"
      (let ((params '((:batch . "")
                      (:format . "")
                      (:maxrows . "")
                      (:job-timeout . "0"))))
        (let ((cmd (ob-bigquery--build-command params)))
          (expect cmd :to-match (rx "--batch="))
          (expect cmd :to-match (rx "--format="))
          (expect cmd :to-match (rx "--max_rows=")))))
    (it "Should handle project ID with underscores"
      (let ((params '((:project . "my_project_123")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--project_id=my_project_123"))))
    (it "Should handle long project ID"
      (let ((params '((:project . "very-long-project-name-with-many-characters-123456")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "0"))))
        (expect (ob-bigquery--build-command params)
                :to-match (rx "--project_id=very-long-project-name-with-many-characters-123456"))))
    (it "Should produce valid command string format"
      (let ((params '((:project . "test-project")
                      (:batch . "true")
                      (:format . "csv")
                      (:maxrows . "100")
                      (:job-timeout . "5000"))))
        (let ((cmd (ob-bigquery--build-command params)))
          (expect (stringp cmd) :to-be-truthy)
          (expect (> (length cmd) 0) :to-be-truthy)
          (expect cmd :to-match (rx bol (regexp (regexp-quote ob-bigquery-base-command))))))))
  (it "Should handle timeout string '0' explicitly"
    (let ((params '((:batch . "true")
                    (:format . "csv")
                    (:maxrows . "100")
                    (:job-timeout . "0"))))
      (let ((cmd (ob-bigquery--build-command params)))
        (expect cmd :not :to-match (rx "--job_timeout_ms"))
        (expect cmd :to-match (rx "--batch=true")))))
  )


;;; ob-bigquery-tests.el ends here
