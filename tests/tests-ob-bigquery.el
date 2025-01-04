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
  )

(provide 'ob-bigquery-tests)
;;; ob-bigquery-tests.el ends here
