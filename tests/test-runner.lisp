;; SPDX-License-Identifier: MIT
(in-package :openweathermap/tests)

(defun run-tests ()
  (let ((result (run 'openweathermap-suite)))
    (unless (results-status result)
      (error "Unit test suite failed."))
    result))
