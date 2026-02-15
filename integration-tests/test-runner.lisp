(in-package :openweathermap/integration-tests)

(defun run-integration-tests ()
  (let ((result (run 'openweathermap-integration-suite)))
    (unless (results-status result)
      (error "Integration test suite failed."))
    result))
