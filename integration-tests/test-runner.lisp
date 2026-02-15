(in-package :openweathermap-onecall/integration-tests)

(defun run-integration-tests ()
  (let ((result (run 'openweathermap-onecall-integration-suite)))
    (unless (results-status result)
      (error "Integration test suite failed."))
    result))
