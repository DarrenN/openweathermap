(in-package :openweathermap-onecall/tests)

(defun run-tests ()
  (let ((result (run 'openweathermap-onecall-suite)))
    (unless (results-status result)
      (error "Unit test suite failed."))
    result))
