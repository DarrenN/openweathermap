(in-package :openweathermap/integration-tests)

(defun %env-true-p (value)
  (and value
       (member (string-downcase value) '("1" "true" "yes") :test #'string=)))

(defun ensure-live-integration-api-key ()
  "Return live API key or skip integration test when prerequisites are not met."
  (let ((key (uiop:getenv "OPENWEATHER_API_KEY"))
        (enabled (uiop:getenv "OPENWEATHERMAP_RUN_LIVE_TESTS")))
    (cond
      ((or (null key) (string= key ""))
       (skip "Set OPENWEATHER_API_KEY to run live integration tests.")
       nil)
      ((not (%env-true-p enabled))
       (skip "Set OPENWEATHERMAP_RUN_LIVE_TESTS=1 to enable live integration tests.")
       nil)
      (t
       (setf openweathermap:*api-key* key)
       key))))
