(in-package :openweathermap/integration-tests)

(defun %run-suite-or-fail (suite-name failure-message)
  (let ((result (run suite-name)))
    (unless (results-status result)
      (error failure-message))
    result))

(defun run-integration-tests ()
  (%run-suite-or-fail 'openweathermap-integration-suite
                      "Integration test suite failed."))

(defun run-onecall-integration-tests ()
  (%run-suite-or-fail 'onecall-integration-suite
                      "One Call integration test suite failed."))

(defun run-current-integration-tests ()
  (%run-suite-or-fail 'current-integration-suite
                      "Current Weather integration test suite failed."))

(defun run-forecast-integration-tests ()
  (%run-suite-or-fail 'forecast-integration-suite
                      "Forecast integration test suite failed."))

(defun run-geocoding-integration-tests ()
  (%run-suite-or-fail 'geocoding-integration-suite
                      "Geocoding integration test suite failed."))

(defun run-air-pollution-integration-tests ()
  (%run-suite-or-fail 'air-pollution-integration-suite
                      "Air Pollution integration test suite failed."))

(defun run-maps-integration-tests ()
  (%run-suite-or-fail 'maps-integration-suite
                      "Maps integration test suite failed."))
