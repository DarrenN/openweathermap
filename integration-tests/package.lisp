(defpackage :openweathermap/integration-tests
  (:use :cl :fiveam)
  (:import-from :openweathermap
                :*api-key*
                :build-onecall-url)
  (:export :run-integration-tests))

(in-package :openweathermap/integration-tests)

(def-suite openweathermap-integration-suite
  :description "Live API integration tests for openweathermap")

(in-suite openweathermap-integration-suite)
