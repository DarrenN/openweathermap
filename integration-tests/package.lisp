(defpackage :openweathermap-onecall/integration-tests
  (:use :cl :fiveam)
  (:import-from :openweathermap-onecall
                :*api-key*
                :build-onecall-url)
  (:export :run-integration-tests))

(in-package :openweathermap-onecall/integration-tests)

(def-suite openweathermap-onecall-integration-suite
  :description "Live API integration tests for openweathermap-onecall")

(in-suite openweathermap-onecall-integration-suite)
