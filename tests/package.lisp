(defpackage :openweathermap-onecall/tests
  (:use :cl :fiveam)
  (:import-from :openweathermap-onecall
                :*api-key*
                :*max-retries*
                :*retry-backoff-seconds*
                :configure-api-key
                :build-onecall-url
                :build-timemachine-url
                :build-day-summary-url
                :build-overview-url
                :make-client-weather-request
                :make-timemachine-request
                :fetch-onecall
                :api-request-error
                :api-network-error
                :with-http-get-function)
  (:export :run-tests))

(in-package :openweathermap-onecall/tests)

(def-suite openweathermap-onecall-suite
  :description "Unit tests for openweathermap-onecall")

(in-suite openweathermap-onecall-suite)
