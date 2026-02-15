(defpackage :openweathermap/tests
  (:use :cl :fiveam)
  (:import-from :openweathermap
                :*api-key*
                :*max-retries*
                :*retry-backoff-seconds*
                :configure-api-key
                :build-onecall-url
                :build-timemachine-url
                :build-day-summary-url
                :build-overview-url
                :build-current-weather-url
                :build-forecast-url
                :make-client-weather-request
                :make-current-weather-request
                :make-forecast-request
                :make-timemachine-request
                :fetch-onecall
                :fetch-current-weather
                :fetch-forecast
                :api-request-error
                :api-network-error
                :invalid-parameters-error
                :with-http-get-function)
  (:export :run-tests))

(in-package :openweathermap/tests)

(def-suite openweathermap-suite
  :description "Unit tests for openweathermap")

(in-suite openweathermap-suite)
