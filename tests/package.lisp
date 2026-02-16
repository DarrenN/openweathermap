;; SPDX-License-Identifier: MIT
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
                :build-geocoding-url
                :build-reverse-geocoding-url
                :build-zip-geocoding-url
                :build-air-pollution-url
                :build-air-pollution-forecast-url
                :build-air-pollution-history-url
                :build-weather-tile-url
                :make-client-weather-request
                :make-onecall-request
                :make-current-weather-request
                :make-forecast-request
                :make-geocoding-request
                :make-air-pollution-request
                :make-air-pollution-history-request
                :make-weather-tile-request
                :make-timemachine-request
                :fetch-onecall
                :fetch-current-weather
                :fetch-forecast
                :fetch-geocoding
                :fetch-reverse-geocoding
                :fetch-zip-geocoding
                :fetch-air-pollution
                :fetch-air-pollution-forecast
                :fetch-air-pollution-history
                :fetch-weather-tile
                :api-request-error
                :api-request-error-status-code
                :api-request-error-message
                :api-request-error-endpoint
                :api-network-error
                :invalid-parameters-error
                :invalid-parameters-error-message
                :with-http-get-function)
  (:export :run-tests))

(in-package :openweathermap/tests)

(def-suite openweathermap-suite
  :description "Unit tests for openweathermap")

(in-suite openweathermap-suite)
