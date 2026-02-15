;; SPDX-License-Identifier: MIT
(defpackage :openweathermap/integration-tests
  (:use :cl :fiveam)
  (:import-from :openweathermap
                :*api-key*
                :fetch-onecall
                :fetch-current-weather
                :fetch-forecast
                :fetch-geocoding
                :fetch-air-pollution
                :fetch-weather-tile)
  (:export
   :run-integration-tests
   :run-onecall-integration-tests
   :run-current-integration-tests
   :run-forecast-integration-tests
   :run-geocoding-integration-tests
   :run-air-pollution-integration-tests
   :run-maps-integration-tests))

(in-package :openweathermap/integration-tests)

(def-suite openweathermap-integration-suite
  :description "Live API integration tests for openweathermap")

(def-suite onecall-integration-suite
  :in openweathermap-integration-suite
  :description "Live integration tests for One Call API")

(def-suite current-integration-suite
  :in openweathermap-integration-suite
  :description "Live integration tests for Current Weather API")

(def-suite forecast-integration-suite
  :in openweathermap-integration-suite
  :description "Live integration tests for Forecast API")

(def-suite geocoding-integration-suite
  :in openweathermap-integration-suite
  :description "Live integration tests for Geocoding API")

(def-suite air-pollution-integration-suite
  :in openweathermap-integration-suite
  :description "Live integration tests for Air Pollution API")

(def-suite maps-integration-suite
  :in openweathermap-integration-suite
  :description "Live integration tests for Weather Maps API")
