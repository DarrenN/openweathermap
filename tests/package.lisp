(defpackage :openweathermap-onecall/tests
  (:use :cl :fiveam)
  (:import-from :openweathermap-onecall
                :*api-key*
                :configure-api-key
                :build-onecall-url
                :make-client-weather-request)
  (:export :run-tests))

(in-package :openweathermap-onecall/tests)

(def-suite openweathermap-onecall-suite
  :description "Unit tests for openweathermap-onecall")

(in-suite openweathermap-onecall-suite)
