;; SPDX-License-Identifier: MIT
(asdf:defsystem "openweathermap-tests"
  :description "Unit tests for openweathermap"
  :author "DarrenN"
  :license "MIT"
  :depends-on ("openweathermap" "fiveam")
  :serial t
  :components
  ((:file "tests/package")
   (:file "tests/config-test")
   (:file "tests/current-test")
   (:file "tests/forecast-test")
   (:file "tests/geocoding-test")
   (:file "tests/air-pollution-test")
   (:file "tests/maps-test")
   (:file "tests/query-param-conformance-test")
   (:file "tests/client-test")
   (:file "tests/weather-conditions-test")
   (:file "tests/test-runner"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :openweathermap/tests :run-tests)))
