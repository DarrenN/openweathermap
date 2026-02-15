(asdf:defsystem "openweathermap-integration-tests"
  :description "Integration tests for openweathermap against live APIs"
  :author "OpenWeatherMap API Client Contributors"
  :license "MIT"
  :depends-on ("openweathermap" "fiveam")
  :serial t
  :components
  ((:file "integration-tests/package")
   (:file "integration-tests/helpers")
   (:file "integration-tests/smoke-test")
   (:file "integration-tests/test-runner"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :openweathermap/integration-tests :run-integration-tests)))
