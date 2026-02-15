(asdf:defsystem "openweathermap-onecall-integration-tests"
  :description "Integration tests for openweathermap-onecall against live API"
  :author "OpenWeatherMap OneCall Contributors"
  :license "MIT"
  :depends-on ("openweathermap-onecall" "fiveam")
  :serial t
  :components
  ((:file "integration-tests/package")
   (:file "integration-tests/smoke-test")
   (:file "integration-tests/test-runner"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :openweathermap-onecall/integration-tests :run-integration-tests)))
