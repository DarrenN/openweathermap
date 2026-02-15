(asdf:defsystem "openweathermap-onecall-tests"
  :description "Unit tests for openweathermap-onecall"
  :author "OpenWeatherMap OneCall Contributors"
  :license "MIT"
  :depends-on ("openweathermap-onecall" "fiveam")
  :serial t
  :components
  ((:file "tests/package")
   (:file "tests/config-test")
   (:file "tests/client-test")
   (:file "tests/test-runner"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :openweathermap-onecall/tests :run-tests)))
