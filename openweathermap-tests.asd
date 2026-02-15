(asdf:defsystem "openweathermap-tests"
  :description "Unit tests for openweathermap"
  :author "OpenWeatherMap API Client Contributors"
  :license "MIT"
  :depends-on ("openweathermap" "fiveam")
  :serial t
  :components
  ((:file "tests/package")
   (:file "tests/config-test")
   (:file "tests/client-test")
   (:file "tests/test-runner"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :openweathermap/tests :run-tests)))
