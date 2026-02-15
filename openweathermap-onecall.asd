(asdf:defsystem "openweathermap-onecall"
  :description "Common Lisp client for OpenWeatherMap One Call 3.0 API"
  :author "OpenWeatherMap OneCall Contributors"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on ("dexador" "jonathan")
  :components
  ((:file "src/package")
   (:file "src/errors")
   (:file "src/config")
   (:file "src/client")
   (:file "src/openweathermap-onecall")))
