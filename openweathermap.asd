(asdf:defsystem "openweathermap"
  :description "Common Lisp client for OpenWeatherMap APIs"
  :author "OpenWeatherMap API Client Contributors"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on ("dexador" "jonathan")
  :components
  ((:file "src/package")
   (:file "src/errors")
   (:file "src/config")
   (:file "src/client")
   (:file "src/apis/current")
   (:file "src/apis/forecast")
   (:file "src/apis/geocoding")
   (:file "src/apis/air-pollution")
   (:file "src/apis/maps")
   (:file "src/openweathermap")))
