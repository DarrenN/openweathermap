(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(configure-api-key "demo-key")

(format t "~&Current URL: ~A~%"
        (build-current-weather-url :q "London" :units :metric :lang "en"))
(format t "Forecast URL: ~A~%"
        (build-forecast-url :lat 35.0 :lon 139.0 :cnt 8 :units :metric))
(format t "OneCall URL: ~A~%"
        (build-onecall-url 35.0 139.0 :units :metric :exclude "minutely,alerts"))
(format t "Geocoding URL: ~A~%"
        (build-geocoding-url "Paris" :limit 3))
(format t "Maps Tile URL: ~A~%"
        (build-weather-tile-url :temp_new 3 4 5 :opacity 0.7))
