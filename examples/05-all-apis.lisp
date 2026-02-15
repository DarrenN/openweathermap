;; SPDX-License-Identifier: MIT
(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(defun %value (plist key)
  (or (getf plist key)
      (getf plist (intern (string-upcase (string key)) "KEYWORD"))))

(let ((api-key (uiop:getenv "OPENWEATHER_API_KEY")))
  (unless (and api-key (> (length api-key) 0))
    (error "Set OPENWEATHER_API_KEY before running this example."))
  (configure-api-key api-key))

(format t "~&Running all API examples...~%")

(let ((current (fetch-current-weather :q "London" :units :metric :lang "en")))
  (format t "Current: city=~A~%" (%value current :name)))

(let ((forecast (fetch-forecast :q "London" :units :metric :cnt 2)))
  (format t "Forecast: entries=~A~%"
          (length (%value forecast :list))))

(let* ((geocoding (fetch-geocoding "New York" :limit 1))
       (first-item (first geocoding)))
  (format t "Geocoding: name=~A lat=~A lon=~A~%"
          (%value first-item :name)
          (%value first-item :lat)
          (%value first-item :lon)))

(let ((air (fetch-air-pollution 40.7128 -74.0060)))
  (format t "Air Pollution: list-count=~A~%"
          (length (%value air :list))))

(let ((onecall (fetch-onecall 40.7128 -74.0060 :units :metric :exclude "minutely,alerts")))
  (format t "OneCall: timezone=~A~%" (%value onecall :timezone)))

(let ((tile (fetch-weather-tile :temp_new 3 2 3 :opacity 0.9)))
  (format t "Maps: tile-bytes/chars=~A~%" (length tile)))

(format t "~&Done.~%")
