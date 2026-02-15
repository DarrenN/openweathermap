(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(let ((api-key (uiop:getenv "OPENWEATHER_API_KEY")))
  (unless (and api-key (> (length api-key) 0))
    (error "Set OPENWEATHER_API_KEY before running this example."))
  (configure-api-key api-key))

(let* ((tile (fetch-weather-tile :temp_new 3 4 5 :opacity 0.9))
       (byte-count (length tile)))
  (format t "~&Fetched tile payload bytes/chars: ~A~%" byte-count))
