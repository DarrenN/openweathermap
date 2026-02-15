;; SPDX-License-Identifier: MIT
(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(let ((api-key (uiop:getenv "OPENWEATHER_API_KEY")))
  (unless (and api-key (> (length api-key) 0))
    (error "Set OPENWEATHER_API_KEY before running this example."))
  (configure-api-key api-key))

(let* ((result (fetch-geocoding "New York" :limit 1))
       (first-item (first result)))
  (format t "~&Name: ~A~%"
          (or (getf first-item :name) (getf first-item :|name|)))
  (format t "Lat/Lon: ~A, ~A~%"
          (or (getf first-item :lat) (getf first-item :|lat|))
          (or (getf first-item :lon) (getf first-item :|lon|))))
