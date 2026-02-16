;; SPDX-License-Identifier: MIT
(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(defun %normalize-key-name (key)
  (string-downcase
   (etypecase key
     (keyword (symbol-name key))
     (string key)
     (symbol (symbol-name key)))))

(defun %plist-value (plist key)
  (let ((target (%normalize-key-name key)))
    (loop for (k v) on plist by #'cddr
          when (string= target (%normalize-key-name k))
            do (return v))))

(let ((api-key (uiop:getenv "OPENWEATHER_API_KEY")))
  (unless (and api-key (> (length api-key) 0))
    (error "Set OPENWEATHER_API_KEY before running this example."))
  (configure-api-key api-key))

(let* ((current (fetch-current-weather :q "London" :units :metric :lang "en"))
       (weather-list (%plist-value current :weather)))
  (unless (and (listp weather-list) weather-list)
    (error "Current weather response did not include a non-empty weather list. Top-level keys: ~S"
           (loop for (k _) on current by #'cddr collect k)))
  (let* ((primary (first weather-list))
       (enriched (enrich-weather-entry primary)))
    (format t "~&Current weather primary entry: ~S~%~%" primary)
    (format t "Enriched primary entry: ~S~%" enriched)))
