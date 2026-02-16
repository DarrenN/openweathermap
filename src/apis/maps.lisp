;; SPDX-License-Identifier: MIT
(in-package :openweathermap)

(defparameter *supported-weather-map-layers*
  '("clouds_new"
    "precipitation_new"
    "pressure_new"
    "temp_new"
    "temperature_new"
    "wind_new")
  "Documented OpenWeather weather map layers supported by this client.")

(defun %normalize-layer-name (layer)
  "Normalize tile LAYER to lowercase string form and validate non-empty."
  (let ((name (string-downcase
               (etypecase layer
                 (keyword (symbol-name layer))
                 (string layer)
                 (symbol (symbol-name layer))))))
    (unless (> (length name) 0)
      (error 'invalid-parameters-error
             :message "Weather tile layer must be a non-empty string/symbol."))
    (unless (member name *supported-weather-map-layers* :test #'string=)
      (error 'invalid-parameters-error
             :message (format nil "Unsupported weather tile layer '~A'. Supported layers: ~{~A~^, ~}."
                              name
                              *supported-weather-map-layers*)))
    name))

(defun %ensure-non-negative-integer (value field-name)
  "Validate VALUE is a non-negative integer for FIELD-NAME."
  (unless (and (integerp value) (>= value 0))
    (error 'invalid-parameters-error
           :message (format nil "~A must be a non-negative integer." field-name))))

(defun build-weather-tile-url (layer z x y &rest query-params)
  "Build URL for weather map tile endpoint."
  (%ensure-non-negative-integer z "z")
  (%ensure-non-negative-integer x "x")
  (%ensure-non-negative-integer y "y")
  (let* ((api-key (ensure-api-key))
         (layer-name (%normalize-layer-name layer))
         (merged-params (append (list :appid api-key) query-params))
         (query-string (%query-string-from-plist merged-params)))
    (format nil "~A/map/~A/~A/~A/~A.png?~A"
            *maps-base-url*
            layer-name
            z
            x
            y
            query-string)))

(defun make-weather-tile-request (layer z x y &rest query-params)
  "Build request descriptor plist for weather tile endpoint."
  (list :method :get
        :url (apply #'build-weather-tile-url layer z x y query-params)))

(defun fetch-weather-tile (layer z x y &rest query-params)
  "Fetch raw tile payload bytes for a weather map tile endpoint."
  (%execute-raw-request (apply #'make-weather-tile-request layer z x y query-params)
                        :weather-tile))
