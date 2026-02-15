(in-package :openweathermap)

(defun build-weather-tile-url (layer z x y &rest query-params)
  "Build URL for weather map tile endpoint."
  (let* ((api-key (ensure-api-key))
         (merged-params (append (list :appid api-key) query-params))
         (query-string (%query-string-from-plist merged-params)))
    (format nil "~A/map/~A/~A/~A/~A.png?~A"
            *maps-base-url*
            (string-downcase (etypecase layer
                               (keyword (subseq (string layer) 1))
                               (string layer)
                               (symbol (symbol-name layer))))
            z
            x
            y
            query-string)))

(defun make-weather-tile-request (layer z x y &rest query-params)
  (list :method :get
        :url (apply #'build-weather-tile-url layer z x y query-params)))

(defun fetch-weather-tile (layer z x y &rest query-params)
  "Fetch raw tile payload bytes for a weather map tile endpoint."
  (%fetch-raw (apply #'build-weather-tile-url layer z x y query-params)
              :weather-tile))
