;; SPDX-License-Identifier: MIT
(in-package :openweathermap)

(defun %current-selector-provided-p (value)
  "Return true when VALUE should count as a provided selector."
  (not (null value)))

(defun %ensure-current-weather-location-selector (lat lon q id zip)
  "Validate current-weather location selectors for mutual exclusivity."
  (let ((has-lat (%current-selector-provided-p lat))
        (has-lon (%current-selector-provided-p lon)))
    (unless (eq has-lat has-lon)
      (error 'invalid-parameters-error
             :message "Current weather coordinates require both lat and lon together."))
    (let ((selector-count (+ (if (and has-lat has-lon) 1 0)
                             (if (%current-selector-provided-p q) 1 0)
                             (if (%current-selector-provided-p id) 1 0)
                             (if (%current-selector-provided-p zip) 1 0))))
      (unless (= selector-count 1)
        (error 'invalid-parameters-error
               :message "Provide exactly one current weather location selector: lat/lon, q, id, or zip.")))))

(defun %compact-plist (plist)
  "Return PLIST with key/value pairs containing NIL values removed."
  (loop for (key value) on plist by #'cddr
        when value
          append (list key value)))

(defun build-current-weather-url (&key lat lon q id zip units lang mode)
  "Build URL for /data/2.5/weather endpoint.

Exactly one location selector must be provided:
- lat/lon pair
- q (city name)
- id (city id)
- zip
"
  (%ensure-current-weather-location-selector lat lon q id zip)
  (%build-endpoint-url
   "/data/2.5/weather"
   '()
   (%compact-plist
    (list :lat lat
          :lon lon
          :q q
          :id id
          :zip zip
          :units units
          :lang lang
          :mode mode))))

(defun make-current-weather-request (&key lat lon q id zip units lang mode)
  "Build request descriptor plist for current weather endpoint."
  (list :method :get
        :url (build-current-weather-url
              :lat lat :lon lon :q q :id id :zip zip
              :units units :lang lang :mode mode)))

(defun fetch-current-weather (&key lat lon q id zip units lang mode)
  "Fetch and decode current weather response as a plist."
  (%ensure-json-mode-for-fetch mode "Current weather")
  (%execute-json-request (make-current-weather-request
                          :lat lat :lon lon :q q :id id :zip zip
                          :units units :lang lang :mode mode)
                         :current-weather))
