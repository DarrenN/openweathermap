;; SPDX-License-Identifier: MIT
(in-package :openweathermap)

(defun %forecast-selector-provided-p (value)
  "Return true when VALUE should count as a provided selector."
  (not (null value)))

(defun %ensure-forecast-location-selector (lat lon q id zip)
  "Validate forecast location selectors for mutual exclusivity."
  (let ((has-lat (%forecast-selector-provided-p lat))
        (has-lon (%forecast-selector-provided-p lon)))
    (unless (eq has-lat has-lon)
      (error 'invalid-parameters-error
             :message "Forecast coordinates require both lat and lon together."))
    (let ((selector-count (+ (if (and has-lat has-lon) 1 0)
                             (if (%forecast-selector-provided-p q) 1 0)
                             (if (%forecast-selector-provided-p id) 1 0)
                             (if (%forecast-selector-provided-p zip) 1 0))))
      (unless (= selector-count 1)
        (error 'invalid-parameters-error
               :message "Provide exactly one forecast location selector: lat/lon, q, id, or zip.")))))

(defun %ensure-forecast-cnt (cnt)
  "Validate optional forecast CNT is an integer in the range 1..40."
  (when cnt
    (unless (and (integerp cnt) (<= 1 cnt 40))
      (error 'invalid-parameters-error
             :message "Forecast cnt must be an integer between 1 and 40."))))

(defun %compact-forecast-plist (plist)
  "Return PLIST with key/value pairs containing NIL values removed."
  (loop for (key value) on plist by #'cddr
        when value
          append (list key value)))

(defun build-forecast-url (&key lat lon q id zip units lang cnt mode)
  "Build URL for /data/2.5/forecast endpoint.

Exactly one location selector must be provided:
- lat/lon pair
- q (city name)
- id (city id)
- zip
"
  (%ensure-forecast-location-selector lat lon q id zip)
  (%ensure-forecast-cnt cnt)
  (%build-endpoint-url
   "/data/2.5/forecast"
   '()
   (%compact-forecast-plist
    (list :lat lat
          :lon lon
          :q q
          :id id
          :zip zip
          :units units
          :lang lang
          :cnt cnt
          :mode mode))))

(defun make-forecast-request (&key lat lon q id zip units lang cnt mode)
  "Build request descriptor plist for forecast endpoint."
  (list :method :get
        :url (build-forecast-url
              :lat lat :lon lon :q q :id id :zip zip
              :units units :lang lang :cnt cnt :mode mode)))

(defun fetch-forecast (&key lat lon q id zip units lang cnt mode)
  "Fetch and decode forecast response as a plist."
  (%ensure-json-mode-for-fetch mode "Forecast")
  (%execute-json-request (make-forecast-request
                          :lat lat :lon lon :q q :id id :zip zip
                          :units units :lang lang :cnt cnt :mode mode)
                         :forecast))
