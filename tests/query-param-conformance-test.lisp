;; SPDX-License-Identifier: MIT
(in-package :openweathermap/tests)

(defun %query-alist (url)
  (let* ((qpos (position #\? url))
         (query (and qpos (subseq url (1+ qpos)))))
    (if (or (null query) (string= query ""))
        '()
        (mapcar (lambda (pair)
                  (let ((eq-pos (position #\= pair)))
                    (if eq-pos
                        (cons (subseq pair 0 eq-pos)
                              (subseq pair (1+ eq-pos)))
                        (cons pair ""))))
                (uiop:split-string query :separator '(#\&))))))

(defun %query-keys (url)
  (mapcar #'car (%query-alist url)))

(defun %query-value (url key)
  (cdr (assoc key (%query-alist url) :test #'string=)))

(defun %sorted-copy (items)
  (sort (copy-list items) #'string<))

(defun %assert-query-keys= (url expected-keys)
  (is (equal (%sorted-copy expected-keys)
             (%sorted-copy (%query-keys url)))))

(defun %assert-query-value= (url key expected-value)
  (is (string= expected-value (%query-value url key))))

(defun %assert-no-truncated-keys (url)
  (is (null (%query-value url "ppid")))
  (is (null (%query-value url "at")))
  (is (null (%query-value url "on")))
  (is (null (%query-value url "nits")))
  (is (null (%query-value url "ang"))))

(test onecall-required-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((required-url (openweathermap:build-onecall-url 35.0 139.0))
          (optional-url (openweathermap:build-onecall-url
                         35.0 139.0
                         :units :metric
                         :lang "en"
                         :exclude "minutely,alerts")))
      (%assert-query-keys= required-url '("lat" "lon" "appid"))
      (%assert-query-keys= optional-url '("lat" "lon" "appid" "units" "lang" "exclude"))
      (%assert-query-value= required-url "appid" "test-key")
      (%assert-query-value= optional-url "exclude" "minutely%2Calerts")
      (%assert-no-truncated-keys optional-url))))

(test timemachine-required-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((required-url (openweathermap:build-timemachine-url 35.0 139.0 1700000000))
          (optional-url (openweathermap:build-timemachine-url
                         35.0 139.0 1700000000
                         :units :metric
                         :lang "en")))
      (%assert-query-keys= required-url '("lat" "lon" "dt" "appid"))
      (%assert-query-keys= optional-url '("lat" "lon" "dt" "appid" "units" "lang"))
      (is (string= "1700000000" (%query-value required-url "dt")))
      (%assert-no-truncated-keys optional-url))))

(test day-summary-required-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((required-url (openweathermap:build-day-summary-url 35.0 139.0 "2026-02-15"))
          (optional-url (openweathermap:build-day-summary-url
                         35.0 139.0 "2026-02-15"
                         :tz "+09:00"
                         :units :metric
                         :lang "ja")))
      (%assert-query-keys= required-url '("lat" "lon" "date" "appid"))
      (%assert-query-keys= optional-url '("lat" "lon" "date" "appid" "tz" "units" "lang"))
      (%assert-query-value= required-url "date" "2026-02-15")
      (%assert-query-value= optional-url "tz" "%2B09%3A00")
      (%assert-no-truncated-keys optional-url))))

(test overview-required-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((required-url (openweathermap:build-overview-url 35.0 139.0))
          (optional-url (openweathermap:build-overview-url 35.0 139.0 :date "2026-02-16" :units :imperial)))
      (%assert-query-keys= required-url '("lat" "lon" "appid"))
      (%assert-query-keys= optional-url '("lat" "lon" "appid" "date" "units"))
      (%assert-query-value= optional-url "date" "2026-02-16")
      (%assert-no-truncated-keys optional-url))))

(test current-required-location-variants-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((q-url (openweathermap:build-current-weather-url :q "London"))
          (coord-url (openweathermap:build-current-weather-url :lat 35.0 :lon 139.0))
          (id-url (openweathermap:build-current-weather-url :id 524901))
          (zip-url (openweathermap:build-current-weather-url :zip "94040,US"))
          (optional-url (openweathermap:build-current-weather-url
                         :q "London"
                         :units :metric
                         :lang "en"
                         :mode "json")))
      (%assert-query-keys= q-url '("q" "appid"))
      (%assert-query-keys= coord-url '("lat" "lon" "appid"))
      (%assert-query-keys= id-url '("id" "appid"))
      (%assert-query-keys= zip-url '("zip" "appid"))
      (%assert-query-keys= optional-url '("q" "appid" "units" "lang" "mode"))
      (%assert-no-truncated-keys optional-url))))

(test forecast-required-location-variants-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((q-url (openweathermap:build-forecast-url :q "London"))
          (coord-url (openweathermap:build-forecast-url :lat 35.0 :lon 139.0))
          (id-url (openweathermap:build-forecast-url :id 524901))
          (zip-url (openweathermap:build-forecast-url :zip "94040,US"))
          (optional-url (openweathermap:build-forecast-url
                         :q "London"
                         :units :metric
                         :lang "en"
                         :cnt 8
                         :mode "json")))
      (%assert-query-keys= q-url '("q" "appid"))
      (%assert-query-keys= coord-url '("lat" "lon" "appid"))
      (%assert-query-keys= id-url '("id" "appid"))
      (%assert-query-keys= zip-url '("zip" "appid"))
      (%assert-query-keys= optional-url '("q" "appid" "units" "lang" "cnt" "mode"))
      (%assert-no-truncated-keys optional-url))))

(test geocoding-required-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((direct-required (openweathermap:build-geocoding-url "New York"))
          (direct-optional (openweathermap:build-geocoding-url "New York" :limit 5))
          (reverse-required (openweathermap:build-reverse-geocoding-url 35.0 139.0))
          (reverse-optional (openweathermap:build-reverse-geocoding-url 35.0 139.0 :limit 3))
          (zip-required (openweathermap:build-zip-geocoding-url "94040"))
          (zip-optional (openweathermap:build-zip-geocoding-url "94040" :country-code "US")))
      (%assert-query-keys= direct-required '("q" "appid"))
      (%assert-query-keys= direct-optional '("q" "limit" "appid"))
      (%assert-query-keys= reverse-required '("lat" "lon" "appid"))
      (%assert-query-keys= reverse-optional '("lat" "lon" "limit" "appid"))
      (%assert-query-keys= zip-required '("zip" "appid"))
      (%assert-query-keys= zip-optional '("zip" "appid"))
      (%assert-query-value= direct-required "q" "New%20York")
      (%assert-query-value= zip-optional "zip" "94040%2CUS")
      (%assert-no-truncated-keys direct-optional))))

(test air-pollution-required-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((current-url (openweathermap:build-air-pollution-url 35.0 139.0))
          (forecast-url (openweathermap:build-air-pollution-forecast-url 35.0 139.0))
          (history-url (openweathermap:build-air-pollution-history-url 35.0 139.0 1700000000 1700003600)))
      (%assert-query-keys= current-url '("lat" "lon" "appid"))
      (%assert-query-keys= forecast-url '("lat" "lon" "appid"))
      (%assert-query-keys= history-url '("lat" "lon" "start" "end" "appid"))
      (%assert-no-truncated-keys history-url))))

(test maps-required-and-optional-query-params
  (let ((openweathermap:*api-key* "test-key"))
    (let ((required-url (openweathermap:build-weather-tile-url :temp_new 3 2 3))
          (optional-url (openweathermap:build-weather-tile-url :temp_new 3 2 3 :opacity 0.9 :palette "warm/cold")))
      (%assert-query-keys= required-url '("appid"))
      (%assert-query-keys= optional-url '("appid" "opacity" "palette"))
      (%assert-query-value= optional-url "palette" "warm%2Fcold")
      (%assert-no-truncated-keys optional-url))))
