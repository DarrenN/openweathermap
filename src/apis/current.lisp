(in-package :openweathermap)

(defun %current-weather-location-valid-p (lat lon q id zip)
  (or (and lat lon)
      q
      id
      zip))

(defun %compact-plist (plist)
  (loop for (key value) on plist by #'cddr
        when value
          append (list key value)))

(defun build-current-weather-url (&key lat lon q id zip units lang mode)
  "Build URL for /data/2.5/weather endpoint.

At least one location selector must be provided:
- lat/lon pair
- q (city name)
- id (city id)
- zip
"
  (unless (%current-weather-location-valid-p lat lon q id zip)
    (error 'invalid-parameters-error
           :message "Provide location via lat/lon, q, id, or zip."))
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
  (list :method :get
        :url (build-current-weather-url
              :lat lat :lon lon :q q :id id :zip zip
              :units units :lang lang :mode mode)))

(defun fetch-current-weather (&key lat lon q id zip units lang mode)
  "Fetch and decode current weather response as a plist."
  (%fetch-json (build-current-weather-url
                :lat lat :lon lon :q q :id id :zip zip
                :units units :lang lang :mode mode)
               :current-weather))
