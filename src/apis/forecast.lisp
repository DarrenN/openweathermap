(in-package :openweathermap)

(defun %forecast-location-valid-p (lat lon q id zip)
  (or (and lat lon)
      q
      id
      zip))

(defun %compact-forecast-plist (plist)
  (loop for (key value) on plist by #'cddr
        when value
          append (list key value)))

(defun build-forecast-url (&key lat lon q id zip units lang cnt mode)
  "Build URL for /data/2.5/forecast endpoint.

At least one location selector must be provided:
- lat/lon pair
- q (city name)
- id (city id)
- zip
"
  (unless (%forecast-location-valid-p lat lon q id zip)
    (error 'invalid-parameters-error
           :message "Provide location via lat/lon, q, id, or zip."))
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
  (list :method :get
        :url (build-forecast-url
              :lat lat :lon lon :q q :id id :zip zip
              :units units :lang lang :cnt cnt :mode mode)))

(defun fetch-forecast (&key lat lon q id zip units lang cnt mode)
  "Fetch and decode forecast response as a plist."
  (%fetch-json (build-forecast-url
                :lat lat :lon lon :q q :id id :zip zip
                :units units :lang lang :cnt cnt :mode mode)
               :forecast))
