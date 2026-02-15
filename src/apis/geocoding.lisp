(in-package :openweathermap)

(defun build-geocoding-url (query &key limit)
  "Build URL for direct geocoding endpoint."
  (%build-endpoint-url "/geo/1.0/direct"
                       (list :q query)
                       (when limit (list :limit limit))))

(defun make-geocoding-request (query &key limit)
  (list :method :get
        :url (build-geocoding-url query :limit limit)))

(defun fetch-geocoding (query &key limit)
  "Fetch and decode direct geocoding response as a plist."
  (%fetch-json (build-geocoding-url query :limit limit) :geocoding-direct))

(defun build-reverse-geocoding-url (lat lon &key limit)
  "Build URL for reverse geocoding endpoint."
  (%build-endpoint-url "/geo/1.0/reverse"
                       (list :lat lat :lon lon)
                       (when limit (list :limit limit))))

(defun make-reverse-geocoding-request (lat lon &key limit)
  (list :method :get
        :url (build-reverse-geocoding-url lat lon :limit limit)))

(defun fetch-reverse-geocoding (lat lon &key limit)
  "Fetch and decode reverse geocoding response as a plist."
  (%fetch-json (build-reverse-geocoding-url lat lon :limit limit) :geocoding-reverse))

(defun build-zip-geocoding-url (zip &key country-code)
  "Build URL for zip geocoding endpoint."
  (%build-endpoint-url "/geo/1.0/zip"
                       (list :zip (if country-code
                                      (format nil "~A,~A" zip country-code)
                                      zip))
                       '()))

(defun make-zip-geocoding-request (zip &key country-code)
  (list :method :get
        :url (build-zip-geocoding-url zip :country-code country-code)))

(defun fetch-zip-geocoding (zip &key country-code)
  "Fetch and decode zip geocoding response as a plist."
  (%fetch-json (build-zip-geocoding-url zip :country-code country-code) :geocoding-zip))
