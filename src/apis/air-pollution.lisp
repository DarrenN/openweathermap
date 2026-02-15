(in-package :openweathermap)

(defun build-air-pollution-url (lat lon)
  "Build URL for current air pollution endpoint."
  (%build-endpoint-url "/data/2.5/air_pollution" (list :lat lat :lon lon) '()))

(defun make-air-pollution-request (lat lon)
  (list :method :get
        :url (build-air-pollution-url lat lon)))

(defun fetch-air-pollution (lat lon)
  "Fetch and decode current air pollution response as a plist."
  (%fetch-json (build-air-pollution-url lat lon) :air-pollution-current))

(defun build-air-pollution-forecast-url (lat lon)
  "Build URL for air pollution forecast endpoint."
  (%build-endpoint-url "/data/2.5/air_pollution/forecast" (list :lat lat :lon lon) '()))

(defun make-air-pollution-forecast-request (lat lon)
  (list :method :get
        :url (build-air-pollution-forecast-url lat lon)))

(defun fetch-air-pollution-forecast (lat lon)
  "Fetch and decode air pollution forecast response as a plist."
  (%fetch-json (build-air-pollution-forecast-url lat lon) :air-pollution-forecast))

(defun build-air-pollution-history-url (lat lon start end)
  "Build URL for historical air pollution endpoint."
  (%build-endpoint-url "/data/2.5/air_pollution/history"
                       (list :lat lat :lon lon :start start :end end)
                       '()))

(defun make-air-pollution-history-request (lat lon start end)
  (list :method :get
        :url (build-air-pollution-history-url lat lon start end)))

(defun fetch-air-pollution-history (lat lon start end)
  "Fetch and decode historical air pollution response as a plist."
  (%fetch-json (build-air-pollution-history-url lat lon start end) :air-pollution-history))
