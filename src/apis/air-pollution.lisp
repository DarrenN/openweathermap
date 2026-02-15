(in-package :openweathermap)

(defun %ensure-lat-lon (lat lon endpoint-name)
  (unless (and lat lon)
    (error 'invalid-parameters-error
           :message (format nil "~A requires both lat and lon." endpoint-name))))

(defun %ensure-history-range (start end)
  (unless (and start end)
    (error 'invalid-parameters-error
           :message "Air pollution history requires both start and end timestamps."))
  (unless (and (integerp start) (integerp end))
    (error 'invalid-parameters-error
           :message "Air pollution history start/end must be UNIX integer timestamps."))
  (when (> start end)
    (error 'invalid-parameters-error
           :message "Air pollution history requires start <= end.")))

(defun build-air-pollution-url (lat lon)
  "Build URL for current air pollution endpoint."
  (%ensure-lat-lon lat lon "Air pollution current")
  (%build-endpoint-url "/data/2.5/air_pollution" (list :lat lat :lon lon) '()))

(defun make-air-pollution-request (lat lon)
  (list :method :get
        :url (build-air-pollution-url lat lon)))

(defun fetch-air-pollution (lat lon)
  "Fetch and decode current air pollution response as a plist."
  (%fetch-json (build-air-pollution-url lat lon) :air-pollution-current))

(defun build-air-pollution-forecast-url (lat lon)
  "Build URL for air pollution forecast endpoint."
  (%ensure-lat-lon lat lon "Air pollution forecast")
  (%build-endpoint-url "/data/2.5/air_pollution/forecast" (list :lat lat :lon lon) '()))

(defun make-air-pollution-forecast-request (lat lon)
  (list :method :get
        :url (build-air-pollution-forecast-url lat lon)))

(defun fetch-air-pollution-forecast (lat lon)
  "Fetch and decode air pollution forecast response as a plist."
  (%fetch-json (build-air-pollution-forecast-url lat lon) :air-pollution-forecast))

(defun build-air-pollution-history-url (lat lon start end)
  "Build URL for historical air pollution endpoint."
  (%ensure-lat-lon lat lon "Air pollution history")
  (%ensure-history-range start end)
  (%build-endpoint-url "/data/2.5/air_pollution/history"
                       (list :lat lat :lon lon :start start :end end)
                       '()))

(defun make-air-pollution-history-request (lat lon start end)
  (list :method :get
        :url (build-air-pollution-history-url lat lon start end)))

(defun fetch-air-pollution-history (lat lon start end)
  "Fetch and decode historical air pollution response as a plist."
  (%fetch-json (build-air-pollution-history-url lat lon start end) :air-pollution-history))
