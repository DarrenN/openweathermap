(in-package :openweathermap)

(defun build-current-weather-url (&rest query-params)
  "Build URL for /data/2.5/weather endpoint."
  (%build-endpoint-url "/data/2.5/weather" '() query-params))

(defun make-current-weather-request (&rest query-params)
  (list :method :get
        :url (apply #'build-current-weather-url query-params)))

(defun fetch-current-weather (&rest query-params)
  "Fetch and decode current weather response as a plist."
  (let ((url (apply #'build-current-weather-url query-params)))
    (%fetch-json url :current-weather)))
