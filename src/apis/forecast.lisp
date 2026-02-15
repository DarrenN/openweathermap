(in-package :openweathermap)

(defun build-forecast-url (&rest query-params)
  "Build URL for /data/2.5/forecast endpoint."
  (%build-endpoint-url "/data/2.5/forecast" '() query-params))

(defun make-forecast-request (&rest query-params)
  (list :method :get
        :url (apply #'build-forecast-url query-params)))

(defun fetch-forecast (&rest query-params)
  "Fetch and decode forecast response as a plist."
  (let ((url (apply #'build-forecast-url query-params)))
    (%fetch-json url :forecast)))
