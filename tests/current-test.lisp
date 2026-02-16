;; SPDX-License-Identifier: MIT
(in-package :openweathermap/tests)

(test build-current-weather-url-by-coordinates
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-current-weather-url
                :lat 35.0 :lon 139.0 :units :metric :lang "en")))
      (is (search "/data/2.5/weather" url))
      (is (search "lat=35.0" url))
      (is (search "lon=139.0" url))
      (is (search "units=metric" url))
      (is (search "appid=test-key" url)))))

(test build-current-weather-url-by-city-query
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-current-weather-url :q "London" :units :imperial)))
      (is (search "q=London" url))
      (is (search "units=imperial" url)))))

(test build-current-weather-url-requires-location
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-current-weather-url :units :metric))))

(test build-current-weather-url-requires-lat-lon-pair
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-current-weather-url :lat 35.0))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-current-weather-url :lon 139.0))))

(test build-current-weather-url-requires-exactly-one-selector
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-current-weather-url :q "Tokyo" :id 1850147))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-current-weather-url :lat 35.0 :lon 139.0 :q "Tokyo"))))

(test make-current-weather-request-returns-shape
  (let ((openweathermap:*api-key* "test-key"))
    (let ((request (openweathermap:make-current-weather-request :id 524901 :lang "en")))
      (is (eq :get (getf request :method)))
      (is (search "id=524901" (getf request :url))))))

(test fetch-current-weather-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "{\"name\":\"Tokyo\",\"cod\":200}" 200)))
      (let ((result (openweathermap:fetch-current-weather :q "Tokyo")))
        (is (listp result))
        (is (or (equal "Tokyo" (getf result :name))
                (equal "Tokyo" (getf result :|name|))))))))

(test fetch-current-weather-rejects-non-json-mode
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:fetch-current-weather :q "Tokyo" :mode "xml"))))
