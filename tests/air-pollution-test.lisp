;; SPDX-License-Identifier: MIT
(in-package :openweathermap/tests)

(test build-air-pollution-url-current
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-air-pollution-url 35.0 139.0)))
      (is (search "/data/2.5/air_pollution" url))
      (is (search "lat=35.0" url))
      (is (search "lon=139.0" url))
      (is (search "appid=test-key" url)))))

(test build-air-pollution-forecast-url
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-air-pollution-forecast-url 35.0 139.0)))
      (is (search "/data/2.5/air_pollution/forecast" url))
      (is (search "lat=35.0" url))
      (is (search "lon=139.0" url)))))

(test build-air-pollution-history-url
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-air-pollution-history-url 35.0 139.0 1700000000 1700003600)))
      (is (search "/data/2.5/air_pollution/history" url))
      (is (search "start=1700000000" url))
      (is (search "end=1700003600" url)))))

(test air-pollution-requires-lat-lon
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-air-pollution-url nil 139.0))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-air-pollution-forecast-url 35.0 nil))))

(test air-pollution-history-validates-range
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-air-pollution-history-url 35.0 139.0 nil 1700003600))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-air-pollution-history-url 35.0 139.0 1700003600 1700000000))))

(test make-air-pollution-request-returns-shape
  (let ((openweathermap:*api-key* "test-key"))
    (let ((request (openweathermap:make-air-pollution-request 35.0 139.0)))
      (is (eq :get (getf request :method)))
      (is (search "/data/2.5/air_pollution" (getf request :url))))))

(test make-air-pollution-history-request-returns-shape
  (let ((openweathermap:*api-key* "test-key"))
    (let ((request (openweathermap:make-air-pollution-history-request 35.0 139.0 1700000000 1700003600)))
      (is (eq :get (getf request :method)))
      (is (search "start=1700000000" (getf request :url))))))

(test fetch-air-pollution-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "{\"coord\":{\"lon\":139.0,\"lat\":35.0},\"list\":[]}" 200)))
      (let ((result (openweathermap:fetch-air-pollution 35.0 139.0)))
        (is (listp result))
        (is (or (getf result :coord) (getf result :|coord|)))))))

(test fetch-air-pollution-forecast-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "{\"list\":[]}" 200)))
      (let ((result (openweathermap:fetch-air-pollution-forecast 35.0 139.0)))
        (is (listp result))))))

(test fetch-air-pollution-history-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "{\"list\":[{\"dt\":1700000000}]}" 200)))
      (let ((result (openweathermap:fetch-air-pollution-history 35.0 139.0 1700000000 1700003600)))
        (is (listp result))
        (is (or (getf result :list) (getf result :|list|)))))))
