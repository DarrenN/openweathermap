(in-package :openweathermap/tests)

(test build-forecast-url-by-coordinates
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-forecast-url
                :lat 35.0 :lon 139.0 :units :metric :cnt 8)))
      (is (search "/data/2.5/forecast" url))
      (is (search "lat=35.0" url))
      (is (search "lon=139.0" url))
      (is (search "units=metric" url))
      (is (search "cnt=8" url))
      (is (search "appid=test-key" url)))))

(test build-forecast-url-by-city-query
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-forecast-url :q "Berlin" :lang "de")))
      (is (search "q=Berlin" url))
      (is (search "lang=de" url)))))

(test build-forecast-url-requires-location
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-forecast-url :units :metric))))

(test make-forecast-request-returns-shape
  (let ((openweathermap:*api-key* "test-key"))
    (let ((request (openweathermap:make-forecast-request :zip "10001,US" :mode "json")))
      (is (eq :get (getf request :method)))
      (is (search "zip=10001,US" (getf request :url)))
      (is (search "mode=json" (getf request :url))))))

(test fetch-forecast-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "{\"cod\":\"200\",\"cnt\":40}" 200)))
      (let ((result (openweathermap:fetch-forecast :q "Tokyo")))
        (is (listp result))
        (is (or (equal 40 (getf result :cnt))
                (equal 40 (getf result :|cnt|))))))))
