(in-package :openweathermap/tests)

(test build-weather-tile-url
  (let ((openweathermap:*api-key* "test-key")
        (openweathermap:*maps-base-url* "https://tile.openweathermap.org"))
    (let ((url (openweathermap:build-weather-tile-url :temp_new 3 4 5 :opacity 0.7)))
      (is (search "https://tile.openweathermap.org/map/temp_new/3/4/5.png" url))
      (is (search "appid=test-key" url))
      (is (search "opacity=0.7" url)))))

(test build-weather-tile-url-validates-input
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-weather-tile-url "" 1 1 1))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-weather-tile-url :temp_new -1 1 1))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-weather-tile-url :temp_new 1 -1 1))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-weather-tile-url :temp_new 1 1 -1))))

(test make-weather-tile-request-returns-shape
  (let ((openweathermap:*api-key* "test-key"))
    (let ((request (openweathermap:make-weather-tile-request :clouds_new 2 3 4)))
      (is (eq :get (getf request :method)))
      (is (search "/map/clouds_new/2/3/4.png" (getf request :url))))))

(test fetch-weather-tile-returns-raw-body
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "PNG-BYTES" 200)))
      (let ((result (openweathermap:fetch-weather-tile :temp_new 3 4 5)))
        (is (string= "PNG-BYTES" result))))))
