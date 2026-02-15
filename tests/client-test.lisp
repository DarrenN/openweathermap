(in-package :openweathermap-onecall/tests)

(test build-onecall-url-embeds-core-params
  (let ((openweathermap-onecall:*api-key* "test-key"))
    (let ((url (openweathermap-onecall:build-onecall-url 35.0 139.0 :units :metric)))
      (is (search "lat=35.0" url))
      (is (search "lon=139.0" url))
      (is (search "appid=test-key" url))
      (is (search "units=metric" url)))))

(test make-client-weather-request-returns-request-shape
  (let ((openweathermap-onecall:*api-key* "test-key"))
    (let ((request (openweathermap-onecall:make-client-weather-request 35.0 139.0 :lang "en")))
      (is (eq :get (getf request :method)))
      (is (search "lang=en" (getf request :url))))))
