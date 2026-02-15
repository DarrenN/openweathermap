(in-package :openweathermap-onecall/integration-tests)

(test build-url-with-live-key
  (let ((key (uiop:getenv "OPENWEATHER_API_KEY")))
    (if (or (null key) (string= key ""))
        (skip "Set OPENWEATHER_API_KEY to run integration tests.")
        (let ((openweathermap-onecall:*api-key* key)
              (url (openweathermap-onecall:build-onecall-url 40.7128 -74.0060 :units :metric)))
          (is (search "appid=" url))
          (is (search "units=metric" url))))))
