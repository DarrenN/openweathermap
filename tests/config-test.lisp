(in-package :openweathermap-onecall/tests)

(test configure-api-key-stores-value
  (let ((openweathermap-onecall:*api-key* nil))
    (openweathermap-onecall:configure-api-key "abc123")
    (is (string= "abc123" openweathermap-onecall:*api-key*))))

(test configure-client-updates-runtime-settings
  (let ((openweathermap-onecall:*api-base-url* "https://api.openweathermap.org")
        (openweathermap-onecall:*request-timeout-seconds* 10)
        (openweathermap-onecall:*max-retries* 2)
        (openweathermap-onecall:*retry-backoff-seconds* 1))
    (openweathermap-onecall:configure-client
     :api-base-url "https://example.invalid"
     :request-timeout-seconds 3
     :max-retries 4
     :retry-backoff-seconds 0)
    (is (string= "https://example.invalid" openweathermap-onecall:*api-base-url*))
    (is (= 3 openweathermap-onecall:*request-timeout-seconds*))
    (is (= 4 openweathermap-onecall:*max-retries*))
    (is (= 0 openweathermap-onecall:*retry-backoff-seconds*))))
