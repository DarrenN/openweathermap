(in-package :openweathermap/tests)

(test configure-api-key-stores-value
  (let ((openweathermap:*api-key* nil))
    (openweathermap:configure-api-key "abc123")
    (is (string= "abc123" openweathermap:*api-key*))))

(test configure-client-updates-runtime-settings
  (let ((openweathermap:*api-base-url* "https://api.openweathermap.org")
        (openweathermap:*request-timeout-seconds* 10)
        (openweathermap:*max-retries* 2)
        (openweathermap:*retry-backoff-seconds* 1))
    (openweathermap:configure-client
     :api-base-url "https://example.invalid"
     :request-timeout-seconds 3
     :max-retries 4
     :retry-backoff-seconds 0)
    (is (string= "https://example.invalid" openweathermap:*api-base-url*))
    (is (= 3 openweathermap:*request-timeout-seconds*))
    (is (= 4 openweathermap:*max-retries*))
    (is (= 0 openweathermap:*retry-backoff-seconds*))))
