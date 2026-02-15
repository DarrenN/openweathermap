(in-package :openweathermap-onecall/tests)

(test build-onecall-url-embeds-core-params
  (let ((openweathermap-onecall:*api-key* "test-key"))
    (let ((url (openweathermap-onecall:build-onecall-url 35.0 139.0 :units :metric)))
      (is (search "lat=35.0" url))
      (is (search "lon=139.0" url))
      (is (search "appid=test-key" url))
      (is (search "units=metric" url)))))

(test build-other-endpoint-urls
  (let ((openweathermap-onecall:*api-key* "test-key"))
    (let ((timemachine (openweathermap-onecall:build-timemachine-url 35.0 139.0 1700000000 :lang "en"))
          (day-summary (openweathermap-onecall:build-day-summary-url 35.0 139.0 "2026-02-15" :units :metric))
          (overview (openweathermap-onecall:build-overview-url 35.0 139.0 :date "2026-02-16")))
      (is (search "/data/3.0/onecall/timemachine" timemachine))
      (is (search "dt=1700000000" timemachine))
      (is (search "/data/3.0/onecall/day_summary" day-summary))
      (is (search "date=2026-02-15" day-summary))
      (is (search "/data/3.0/onecall/overview" overview))
      (is (search "date=2026-02-16" overview)))))

(test request-builders-return-shape
  (let ((openweathermap-onecall:*api-key* "test-key"))
    (let ((onecall (openweathermap-onecall:make-client-weather-request 35.0 139.0 :lang "en"))
          (timemachine (openweathermap-onecall:make-timemachine-request 35.0 139.0 1700000000 :units :metric)))
      (is (eq :get (getf onecall :method)))
      (is (search "lang=en" (getf onecall :url)))
      (is (eq :get (getf timemachine :method)))
      (is (search "dt=1700000000" (getf timemachine :url))))))

(test fetch-onecall-parses-json
  (let ((openweathermap-onecall:*api-key* "test-key"))
    (openweathermap-onecall:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "{\"lat\":35.0,\"timezone\":\"Asia/Tokyo\"}" 200)))
      (let ((result (openweathermap-onecall:fetch-onecall 35.0 139.0)))
        (is (listp result))
        (is (or (equal 35.0 (getf result :lat))
                (equal 35.0 (getf result :|lat|))))))))

(test fetch-onecall-retries-transient-status
  (let ((openweathermap-onecall:*api-key* "test-key")
        (openweathermap-onecall:*max-retries* 1)
        (openweathermap-onecall:*retry-backoff-seconds* 0)
        (attempts 0))
    (openweathermap-onecall:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (incf attempts)
           (if (= attempts 1)
               (values "temporary" 500)
               (values "{\"ok\":true}" 200))))
      (let ((result (openweathermap-onecall:fetch-onecall 35.0 139.0)))
        (is (= 2 attempts))
        (is (or (eq t (getf result :ok))
                (eq t (getf result :|ok|))))))))

(test fetch-onecall-signals-api-request-error-on-http-failure
  (let ((openweathermap-onecall:*api-key* "test-key")
        (openweathermap-onecall:*max-retries* 0))
    (openweathermap-onecall:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "bad request" 400)))
      (signals openweathermap-onecall:api-request-error
        (openweathermap-onecall:fetch-onecall 35.0 139.0)))))

(test fetch-onecall-signals-api-network-error-on-transport-failure
  (let ((openweathermap-onecall:*api-key* "test-key")
        (openweathermap-onecall:*max-retries* 0))
    (openweathermap-onecall:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (error "socket closed")))
      (signals openweathermap-onecall:api-network-error
        (openweathermap-onecall:fetch-onecall 35.0 139.0)))))
