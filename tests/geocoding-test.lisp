;; SPDX-License-Identifier: MIT
(in-package :openweathermap/tests)

(test build-geocoding-url-direct
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-geocoding-url "London" :limit 5)))
      (is (search "/geo/1.0/direct" url))
      (is (search "q=London" url))
      (is (search "limit=5" url))
      (is (search "appid=test-key" url)))))

(test build-geocoding-url-requires-query
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-geocoding-url ""))))

(test build-geocoding-url-validates-limit-range
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-geocoding-url "London" :limit 0))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-geocoding-url "London" :limit 6))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-geocoding-url "London" :limit 2.5))))

(test build-reverse-geocoding-url
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-reverse-geocoding-url 35.0 139.0 :limit 3)))
      (is (search "/geo/1.0/reverse" url))
      (is (search "lat=35.0" url))
      (is (search "lon=139.0" url))
      (is (search "limit=3" url)))))

(test build-reverse-geocoding-url-requires-lat-lon
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-reverse-geocoding-url nil 139.0))))

(test build-reverse-geocoding-url-validates-limit-range
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-reverse-geocoding-url 35.0 139.0 :limit 0))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-reverse-geocoding-url 35.0 139.0 :limit 6))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-reverse-geocoding-url 35.0 139.0 :limit 3.5))))

(test build-zip-geocoding-url
  (let ((openweathermap:*api-key* "test-key"))
    (let ((url (openweathermap:build-zip-geocoding-url "94040" :country-code "US")))
      (is (search "/geo/1.0/zip" url))
      (is (search "zip=94040,US" url)))))

(test build-zip-geocoding-url-requires-zip
  (let ((openweathermap:*api-key* "test-key"))
    (signals openweathermap:invalid-parameters-error
      (openweathermap:build-zip-geocoding-url ""))))

(test make-geocoding-request-returns-shape
  (let ((openweathermap:*api-key* "test-key"))
    (let ((request (openweathermap:make-geocoding-request "Paris" :limit 2)))
      (is (eq :get (getf request :method)))
      (is (search "q=Paris" (getf request :url))))))

(test fetch-geocoding-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "[{\"name\":\"London\",\"lat\":51.5072}]" 200)))
      (let ((result (openweathermap:fetch-geocoding "London" :limit 1)))
        (is (listp result))
        (let ((first (first result)))
          (is (or (equal "London" (getf first :name))
                  (equal "London" (getf first :|name|)))))))))

(test fetch-reverse-geocoding-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "[{\"name\":\"Tokyo\",\"country\":\"JP\"}]" 200)))
      (let ((result (openweathermap:fetch-reverse-geocoding 35.0 139.0 :limit 1)))
        (is (listp result))))))

(test fetch-zip-geocoding-parses-json
  (let ((openweathermap:*api-key* "test-key"))
    (openweathermap:with-http-get-function
        ((lambda (_url _timeout)
           (declare (ignore _url _timeout))
           (values "{\"name\":\"Mountain View\",\"lat\":37.3861}" 200)))
      (let ((result (openweathermap:fetch-zip-geocoding "94040" :country-code "US")))
        (is (listp result))
        (is (or (equal "Mountain View" (getf result :name))
                (equal "Mountain View" (getf result :|name|))))))))
