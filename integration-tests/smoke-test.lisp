;; SPDX-License-Identifier: MIT
(in-package :openweathermap/integration-tests)

(in-suite onecall-integration-suite)

(test fetch-onecall-live-smoke
  (when (ensure-live-integration-api-key)
    (let ((result (openweathermap:fetch-onecall 40.7128 -74.0060 :units :metric :exclude "minutely,alerts")))
      (is (listp result))
      (is (getf result :lat))
      (is (getf result :timezone)))))

(in-suite current-integration-suite)

(test fetch-current-live-smoke
  (when (ensure-live-integration-api-key)
    (let ((result (openweathermap:fetch-current-weather :q "London" :units :metric)))
      (is (listp result))
      (is (getf result :name)))))

(in-suite forecast-integration-suite)

(test fetch-forecast-live-smoke
  (when (ensure-live-integration-api-key)
    (let ((result (openweathermap:fetch-forecast :q "London" :units :metric :cnt 2)))
      (is (listp result))
      (is (getf result :list)))))

(in-suite geocoding-integration-suite)

(test fetch-geocoding-live-smoke
  (when (ensure-live-integration-api-key)
    (let ((result (openweathermap:fetch-geocoding "London" :limit 1)))
      (is (listp result))
      (is (> (length result) 0)))))

(in-suite air-pollution-integration-suite)

(test fetch-air-pollution-live-smoke
  (when (ensure-live-integration-api-key)
    (let ((result (openweathermap:fetch-air-pollution 40.7128 -74.0060)))
      (is (listp result))
      (is (getf result :list)))))

(in-suite maps-integration-suite)

(test fetch-weather-tile-live-smoke
  (when (ensure-live-integration-api-key)
    (let ((tile (openweathermap:fetch-weather-tile :temp_new 3 2 3 :opacity 0.9)))
      (is (or (stringp tile) (arrayp tile)))
      (is (> (length tile) 0)))))
