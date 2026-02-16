;; SPDX-License-Identifier: MIT

(in-package :openweathermap/tests)

(test lookup-weather-condition-known-id
  (let ((row (openweathermap:lookup-weather-condition 800)))
    (is (listp row))
    (is (= 800 (getf row :id)))
    (is (string= "Clear" (getf row :group)))
    (is (string= "clear sky" (getf row :description)))
    (is (string= "01d" (getf row :icon)))))

(test lookup-weather-condition-known-id-samples
  (let ((samples '((200 "Thunderstorm" "11d")
                   (500 "Rain" "10d")
                   (800 "Clear" "01d")
                   (804 "Clouds" "04d"))))
    (dolist (sample samples)
      (destructuring-bind (id expected-group expected-icon) sample
        (let ((row (openweathermap:lookup-weather-condition id)))
          (is (listp row))
          (is (= id (getf row :id)))
          (is (string= expected-group (getf row :group)))
          (is (string= expected-icon (getf row :icon))))))))

(test weather-icon-url-builds-expected-path
  (is (string= "https://openweathermap.org/payload/api/media/file/10d@2x.png"
               (openweathermap:weather-icon-url "10d")))
  (is (string= "https://openweathermap.org/payload/api/media/file/10d.png"
               (openweathermap:weather-icon-url "10d" :size :1x)))
  (is (string= "http://openweathermap.org/payload/api/media/file/10n@4x.png"
               (openweathermap:weather-icon-url "10n" :size :4x :secure-p nil))))

(test weather-icon-url-validates-input
  (signals openweathermap:invalid-parameters-error
    (openweathermap:weather-icon-url "10x"))
  (signals openweathermap:invalid-parameters-error
    (openweathermap:weather-icon-url "1d"))
  (signals openweathermap:invalid-parameters-error
    (openweathermap:weather-icon-url "10d" :size :3x)))

(test resolve-weather-condition-prefers-payload-fields
  (let ((resolved (openweathermap:resolve-weather-condition
                   :id 500
                   :icon "10n"
                   :description "lluvia ligera"
                   :main "Rain")))
    (is (= 500 (getf resolved :id)))
    (is (string= "Rain" (getf resolved :main)))
    (is (string= "lluvia ligera" (getf resolved :description)))
    (is (string= "light rain" (getf resolved :canonical-description)))
    (is (string= "10n" (getf resolved :icon)))
    (is (string= "10d" (getf resolved :icon-day)))
    (is (string= "10n" (getf resolved :icon-night)))
    (is (search "/10n@2x.png" (getf resolved :icon-url)))
    (is (not (getf resolved :unknown-condition-p)))))

(test resolve-weather-condition-falls-back-to-canonical-fields
  (let ((resolved (openweathermap:resolve-weather-condition :id 800)))
    (is (= 800 (getf resolved :id)))
    (is (string= "Clear" (getf resolved :main)))
    (is (string= "clear sky" (getf resolved :description)))
    (is (string= "clear sky" (getf resolved :canonical-description)))
    (is (string= "01d" (getf resolved :icon)))
    (is (string= "01d" (getf resolved :icon-day)))
    (is (string= "01n" (getf resolved :icon-night)))
    (is (search "/01d@2x.png" (getf resolved :icon-url)))
    (is (not (getf resolved :unknown-condition-p)))))

(test resolve-weather-condition-unknown-id
  (let ((resolved (openweathermap:resolve-weather-condition
                   :id 999
                   :icon "02n"
                   :description "custom"
                   :main "Custom")))
    (is (= 999 (getf resolved :id)))
    (is (eq t (getf resolved :unknown-condition-p)))
    (is (string= "02d" (getf resolved :icon-day)))
    (is (string= "02n" (getf resolved :icon-night)))
    (is (string= "custom" (getf resolved :description)))))

(test enrich-weather-entry-tolerates-pipe-keys
  (let* ((entry (list :|id| 801
                      :|main| "Clouds"
                      :|description| "few clouds"
                      :|icon| "02n"))
         (resolved (openweathermap:enrich-weather-entry entry)))
    (is (= 801 (getf resolved :id)))
    (is (string= "Clouds" (getf resolved :main)))
    (is (string= "few clouds" (getf resolved :description)))
    (is (string= "02d" (getf resolved :icon-day)))
    (is (string= "02n" (getf resolved :icon-night)))))

(test enrich-weather-list-maps-over-list
  (let* ((input (list (list :id 800 :main "Clear" :description "clear sky" :icon "01d")
                      (list :id 500 :main "Rain" :description "light rain" :icon "10n")))
         (output (openweathermap:enrich-weather-list input)))
    (is (= 2 (length output)))
    (is (= 800 (getf (first output) :id)))
    (is (= 500 (getf (second output) :id)))))

(test weather-condition-catalog-icons-conform-to-supported-set
  (let ((allowed-icons '("01d" "01n"
                         "02d" "02n"
                         "03d" "03n"
                         "04d" "04n"
                         "09d" "09n"
                         "10d" "10n"
                         "11d" "11n"
                         "13d" "13n"
                         "50d" "50n")))
    (dolist (row openweathermap::*weather-condition-catalog*)
      (let ((icon (getf row :icon)))
        (is (member icon allowed-icons :test #'string=))))))
