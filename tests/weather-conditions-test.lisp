;; SPDX-License-Identifier: MIT

(in-package :openweathermap/tests)

(test lookup-weather-condition-known-id
  (let ((row (openweathermap:lookup-weather-condition 800)))
    (is (listp row))
    (is (= 800 (getf row :id)))
    (is (string= "Clear" (getf row :group)))
    (is (string= "clear sky" (getf row :description)))
    (is (string= "01d" (getf row :icon)))))

(test weather-icon-url-builds-expected-path
  (is (string= "https://openweathermap.org/payload/api/media/file/10d@2x.png"
               (openweathermap:weather-icon-url "10d")))
  (is (string= "https://openweathermap.org/payload/api/media/file/10d.png"
               (openweathermap:weather-icon-url "10d" :size :1x)))
  (is (string= "http://openweathermap.org/payload/api/media/file/10n@4x.png"
               (openweathermap:weather-icon-url "10n" :size :4x :secure-p nil))))

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
