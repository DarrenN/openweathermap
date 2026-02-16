;; SPDX-License-Identifier: MIT
(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(format t "~&Weather condition helper demo~%~%")

(let ((canonical (lookup-weather-condition 500)))
  (format t "lookup-weather-condition 500 => ~S~%~%" canonical))

(format t "weather-icon-url 10d default => ~A~%"
        (weather-icon-url "10d"))
(format t "weather-icon-url 10d :size :1x => ~A~%"
        (weather-icon-url "10d" :size :1x))
(format t "weather-icon-url 10n :size :4x => ~A~%~%"
        (weather-icon-url "10n" :size :4x))

(let ((resolved (resolve-weather-condition
                 :id 500
                 :icon "10n"
                 :description "light rain"
                 :main "Rain")))
  (format t "resolve-weather-condition => ~S~%~%" resolved))

(let* ((entry '(:id 801 :main "Clouds" :description "few clouds" :icon "02n"))
       (enriched (enrich-weather-entry entry)))
  (format t "enrich-weather-entry => ~S~%~%" enriched))

(let* ((entries '((:id 800 :main "Clear" :description "clear sky" :icon "01d")
                  (:id 500 :main "Rain" :description "light rain" :icon "10n")))
       (enriched-list (enrich-weather-list entries)))
  (format t "enrich-weather-list => ~S~%" enriched-list))
