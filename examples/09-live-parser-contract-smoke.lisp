;; SPDX-License-Identifier: MIT
(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(defun %fail (control &rest args)
  "Signal a formatted error using CONTROL and ARGS."
  (error (apply #'format nil control args)))

(defun %assert (condition control &rest args)
  "Signal an error when CONDITION is false."
  (unless condition
    (apply #'%fail control args)))

(defun %keyword-plist-p (value)
  "Return true when VALUE looks like a proper even plist with keyword keys."
  (handler-case
      (and (listp value)
           (evenp (length value))
           (loop for (key _) on value by #'cddr
                 always (keywordp key)))
    (type-error ()
      nil)))

(defun %ensure-normalized-json-shape (value label)
  "Recursively validate decoded JSON uses plist/list scalars, not hash-tables/vectors."
  (cond
    ((hash-table-p value)
     (%fail "~A unexpectedly contains a hash-table after decode." label))
    ((stringp value)
     t)
    ((vectorp value)
     (%fail "~A unexpectedly contains a vector after decode." label))
    ((and (listp value) value (every #'characterp value))
     (%fail "~A contains a character list where a string-like value was expected." label))
    ((%keyword-plist-p value)
     (loop for (key item) on value by #'cddr
           do (%assert (keywordp key)
                       "~A includes non-keyword key ~S."
                       label
                       key)
              (%ensure-normalized-json-shape item
                                             (format nil "~A.~(~A~)" label key))))
    ((listp value)
     (loop for item in value
           for index from 0
           do (%ensure-normalized-json-shape item
                                             (format nil "~A[~D]" label index)))))
  t)

(defun %require-string (value label)
  "Ensure VALUE is a non-empty string."
  (%assert (and (stringp value) (> (length value) 0))
           "~A must be a non-empty string, got ~S."
           label
           value))

(defun %run-live-step (label thunk)
  "Execute THUNK and provide structured API error output on failure."
  (format t "~&[LIVE] ~A~%" label)
  (handler-case
      (funcall thunk)
    (api-request-error (err)
      (%fail "~A failed (status=~A endpoint=~A): ~A"
             label
             (api-request-error-status-code err)
             (api-request-error-endpoint err)
             (api-request-error-message err)))))

(let ((api-key (uiop:getenv "OPENWEATHER_API_KEY")))
  (%assert (and api-key (> (length api-key) 0))
           "Set OPENWEATHER_API_KEY before running this example.")
  (configure-api-key api-key))

(let* ((lat 40.7128)
       (lon -74.0060)
       (onecall-request (make-onecall-request lat lon
                                              :units :metric
                                              :exclude "minutely,alerts"))
       (onecall-url (getf onecall-request :url)))
  (format t "~&One Call request URL (make-onecall-request): ~A~%" onecall-url)
  (%assert (search "appid=" onecall-url)
           "One Call request URL does not include appid query parameter."))

(let* ((current (%run-live-step
                 "Current Weather"
                 (lambda ()
                   (fetch-current-weather :q "London" :units :metric :lang "en"))))
       (weather-list (getf current :weather))
       (primary-weather (first weather-list))
       (enriched (enrich-weather-entry primary-weather)))
  (%ensure-normalized-json-shape current "current")
  (%require-string (getf current :name) "current.:name")
  (%assert (listp weather-list) "current.:weather must be a list, got ~S." weather-list)
  (%assert weather-list "current.:weather must be non-empty.")
  (%require-string (getf primary-weather :description) "current.:weather[0].:description")
  (%require-string (getf primary-weather :icon) "current.:weather[0].:icon")
  (%assert (numberp (getf (getf current :main) :temp))
           "current.:main.:temp must be numeric.")
  (%require-string (getf enriched :icon-url) "enriched.:icon-url")
  (format t "  City: ~A, Temp: ~A C, Icon: ~A~%"
          (getf current :name)
          (getf (getf current :main) :temp)
          (getf primary-weather :icon)))

(let* ((forecast (%run-live-step
                  "Forecast"
                  (lambda ()
                    (fetch-forecast :q "London" :units :metric :cnt 2))))
       (entries (getf forecast :list))
       (first-entry (first entries)))
  (%ensure-normalized-json-shape forecast "forecast")
  (%assert (listp entries) "forecast.:list must be a list, got ~S." entries)
  (%assert entries "forecast.:list must be non-empty.")
  (%assert (integerp (getf first-entry :dt))
           "forecast.:list[0].:dt must be an integer.")
  (%require-string (getf first-entry :dt_txt) "forecast.:list[0].:dt_txt")
  (%assert (numberp (getf (getf first-entry :main) :temp))
           "forecast.:list[0].:main.:temp must be numeric.")
  (format t "  Forecast entries: ~D, First dt_txt: ~A~%"
          (length entries)
          (getf first-entry :dt_txt)))

(let* ((geocoding (%run-live-step
                   "Direct Geocoding"
                   (lambda ()
                     (fetch-geocoding "New York" :limit 1))))
       (first-item (first geocoding)))
  (%ensure-normalized-json-shape geocoding "geocoding")
  (%assert (and (listp geocoding) geocoding)
           "geocoding response must be a non-empty list.")
  (%require-string (getf first-item :name) "geocoding[0].:name")
  (%assert (realp (getf first-item :lat))
           "geocoding[0].:lat must be numeric.")
  (%assert (realp (getf first-item :lon))
           "geocoding[0].:lon must be numeric.")
  (format t "  Geocoding top match: ~A (~A, ~A)~%"
          (getf first-item :name)
          (getf first-item :lat)
          (getf first-item :lon)))

(let* ((onecall (%run-live-step
                 "One Call"
                 (lambda ()
                   (fetch-onecall 40.7128 -74.0060
                                  :units :metric
                                  :exclude "minutely,alerts"))))
       (current-block (getf onecall :current))
       (current-weather (getf current-block :weather))
       (primary-weather (first current-weather)))
  (%ensure-normalized-json-shape onecall "onecall")
  (%require-string (getf onecall :timezone) "onecall.:timezone")
  (%assert (%keyword-plist-p current-block)
           "onecall.:current must be a keyword plist, got ~S."
           current-block)
  (%assert (and (listp current-weather) current-weather)
           "onecall.:current.:weather must be a non-empty list.")
  (%require-string (getf primary-weather :description)
                   "onecall.:current.:weather[0].:description")
  (format t "  One Call timezone: ~A~%" (getf onecall :timezone)))

(let* ((air (%run-live-step
             "Air Pollution"
             (lambda ()
               (fetch-air-pollution 40.7128 -74.0060))))
       (air-list (getf air :list))
       (first-item (first air-list)))
  (%ensure-normalized-json-shape air "air")
  (%assert (and (listp air-list) air-list)
           "air.:list must be a non-empty list.")
  (%assert (%keyword-plist-p (getf first-item :components))
           "air.:list[0].:components must be a keyword plist.")
  (%assert (integerp (getf (getf first-item :main) :aqi))
           "air.:list[0].:main.:aqi must be an integer.")
  (format t "  AQI: ~A~%" (getf (getf first-item :main) :aqi)))

(format t "~&Live parser and ergonomics smoke checks passed.~%")
