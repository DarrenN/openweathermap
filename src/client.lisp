(in-package :openweathermap-onecall)

(defun %normalize-param-key (key)
  (string-downcase
   (etypecase key
     (keyword (subseq (string key) 1))
     (string key)
     (symbol (symbol-name key)))))

(defun %stringify-param-value (value)
  (etypecase value
    (string value)
    (number (princ-to-string value))
    (symbol (string-downcase (symbol-name value)))))

(defun %query-string-from-plist (query-params)
  (with-output-to-string (stream)
    (loop for (key value) on query-params by #'cddr
          for index from 0
          do (when (> index 0)
               (write-char #\& stream))
             (format stream "~A=~A"
                     (%normalize-param-key key)
                     (%stringify-param-value value)))))

(defun build-onecall-url (lat lon &rest query-params)
  "Build URL for /data/3.0/onecall endpoint without performing HTTP request."
  (let* ((api-key (ensure-api-key))
         (merged-params (append (list :lat lat :lon lon :appid api-key)
                                query-params))
         (query-string (%query-string-from-plist merged-params)))
    (format nil "~A/data/3.0/onecall?~A" *api-base-url* query-string)))

(defun make-client-weather-request (lat lon &rest query-params)
  "Foundation stub: currently returns request metadata for future HTTP adapter wiring."
  (list :method :get
        :url (apply #'build-onecall-url lat lon query-params)))
