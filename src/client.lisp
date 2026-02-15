(in-package :openweathermap)

(defvar *http-get-function* nil
  "Function of (url timeout-seconds) returning two values: body and status code.")

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

(defun %build-endpoint-url (path base-params extra-params)
  (let* ((api-key (ensure-api-key))
         (merged-params (append base-params (list :appid api-key) extra-params))
         (query-string (%query-string-from-plist merged-params)))
    (format nil "~A~A?~A" *api-base-url* path query-string)))

(defun build-onecall-url (lat lon &rest query-params)
  "Build URL for /data/3.0/onecall endpoint without performing HTTP request."
  (%build-endpoint-url "/data/3.0/onecall" (list :lat lat :lon lon) query-params))

(defun build-timemachine-url (lat lon dt &rest query-params)
  "Build URL for /data/3.0/onecall/timemachine endpoint."
  (%build-endpoint-url "/data/3.0/onecall/timemachine" (list :lat lat :lon lon :dt dt) query-params))

(defun build-day-summary-url (lat lon date &rest query-params)
  "Build URL for /data/3.0/onecall/day_summary endpoint."
  (%build-endpoint-url "/data/3.0/onecall/day_summary" (list :lat lat :lon lon :date date) query-params))

(defun build-overview-url (lat lon &rest query-params)
  "Build URL for /data/3.0/onecall/overview endpoint."
  (%build-endpoint-url "/data/3.0/onecall/overview" (list :lat lat :lon lon) query-params))

(defun make-client-weather-request (lat lon &rest query-params)
  (list :method :get
        :url (apply #'build-onecall-url lat lon query-params)))

(defun make-timemachine-request (lat lon dt &rest query-params)
  (list :method :get
        :url (apply #'build-timemachine-url lat lon dt query-params)))

(defun make-day-summary-request (lat lon date &rest query-params)
  (list :method :get
        :url (apply #'build-day-summary-url lat lon date query-params)))

(defun make-overview-request (lat lon &rest query-params)
  (list :method :get
        :url (apply #'build-overview-url lat lon query-params)))

(defun %default-http-get (url timeout-seconds)
  (multiple-value-bind (body status)
      (dexador:get url
                   :read-timeout timeout-seconds
                   :connect-timeout timeout-seconds
                   :keep-alive nil)
    (values body status)))

(defun %http-get (url)
  (funcall (or *http-get-function* #'%default-http-get)
           url
           *request-timeout-seconds*))

(defmacro with-http-get-function ((function) &body body)
  "Temporarily override HTTP GET implementation for tests/custom transports."
  `(let ((*http-get-function* ,function))
     ,@body))

(defun %should-retry-status-p (status)
  (and status (or (= status 429) (>= status 500))))

(defun %parse-response-body (response-body endpoint)
  (handler-case
      (jonathan:parse response-body :as :plist)
    (error (err)
      (error 'api-response-parse-error
             :status-code nil
             :endpoint endpoint
             :message (princ-to-string err)))))

(defun %fetch-json (url endpoint)
  (loop with attempts = 0
        with max-attempts = (1+ *max-retries*)
        do (incf attempts)
           (handler-case
               (multiple-value-bind (body status) (%http-get url)
                 (cond
                   ((or (null status) (<= 200 status 299))
                    (return (%parse-response-body body endpoint)))
                   ((and (< attempts max-attempts) (%should-retry-status-p status))
                    (sleep *retry-backoff-seconds*))
                   (t
                    (error 'api-request-error
                           :status-code status
                           :endpoint endpoint
                           :message body))))
             (api-request-error (err)
               (error err))
             (error (err)
               (if (< attempts max-attempts)
                   (sleep *retry-backoff-seconds*)
                   (error 'api-network-error
                          :status-code nil
                          :endpoint endpoint
                          :message (princ-to-string err)))))))

(defun fetch-onecall (lat lon &rest query-params)
  "Fetch and decode /data/3.0/onecall response as a plist."
  (let ((url (apply #'build-onecall-url lat lon query-params)))
    (%fetch-json url :onecall)))

(defun fetch-timemachine (lat lon dt &rest query-params)
  "Fetch and decode /data/3.0/onecall/timemachine response as a plist."
  (let ((url (apply #'build-timemachine-url lat lon dt query-params)))
    (%fetch-json url :timemachine)))

(defun fetch-day-summary (lat lon date &rest query-params)
  "Fetch and decode /data/3.0/onecall/day_summary response as a plist."
  (let ((url (apply #'build-day-summary-url lat lon date query-params)))
    (%fetch-json url :day-summary)))

(defun fetch-overview (lat lon &rest query-params)
  "Fetch and decode /data/3.0/onecall/overview response as a plist."
  (let ((url (apply #'build-overview-url lat lon query-params)))
    (%fetch-json url :overview)))
