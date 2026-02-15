(in-package :openweathermap)

(defvar *http-get-function* nil
  "Function of (url timeout-seconds) returning two values: body and status code.")

(defun %normalize-param-key (key)
  "Normalize plist key names to lowercase query parameter names."
  (string-downcase
   (etypecase key
     (keyword (symbol-name key))
     (string key)
     (symbol (symbol-name key)))))

(defun %stringify-param-value (value)
  "Convert supported query parameter values to their serialized string form."
  (etypecase value
    (string value)
    (number (princ-to-string value))
    (symbol (string-downcase (symbol-name value)))))

(defun %utf8-octets-for-codepoint (codepoint)
  "Encode a Unicode codepoint into a list of UTF-8 octets."
  (cond
    ((<= codepoint #x7F)
     (list codepoint))
    ((<= codepoint #x7FF)
     (list (+ #xC0 (ash codepoint -6))
           (+ #x80 (logand codepoint #x3F))))
    ((<= codepoint #xFFFF)
     (list (+ #xE0 (ash codepoint -12))
           (+ #x80 (logand (ash codepoint -6) #x3F))
           (+ #x80 (logand codepoint #x3F))))
    ((<= codepoint #x10FFFF)
     (list (+ #xF0 (ash codepoint -18))
           (+ #x80 (logand (ash codepoint -12) #x3F))
           (+ #x80 (logand (ash codepoint -6) #x3F))
           (+ #x80 (logand codepoint #x3F))))
    (t
     (error "Unsupported Unicode codepoint: ~A" codepoint))))

(defun %string-utf8-octets (string)
  "Return UTF-8 octets for STRING."
  (loop for ch across string
        append (%utf8-octets-for-codepoint (char-code ch))))

(defun %unreserved-uri-octet-p (octet)
  "Return true when OCTET is an unreserved URI character."
  (or (and (>= octet (char-code #\A)) (<= octet (char-code #\Z)))
      (and (>= octet (char-code #\a)) (<= octet (char-code #\z)))
      (and (>= octet (char-code #\0)) (<= octet (char-code #\9)))
      (= octet (char-code #\-))
      (= octet (char-code #\_))
      (= octet (char-code #\.))
      (= octet (char-code #\~))))

(defun %percent-encode-uri-component (string)
  "Percent-encode STRING as a URI query component."
  (with-output-to-string (stream)
    (dolist (octet (%string-utf8-octets string))
      (if (%unreserved-uri-octet-p octet)
          (write-char (code-char octet) stream)
          (format stream "%~2,'0X" octet)))))

(defun %query-string-from-plist (query-params)
  "Serialize QUERY-PARAMS plist into a URL-encoded query string."
  (with-output-to-string (stream)
    (loop for (key value) on query-params by #'cddr
          for index from 0
          do (when (> index 0)
               (write-char #\& stream))
             (format stream "~A=~A"
                     (%percent-encode-uri-component (%normalize-param-key key))
                     (%percent-encode-uri-component (%stringify-param-value value))))))

(defun %build-endpoint-url (path base-params extra-params)
  "Construct a full API URL with PATH and merged query parameters."
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
  "Build request descriptor plist for One Call endpoint."
  (list :method :get
        :url (apply #'build-onecall-url lat lon query-params)))

(defun make-timemachine-request (lat lon dt &rest query-params)
  "Build request descriptor plist for One Call timemachine endpoint."
  (list :method :get
        :url (apply #'build-timemachine-url lat lon dt query-params)))

(defun make-day-summary-request (lat lon date &rest query-params)
  "Build request descriptor plist for One Call day summary endpoint."
  (list :method :get
        :url (apply #'build-day-summary-url lat lon date query-params)))

(defun make-overview-request (lat lon &rest query-params)
  "Build request descriptor plist for One Call overview endpoint."
  (list :method :get
        :url (apply #'build-overview-url lat lon query-params)))

(defun %default-http-get (url timeout-seconds)
  "Default HTTP GET implementation backed by Dexador."
  (multiple-value-bind (body status)
      (dexador:get url
                   :read-timeout timeout-seconds
                   :connect-timeout timeout-seconds
                   :keep-alive nil)
    (values body status)))

(defun %http-get (url)
  "Execute HTTP GET for URL using the configured transport hook."
  (funcall (or *http-get-function* #'%default-http-get)
           url
           *request-timeout-seconds*))

(defmacro with-http-get-function ((function) &body body)
  "Temporarily override HTTP GET implementation for tests/custom transports."
  `(let ((*http-get-function* ,function))
     ,@body))

(defun %should-retry-status-p (status)
  "Return true if STATUS is considered transient and should be retried."
  (and status (or (= status 429) (>= status 500))))

(defun %parse-response-body (response-body endpoint)
  "Parse JSON RESPONSE-BODY into a plist, signaling parse errors with endpoint context."
  (handler-case
      (jonathan:parse response-body :as :plist)
    (error (err)
      (error 'api-response-parse-error
             :status-code nil
             :endpoint endpoint
             :message (princ-to-string err)))))

(defun %fetch-json (url endpoint)
  "GET URL and decode JSON response, applying retry policy."
  (loop with attempts = 0
        with max-attempts = (1+ *max-retries*)
        do (incf attempts)
           (handler-case
               (multiple-value-bind (body status) (%http-get url)
                 (cond
                   ((and status (= status 200))
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

(defun %fetch-raw (url endpoint)
  "GET URL and return raw response body, applying retry policy."
  (loop with attempts = 0
        with max-attempts = (1+ *max-retries*)
        do (incf attempts)
           (handler-case
               (multiple-value-bind (body status) (%http-get url)
                 (cond
                   ((and status (= status 200))
                    (return body))
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

(defun %execute-json-request (request endpoint)
  "Execute JSON REQUEST descriptor plist and decode response for ENDPOINT."
  (unless (eq (getf request :method) :get)
    (error 'invalid-parameters-error
           :message "Only :GET request descriptors are currently supported."))
  (%fetch-json (getf request :url) endpoint))

(defun %execute-raw-request (request endpoint)
  "Execute raw REQUEST descriptor plist and return response body for ENDPOINT."
  (unless (eq (getf request :method) :get)
    (error 'invalid-parameters-error
           :message "Only :GET request descriptors are currently supported."))
  (%fetch-raw (getf request :url) endpoint))

(defun fetch-onecall (lat lon &rest query-params)
  "Fetch and decode /data/3.0/onecall response as a plist."
  (%execute-json-request (apply #'make-client-weather-request lat lon query-params)
                         :onecall))

(defun fetch-timemachine (lat lon dt &rest query-params)
  "Fetch and decode /data/3.0/onecall/timemachine response as a plist."
  (%execute-json-request (apply #'make-timemachine-request lat lon dt query-params)
                         :timemachine))

(defun fetch-day-summary (lat lon date &rest query-params)
  "Fetch and decode /data/3.0/onecall/day_summary response as a plist."
  (%execute-json-request (apply #'make-day-summary-request lat lon date query-params)
                         :day-summary))

(defun fetch-overview (lat lon &rest query-params)
  "Fetch and decode /data/3.0/onecall/overview response as a plist."
  (%execute-json-request (apply #'make-overview-request lat lon query-params)
                         :overview))
