(in-package :openweathermap)

(defun %compact-geocoding-plist (plist)
  "Return PLIST with key/value pairs containing NIL values removed."
  (loop for (key value) on plist by #'cddr
        when value
          append (list key value)))

(defun %non-empty-string-p (value)
  "Return true when VALUE is a non-empty string."
  (and (stringp value)
       (> (length value) 0)))

(defun build-geocoding-url (query &key limit)
  "Build URL for direct geocoding endpoint."
  (unless (%non-empty-string-p query)
    (error 'invalid-parameters-error
           :message "Direct geocoding requires a non-empty query string."))
  (%build-endpoint-url "/geo/1.0/direct"
                       '()
                       (%compact-geocoding-plist
                        (list :q query
                              :limit limit))))

(defun make-geocoding-request (query &key limit)
  "Build request descriptor plist for direct geocoding endpoint."
  (list :method :get
        :url (build-geocoding-url query :limit limit)))

(defun fetch-geocoding (query &key limit)
  "Fetch and decode direct geocoding response as a plist."
  (%execute-json-request (make-geocoding-request query :limit limit)
                         :geocoding-direct))

(defun build-reverse-geocoding-url (lat lon &key limit)
  "Build URL for reverse geocoding endpoint."
  (unless (and lat lon)
    (error 'invalid-parameters-error
           :message "Reverse geocoding requires both lat and lon."))
  (%build-endpoint-url "/geo/1.0/reverse"
                       '()
                       (%compact-geocoding-plist
                        (list :lat lat
                              :lon lon
                              :limit limit))))

(defun make-reverse-geocoding-request (lat lon &key limit)
  "Build request descriptor plist for reverse geocoding endpoint."
  (list :method :get
        :url (build-reverse-geocoding-url lat lon :limit limit)))

(defun fetch-reverse-geocoding (lat lon &key limit)
  "Fetch and decode reverse geocoding response as a plist."
  (%execute-json-request (make-reverse-geocoding-request lat lon :limit limit)
                         :geocoding-reverse))

(defun build-zip-geocoding-url (zip &key country-code)
  "Build URL for zip geocoding endpoint."
  (unless (%non-empty-string-p zip)
    (error 'invalid-parameters-error
           :message "ZIP geocoding requires a non-empty zip code."))
  (%build-endpoint-url "/geo/1.0/zip"
                       '()
                       (list :zip (if (%non-empty-string-p country-code)
                                      (format nil "~A,~A" zip country-code)
                                      zip))))

(defun make-zip-geocoding-request (zip &key country-code)
  "Build request descriptor plist for ZIP geocoding endpoint."
  (list :method :get
        :url (build-zip-geocoding-url zip :country-code country-code)))

(defun fetch-zip-geocoding (zip &key country-code)
  "Fetch and decode zip geocoding response as a plist."
  (%execute-json-request (make-zip-geocoding-request zip :country-code country-code)
                         :geocoding-zip))
