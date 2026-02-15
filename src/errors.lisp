(in-package :openweathermap)

(define-condition api-request-error (error)
  ((status-code :initarg :status-code :reader api-request-error-status-code)
   (message :initarg :message :reader api-request-error-message)
   (endpoint :initarg :endpoint :reader api-request-error-endpoint))
  (:report (lambda (condition stream)
             (format stream "API request failed~@[ (status ~A)~]~@[ at ~A~]: ~A"
                     (api-request-error-status-code condition)
                     (api-request-error-endpoint condition)
                     (api-request-error-message condition)))))

(define-condition api-network-error (api-request-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Network failure while calling~@[ ~A~]: ~A"
                     (api-request-error-endpoint condition)
                     (api-request-error-message condition)))))

(define-condition api-response-parse-error (api-request-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unable to parse API response~@[ from ~A~]: ~A"
                     (api-request-error-endpoint condition)
                     (api-request-error-message condition)))))

(define-condition missing-api-key-error (error)
  ()
  (:report (lambda (_ stream)
             (declare (ignore _))
             (format stream "No OpenWeather API key configured. Use configure-api-key or set OPENWEATHER_API_KEY."))))
