(in-package :openweathermap-onecall)

(define-condition api-request-error (error)
  ((status-code :initarg :status-code :reader api-request-error-status-code)
   (message :initarg :message :reader api-request-error-message))
  (:report (lambda (condition stream)
             (format stream "API request failed~@[ (status ~A)~]: ~A"
                     (api-request-error-status-code condition)
                     (api-request-error-message condition)))))

(define-condition missing-api-key-error (error)
  ()
  (:report (lambda (_ stream)
             (declare (ignore _))
             (format stream "No OpenWeather API key configured. Use configure-api-key or set OPENWEATHER_API_KEY."))))
