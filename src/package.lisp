(defpackage :openweathermap-onecall
  (:use :cl)
  (:export
   :*api-base-url*
   :*api-key*
   :configure-api-key
   :missing-api-key-error
   :api-request-error
   :build-onecall-url
   :make-client-weather-request
   :ensure-api-key))
