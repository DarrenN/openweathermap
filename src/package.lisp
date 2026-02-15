(defpackage :openweathermap
  (:use :cl)
  (:export
   :*api-base-url*
   :*api-key*
   :*request-timeout-seconds*
   :*max-retries*
   :*retry-backoff-seconds*
   :configure-api-key
   :configure-client
   :with-http-get-function
   :missing-api-key-error
   :api-request-error
   :api-network-error
   :api-response-parse-error
   :build-onecall-url
   :build-timemachine-url
   :build-day-summary-url
   :build-overview-url
   :make-client-weather-request
   :make-timemachine-request
   :make-day-summary-request
   :make-overview-request
   :fetch-onecall
   :fetch-timemachine
   :fetch-day-summary
   :fetch-overview
   :ensure-api-key))
