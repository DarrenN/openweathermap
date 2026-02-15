(in-package :openweathermap-onecall)

(defparameter *api-base-url* "https://api.openweathermap.org")
(defparameter *api-key* nil)

(defun configure-api-key (api-key)
  "Set API key used for OpenWeather requests."
  (setf *api-key* api-key))

(defun ensure-api-key ()
  "Return configured API key or signal missing-api-key-error."
  (or *api-key*
      (uiop:getenv "OPENWEATHER_API_KEY")
      (error 'missing-api-key-error)))
