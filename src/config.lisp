;; SPDX-License-Identifier: MIT
(in-package :openweathermap)

(defparameter *api-base-url* "https://api.openweathermap.org")
(defparameter *maps-base-url* "https://tile.openweathermap.org")
(defparameter *api-key* nil)
(defparameter *request-timeout-seconds* 10)
(defparameter *max-retries* 2)
(defparameter *retry-backoff-seconds* 1)

(defun configure-api-key (api-key)
  "Set API key used for OpenWeather requests."
  (setf *api-key* api-key))

(defun configure-client (&key api-base-url maps-base-url request-timeout-seconds max-retries retry-backoff-seconds)
  "Configure HTTP behavior and base URL for the client."
  (when api-base-url
    (setf *api-base-url* api-base-url))
  (when maps-base-url
    (setf *maps-base-url* maps-base-url))
  (when request-timeout-seconds
    (setf *request-timeout-seconds* request-timeout-seconds))
  (when max-retries
    (setf *max-retries* max-retries))
  (when retry-backoff-seconds
    (setf *retry-backoff-seconds* retry-backoff-seconds))
  t)

(defun ensure-api-key ()
  "Return configured API key or signal missing-api-key-error."
  (or *api-key*
      (uiop:getenv "OPENWEATHER_API_KEY")
      (error 'missing-api-key-error)))
