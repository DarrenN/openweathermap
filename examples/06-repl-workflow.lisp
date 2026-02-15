(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(defun setup-from-env ()
  "Configure API key from OPENWEATHER_API_KEY and return T on success."
  (let ((api-key (uiop:getenv "OPENWEATHER_API_KEY")))
    (when (and api-key (> (length api-key) 0))
      (configure-api-key api-key)
      t)))

(defun quick-url-checks ()
  "Generate a few URLs to validate query construction while in REPL."
  (list
   (build-current-weather-url :q "London" :units :metric)
   (build-geocoding-url "New York" :limit 1)
   (build-onecall-url 40.7128 -74.0060 :exclude "minutely,alerts")
   (build-weather-tile-url :temp_new 3 2 3 :opacity 0.9)))

(defun quick-live-smoke ()
  "Run minimal live calls from REPL after setup-from-env."
  (unless *api-key*
    (error "Call setup-from-env first (or configure-api-key manually)."))
  (list
   :current (fetch-current-weather :q "London" :units :metric)
   :geocoding (fetch-geocoding "New York" :limit 1)
   :onecall (fetch-onecall 40.7128 -74.0060 :exclude "minutely,alerts")
   :tile-size (length (fetch-weather-tile :temp_new 3 2 3 :opacity 0.9))))

(format t "~&REPL workflow loaded in package OPENWEATHERMAP.~%")
(format t "Functions available: setup-from-env, quick-url-checks, quick-live-smoke~%")
(format t "This file is intended to be loaded with --load for an interactive REPL session.~%")
(format t "Example REPL steps:~%")
(format t "  (setup-from-env)~%")
(format t "  (quick-url-checks)~%")
(format t "  (quick-live-smoke)~%")
