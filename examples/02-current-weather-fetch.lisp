;; SPDX-License-Identifier: MIT
(load (merge-pathnames "common-loader.lisp" *load-truename*))

(in-package :openweathermap)

(let ((api-key (uiop:getenv "OPENWEATHER_API_KEY")))
  (unless (and api-key (> (length api-key) 0))
    (error "Set OPENWEATHER_API_KEY before running this example."))
  (configure-api-key api-key))

(let ((result (fetch-current-weather :q "London" :units :metric :lang "en")))
  (format t "~&City: ~A~%"
          (or (getf result :name) (getf result :|name|)))
  (format t "Temp: ~A~%"
          (or (getf (getf result :main) :temp)
              (and (getf result :|main|) (getf (getf result :|main|) :|temp|)))))
