(in-package :openweathermap-onecall/tests)

(test configure-api-key-stores-value
  (let ((openweathermap-onecall:*api-key* nil))
    (openweathermap-onecall:configure-api-key "abc123")
    (is (string= "abc123" openweathermap-onecall:*api-key*))))
