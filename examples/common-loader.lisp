(let ((quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-setup)
    (load quicklisp-setup)))

(require :asdf)
(asdf:load-asd (truename "openweathermap.asd"))

(handler-case
    (asdf:load-system :openweathermap)
  (error (err)
    (error "Unable to load system :openweathermap. Ensure Quicklisp dependencies are installed (dexador, jonathan). Original error: ~A"
           err)))
