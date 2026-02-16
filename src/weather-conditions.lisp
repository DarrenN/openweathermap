;; SPDX-License-Identifier: MIT

(in-package :openweathermap)

(defvar *weather-condition-index* nil
  "Hash table mapping weather condition ID to canonical condition plist.")

(defvar *weather-icon-key-index* nil
  "Hash table mapping icon key prefix (e.g. \"10\") to icon-set plist.")

(defun %normalize-key-name (key)
  "Normalize plist keys to lowercase names for tolerant lookup."
  (string-downcase
   (etypecase key
     (keyword (symbol-name key))
     (string key)
     (symbol (symbol-name key)))))

(defun %plist-value (plist key)
  "Read KEY from PLIST using case-insensitive key-name matching."
  (let ((target (%normalize-key-name key)))
    (loop for (k v) on plist by #'cddr
          when (string= target (%normalize-key-name k))
            do (return v))))

(defun %build-weather-condition-index ()
  "Build lookup table for canonical weather conditions by numeric ID."
  (let ((index (make-hash-table :test #'eql)))
    (dolist (row *weather-condition-catalog* index)
      (setf (gethash (getf row :id) index) row))))

(defun %build-weather-icon-key-index ()
  "Build lookup table for icon sets by icon key prefix."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (row *weather-condition-icons* index)
      (setf (gethash (getf row :key) index) row))))

(defun %ensure-weather-indexes ()
  "Initialize weather condition and icon indexes lazily."
  (unless *weather-condition-index*
    (setf *weather-condition-index* (%build-weather-condition-index)))
  (unless *weather-icon-key-index*
    (setf *weather-icon-key-index* (%build-weather-icon-key-index))))

(defun %icon-key-from-code (icon-code)
  "Extract icon key prefix from ICON-CODE (e.g. \"10d\" => \"10\")."
  (when (and (stringp icon-code) (>= (length icon-code) 2))
    (subseq icon-code 0 2)))

(defun %digit-char-p* (ch)
  "Return true when CH is an ASCII digit."
  (and (characterp ch)
       (<= (char-code #\0) (char-code ch))
       (<= (char-code ch) (char-code #\9))))

(defun %valid-icon-code-p (icon-code)
  "Return true when ICON-CODE has expected OpenWeather icon shape."
  (and (stringp icon-code)
       (= (length icon-code) 3)
       (%digit-char-p* (char icon-code 0))
       (%digit-char-p* (char icon-code 1))
       (member (char-downcase (char icon-code 2)) '(#\d #\n))))

(defun %normalize-icon-size (size)
  "Normalize icon SIZE input to 1, 2, or 4."
  (cond
    ((or (null size) (eql size :2x) (eql size 2)) 2)
    ((or (eql size :1x) (eql size 1)) 1)
    ((or (eql size :4x) (eql size 4)) 4)
    (t
     (error 'invalid-parameters-error
            :message "Icon size must be one of :1x, :2x, :4x, 1, 2, or 4."))))

(defun lookup-weather-condition (condition-id)
  "Return canonical weather condition plist for CONDITION-ID, or NIL if unknown."
  (%ensure-weather-indexes)
  (and (integerp condition-id)
       (let ((row (gethash condition-id *weather-condition-index*)))
         (and row (copy-list row)))))

(defun weather-icon-url (icon-code &key size (secure-p t))
  "Build icon asset URL for ICON-CODE using OpenWeather icon conventions."
  (unless (%valid-icon-code-p icon-code)
    (error 'invalid-parameters-error
           :message "Icon code must match NN[d|n], e.g. 10d or 01n."))
  (let* ((normalized-size (%normalize-icon-size size))
         (scheme (if secure-p "https" "http")))
    (if (= normalized-size 1)
        (format nil "~A://openweathermap.org/payload/api/media/file/~A.png"
                scheme
                icon-code)
        (format nil "~A://openweathermap.org/payload/api/media/file/~A@~Ax.png"
                scheme
                icon-code
                normalized-size))))

(defun resolve-weather-condition (&key id icon description main)
  "Merge payload weather fields with canonical condition/icon metadata."
  (%ensure-weather-indexes)
  (let* ((canonical (lookup-weather-condition id))
         (canonical-icon (and canonical (getf canonical :icon)))
         (resolved-icon (or icon canonical-icon))
         (icon-key (%icon-key-from-code resolved-icon))
         (icon-set (and icon-key (gethash icon-key *weather-icon-key-index*)))
         (icon-day (and icon-set (getf icon-set :day)))
         (icon-night (and icon-set (getf icon-set :night)))
         (canonical-description (and canonical (getf canonical :description)))
         (condition-group (and canonical (getf canonical :group)))
         (final-icon (or resolved-icon icon-day))
         (icon-url (and final-icon
                        (%valid-icon-code-p final-icon)
                        (weather-icon-url final-icon))))
    (list :id id
          :main (or main condition-group)
          :description (or description canonical-description)
          :canonical-description canonical-description
          :icon final-icon
          :icon-day icon-day
          :icon-night icon-night
          :icon-url icon-url
          :condition-group condition-group
          :unknown-condition-p (and (integerp id) (null canonical)))))

(defun enrich-weather-entry (weather-entry)
  "Enrich one API weather object plist with canonical condition metadata."
  (resolve-weather-condition
   :id (%plist-value weather-entry :id)
   :icon (%plist-value weather-entry :icon)
   :description (%plist-value weather-entry :description)
   :main (%plist-value weather-entry :main)))

(defun enrich-weather-list (weather-list)
  "Enrich each weather object in WEATHER-LIST using canonical metadata."
  (mapcar #'enrich-weather-entry weather-list))
