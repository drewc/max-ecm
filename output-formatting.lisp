(cl:defpackage :max-ecm/output-formatting
  (:use :cl)
  (:import-from :simple-date
		#:decode-timestamp
		#:universal-time-to-timestamp)
  (:export
   #:timestamp->iso8601
   #:now->iso8601
   #:price->string))
(cl:in-package :max-ecm/output-formatting)

(defun timestamp->iso8601 (timestamp)
  (if (stringp timestamp)
      timestamp
      (multiple-value-bind
	    (year month day hour minute second)
	  (decode-timestamp timestamp)
	(format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
		year month day hour minute second))))

(defun now->iso8601 ()
  (timestamp->iso8601
   (universal-time-to-timestamp
    (get-universal-time))))

(defun price->string (price)
  (declare (type (or string ratio integer (member :null)) price))
  (if (or (stringp price)
	  (eq :null price))
      price
      (multiple-value-bind
	    (quotient remainder)
	  (floor (* price 100) 100)
	(format nil "~D.~2,'0D" quotient remainder))))
