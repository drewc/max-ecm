(cl:defpackage :max-ecm/config
  (:use :cl)
  (:import-from :max-ecm/database-interface
		#:*database-connection-parameters*)
  (:import-from :max-ecm/url-scheme-variables
		#:*server-public-url*)
  (:import-from :max-ecm/mail
		#:*mail-authentication*))
(cl:in-package :max-ecm/config)

(defparameter *mail-authentication*
  '("maxclaims@drewc.ca" "y3am2e!!"))

(defparameter *database-connection-parameters* 
  '("maxclaims" "maxclaims" "y3am2e!!" "localhost"))

(defparameter *server-public-url* "http://webinsure.maxwellclaims.net:8042")

