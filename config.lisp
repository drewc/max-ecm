(cl:defpackage :max-ecm/config
  (:use :cl)
  (:import-from :max-ecm/database-interface
		#:*database-connection-parameters*)
    (:import-from :max-ecm/url-scheme-variables
		  #:*server-public-url*))
(cl:in-package :max-ecm/config)

(defparameter *database-connection-parameters* 
  '("maxclaims" "maxclaims" "y3am2e!!" "localhost"))

(defparameter *server-public-url* "http://webinsure.maxwellclaims.net:8042")

