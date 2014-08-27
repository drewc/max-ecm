(cl:defpackage :max-ecm/server
  (:use :cl)
  (:import-from :max-ecm/config)
  
  (:import-from :max-ecm/request-dispatch
		#:max-ecm-acceptor)
  (:import-from :max-ecm/request-context)
  (:import-from :max-ecm/endpoint/search)
  (:import-from :max-ecm/endpoint/update)
  (:import-from :max-ecm/entity/claim-transaction)
  (:import-from :max-ecm/entity/person)

  (:import-from :hunchentoot)
  (:export
   #:start))
(cl:in-package :max-ecm/server)

(defun start-local-web-listener (&key (port 8042))
  (hunchentoot:start
   (make-instance 'max-ecm-acceptor
		  :port port)))

(defun start ()
  (start-local-web-listener))
