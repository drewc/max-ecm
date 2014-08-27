(cl:defpackage :max-ecm/endpoint/update
  (:use :cl :max-ecm/entity)
  (:import-from :max-ecm/entity/user
		#:call-with-user)
  (:import-from :max-ecm/request-dispatch
		#:define-endpoint)
  (:import-from :max-ecm/request-context
		#:send-json-response)
  (:import-from :max-ecm/request-utils
		#:get-post-body-as-jso
		#:add-json-envelope)
  (:import-from  :max-ecm/json
		 #:jso #:getjso)
  (:export))
(cl:in-package :max-ecm/endpoint/update)

(define-endpoint update "^/v1/update")

(defun update-handler-without-user ()
  (let* ((jso (get-post-body-as-jso))
	 (type (getjso "type" jso))
	 (id (getjso "id" jso))
	 (record (find-record type id))
	 (new-record (update-record/jso record jso)))
    (send-json-response 
     (add-json-envelope (object-jso new-record)))))

(defun update-handler ()
  (multiple-value-bind (u p)
      (hunchentoot:authorization)
  (let* ((jso (get-post-body-as-jso))
	 (type (getjso "type" jso))
	 (id (getjso "id" jso))
	 (record (find-record type id))
	 (new-record 
	  (call-with-user (lambda (user)
			    (declare (ignore user))
			    (update-record/jso record jso)) 
			  u p)))
    (send-json-response 
     (add-json-envelope (object-jso new-record))))))

(defun update/post ()
  (update-handler))


  
  
