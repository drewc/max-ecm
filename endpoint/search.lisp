(cl:defpackage :max-ecm/endpoint/search
  (:use :cl :max-ecm/entity)
  (:import-from :max-ecm/request-dispatch
		#:define-endpoint)
  (:import-from :max-ecm/request-context
		#:send-json-response)
  (:import-from :max-ecm/request-utils
		#:get-post-body-as-jso
		#:add-paged-result-envelope
		#:with-paged-result-context
		#:add-json-envelope)
  (:import-from  :max-ecm/json
		 #:jso #:getjso)
  (:export))
(cl:in-package :max-ecm/endpoint/search)

(define-endpoint search-jso "^/v1/search")

(defun search-handler ()    
  (let* ((jso (get-post-body-as-jso)))
    (multiple-value-bind (result number) 
	(search/jso jso)
      (send-json-response 
       (add-paged-result-envelope  
	(mapcar #'object-jso result) "" (list) number)))))
  
(defun search-jso/post ()
  (search-handler))


  
  
