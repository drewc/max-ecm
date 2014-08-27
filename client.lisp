(cl:defpackage :max-ecm/client
  (:documentation
   "This is the interaction with the JSON RCP")
  (:use :cl)
  (:import-from :max-ecm/json
		#:read-json
		#:write-json-to-string
		#:jso
		#:getjso)
  (:import-from :puri)
  (:import-from :drakma
		#:http-request)
  (:export #:*rpc-uri* 
	   #:rpc-request
	   #:jso-rpc))
(cl:in-package :max-ecm/client)

(defvar *rpc-uri* 
  (puri:uri "http://localhost:8042")
  "TODO: This should be set in the conf.json")

(defparameter *rpc-version* "/v1/")

(defun rpc-uri (uri &key (version *rpc-version*))
  (puri:merge-uris 
   uri (puri:merge-uris version *rpc-uri*)))
  
(defvar *http-user* 
  '("maxclaims" 
    "foobar"))

(defun rpc-request (path 
		    &key parameters data
		      (method :post)
		      (auth :http-user)
		      (rpc-uri *rpc-uri*))
  (multiple-value-bind (string status)
      (apply #'http-request
	     (puri:merge-uris path rpc-uri)
	     :method method
	     :parameters parameters
	     `( ,@(when data
			(list :content data))
		  ,@ (if (eql auth :http-user)
			 (list :basic-authorization *http-user*)
			 (when auth 
			   (list :additional-headers `(("Authorization"
							. ,(concatenate
							    'string "Bearer " auth))))))))
    (values (typecase string
	      (string (read-json string))
	      (vector 
	       (read-json (flexi-streams:octets-to-string 
			   string  :external-format :utf8))))
	    status)))

(defvar *access-token* "1-N")

(defun jso-rpc (path jso 
		&key 
		  (access-token *access-token*)
		  (http-user NIL)
		  (version *rpc-version*)
		  parameters)
  (let ((*http-user* http-user))
    (apply #'rpc-request 
	   (rpc-uri path :version version)
	   :auth (if http-user 
		     :http-user
		     access-token)
	   `(,@ (if jso 
		    (list :data (write-json-to-string jso))
		    (list :method :get))
		,@ (when parameters 
		     (list :parameters parameters))))))

