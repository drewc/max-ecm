(cl:defpackage :max-ecm/request-dispatch
  (:use :cl :max-ecm/request-context
	:max-ecm/protocol-error)
  (:import-from :st-json "GETJSO")
  (:export
   "ENDPOINT-NOT-FOUND"
   "ENDPOINT-NOT-FOUND-ENDPOINT"
   #:define-endpoint
   "ENDPOINT-DISPATCHER"
   #:max-ecm-acceptor))
(cl:in-package :max-ecm/request-dispatch)


;;; Our server protocol is defined partly in terms of HTTP
;;; "endpoints", specified as URLs with segments that contain Row IDs
;;; and other such fields.  Some endpoints allow for multiple HTTP
;;; methods, some only one method.  We match endpoints by way of
;;; regular expressions and then dispatch on the HTTP method to find a
;;; named handler function.


(defvar *endpoints* nil
  "A list of defined protocol endpoint specifiers.")

(define-condition endpoint-not-found (protocol-error)
  ((endpoint :initarg :endpoint :reader endpoint-not-found-endpoint))
  (:report (lambda (condition stream)
	     (format stream "The endpoint ~S is not defined"
		     (endpoint-not-found-endpoint condition)))))

(defmethod protocol-error-http-return-code ((condition endpoint-not-found))
  hunchentoot:+http-not-found+)

(defmethod get-jso/protocol-error :around ((condition endpoint-not-found))
  (let* ((base-result (call-next-method))
	 (error-jso (getjso "error" base-result)))
    (setf (getjso "endpoint" error-jso)
	  (endpoint-not-found-endpoint condition))
    base-result))


;;; An endpoint specifier.

(defclass endpoint ()
  ((name :initarg name :reader endpoint-name)
   (regexp :initarg regexp :reader endpoint-regexp)))

(defmethod print-object ((endpoint endpoint) stream)
  (print-unreadable-object (endpoint stream :type t)
    (print (endpoint-name endpoint) stream)
    (princ #\Space stream)
    (print (endpoint-regexp endpoint) stream)))

(defun make-endpoint (name regexp)
  (make-instance 'endpoint
		 'name name
		 'regexp regexp))

(defun endpoint-function-name (endpoint-name method)
  (let ((function-name-name (format nil "~A/~A" endpoint-name method)))
    (intern function-name-name (symbol-package endpoint-name))))


(defun endpoint-function (endpoint method)
  (or (ignore-errors
	(symbol-function (endpoint-function-name (endpoint-name endpoint) method)))
      (error "No ~A method handler defined for endpoint ~A"
	     method (endpoint-name endpoint))))


;;; Declarative endpoint definition.

(defun %define-endpoint (name regexp)
  ;; The load-time magic for endpoint definition... or redefinition.
  ;; Add the endpoint to our dispatch list if necessary, otherwise
  ;; just update the regexp.
  (let ((endpoint (find name *endpoints* :key #'endpoint-name)))
    (unless endpoint
      (push (setf endpoint (make-endpoint name regexp))
	    *endpoints*))
    (setf (slot-value endpoint 'regexp) regexp)))

(defmacro define-endpoint (name regexp)
  "Define an endpoint for the server protocol.  REGEXP defines the URL
scheme for the endpoint, NAME provides the base for the handler
function names."
  `(progn
     (%define-endpoint ',name ,regexp)
     ',name))


;;; Endpoint dispatch.

(defun endpoint-dispatcher (request)
  (dolist (endpoint *endpoints*)
    (multiple-value-bind
	  (match registers)
	(cl-ppcre:scan-to-strings (endpoint-regexp endpoint)
				  (hunchentoot:script-name request))
      (when match
	(return-from endpoint-dispatcher
	  (with-request-context ()
	    (apply (endpoint-function endpoint
				      (hunchentoot:request-method*))
		   (coerce registers 'list)))))))
  (with-request-context ()
    (error 'endpoint-not-found
	   :endpoint (hunchentoot:script-name request))))


;;; Interface for newer Hunchentoot.

(defclass max-ecm-acceptor (hunchentoot:acceptor)
  ()) ;; NOTE: May need :DEFAULT-INITARGS.

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor max-ecm-acceptor) request)
  (endpoint-dispatcher request))


;;; EOF
