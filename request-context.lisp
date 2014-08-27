(cl:defpackage :max-ecm/request-context
  (:use :cl
	:max-ecm/protocol-error
	:max-ecm/database-interface)
  (:import-from :max-ecm/json
		"WRITE-JSON-TO-STRING")
  (:import-from :flexi-streams
		"MAKE-EXTERNAL-FORMAT")
  (:export
   "*JSON-MIME-TYPE*"
   "*UTF-8*"
   "SEND-REQUEST-RESPONSE"
   "SEND-JSON-RESPONSE"
   "CALL-WITH-REQUEST-CONTEXT"
   "WITH-REQUEST-CONTEXT"))
(cl:in-package :max-ecm/request-context)

;; Define all of our parameters as variables now so that we can refer
;; to them before we have defined all of the data structures that they
;; are to contain.
(defparameter *json-mime-type* "application/json")

;; We need an external-format object in order to produce UTF-8 output.
(defvar *utf-8* (make-external-format :utf-8 :eol-style :lf))


(defun send-request-response (mime-type body &key return-code external-format)
  (setf (hunchentoot:content-type*) mime-type)
  (when return-code
    (setf (hunchentoot:return-code*) return-code))
  (when external-format
    (setf (hunchentoot:reply-external-format*) external-format))
  (throw 'exit-call-with-request-context body))

(defun send-json-response (json &key return-code)
  (send-request-response *json-mime-type* (write-json-to-string json)
			 :return-code return-code
			 :external-format *utf-8*))

(defun log-request-error (condition)
  (declare (ignore condition))
  #+(or) ;; XXX Disabled due to too many dependencies.
  (with-logfile (stream :request-error)
    (format stream "Error at universal-time ~D:~%~%"
	    (get-universal-time))
    (print (get-jso/protocol-error condition) stream)
    (format stream "~%~%HEADERS-IN~16T= ~S~%METHOD~16T= ~S~%URI~16T= ~S~%"
	    (hunchentoot:headers-in*) (hunchentoot:request-method*)
	    (hunchentoot:request-uri*))
    (format stream "SERVER-PROTOCOL~16T= ~S~%REMOTE-ADDR~16T= ~S~%"
	    (hunchentoot:server-protocol*) (hunchentoot:remote-addr*))
    (format stream "REMOTE-PORT~16T= ~S~%COOKIES-IN~16T= ~S~%"
	    (hunchentoot:remote-port*) (hunchentoot:cookies-in*))
    (format stream "GET-PARAMETERS~16T= ~S~%POST-PARAMETERS~16T= ~S~%"
	    (hunchentoot:get-parameters*) (hunchentoot:post-parameters*))
    (format stream "SCRIPT-NAME~16T= ~S~%QUERY-STRING~16T= ~S~%~%"
	    (hunchentoot:script-name*) (hunchentoot:query-string*))
    (sb-debug:backtrace most-positive-fixnum stream)
    (format stream "~%End error report.~%")))

(defun handle-request-error (condition)
  (log-request-error condition)
  (protocol-error-set-http-response-headers condition)
  (send-json-response (get-jso/protocol-error condition)
		      :return-code (protocol-error-http-return-code
				    condition)))

(defun call-with-request-context (thunk)
  (catch 'exit-call-with-request-context
    (handler-bind
	((error #'handle-request-error))
      (with-database ()
	(funcall thunk)))))

(defmacro with-request-context (nil &body body)
  (let ((thunk-name (gensym "WITH-REQUEST-CONTEXT-THUNK-")))
    `(flet ((,thunk-name ()
	      ,@body))
       (declare (dynamic-extent #',thunk-name))
       (call-with-request-context #',thunk-name))))

;;; EOF
