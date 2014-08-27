;;;
;;; protocol-error.lisp
;;;
;;; Error handling for the server protocol.
;;;

(cl:defpackage :max-ecm/protocol-error
  (:use :cl)
  (:import-from :max-ecm/json "JSO")
  (:import-from :hunchentoot "+HTTP-INTERNAL-SERVER-ERROR+" "+HTTP-BAD-REQUEST+")
  (:export
   "PROTOCOL-ERROR"
   "GET-JSO/PROTOCOL-ERROR"
   "PROTOCOL-ERROR-HTTP-RETURN-CODE"
   "PROTOCOL-ERROR-SET-HTTP-RESPONSE-HEADERS"))
(cl:in-package :max-ecm/protocol-error)


(define-condition protocol-error (error)
  ())

(defgeneric get-jso/protocol-error (condition))

(defmethod get-jso/protocol-error ((condition condition))
  (let* ((condition-class (class-of condition))
	 (superclasses (sb-mop:class-precedence-list condition-class))
	 ;; Truncate the superclass list after ERROR
	 (superclasses (subseq superclasses 0
			       (1+ (position (find-class 'error)
					     superclasses)))))
    (jso "error"
	 (jso "message" (format nil "~A" condition)
	      "class" (format nil "~A" (class-name condition-class))
	      "superclasses" (map 'list (lambda (class)
					  (format nil "~A"
						  (class-name class)))
				  (cdr superclasses))))))

(defgeneric protocol-error-http-return-code (condition))

(defmethod protocol-error-http-return-code ((condition condition))
  hunchentoot:+http-internal-server-error+)

(defmethod protocol-error-http-return-code ((condition protocol-error))
  hunchentoot:+http-bad-request+)


(defgeneric protocol-error-set-http-response-headers (condition))

(defmethod protocol-error-set-http-response-headers ((condition condition))
  ;; Do nothing, this is a hook for errors with return codes like 401,
  ;; which are required to set headers.
  )

;;; EOF
