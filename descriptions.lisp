(uiop:define-package :max-ecm/descriptions
    (:use  :cl :descriptions
	   :max-ecm/json)
    (:export #:{record}
	     #:type
	     #:id
	     #:object-attribute-value
	     #:=>jso)
  (:reexport :descriptions))

(cl:in-package :max-ecm/descriptions)

(define-attribute =>jso (=>valued) ())

(define-description {record} ()
  ((type (=> (=>string =>view))
	 :label "type")
   (id (=> (=>integer =>view))
       :label "id")))

(define-description {user} ({record})
  ((username (=> =>string))))


(defun lisp->json (form)
  (substitute #\_ #\- (string-downcase (string form))))

(defgeneric object-attribute-value (object attribute)
    (:method (object attribute)
      (let ((value (attribute-value attribute)))
	(if (undefinedp value)
	    (let ((reader (attribute-reader attribute)))
	      (if (undefinedp reader)
		  reader
		  (funcall reader object))))))
  (:method ((object jso) attribute)
    (let ((value (call-next-method)))
      (if (undefinedp value)
	  (multiple-value-bind (v ?) 
	      (getjso (lisp->json (attribute-name attribute)) object)
	    (if ? 
		v
		value))))))





		   

