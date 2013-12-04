(defpackage :max-ecm/gnumeric/xml
  (:use :cl)
  (:import-from :cxml)
  (:import-from :stp)
  (:import-from :pwap/yasexml/tag
		#:<>)
  (:export #:<>
	   #:of-name
	   #:adopt-children
	   #:<gnm>))

(in-package :max-ecm/gnumeric/xml)

(defun of-name (name)
  (stp:of-name name "http://www.gnumeric.org/v10.dtd"))

(defmacro <gnm> (&body body)
  `(stp:copy 
    (stp:document-element 
     (<> (:handler (stp:make-builder))
       (<> (:document) 
	 (<> (:xmlns "gnm" "http://www.gnumeric.org/v10.dtd")
	   ,@body))))))

(defun adopt-children (adopter adoptee &key (using #'stp:append-child))
  (when adoptee
    (stp:do-children (c adoptee)      
      (funcall using adopter (stp:copy c)))))
					 


   

