(defpackage :max-ecm/gnumeric/document
  (:use :cl)
  (:import-from :cxml)
  (:import-from :stp)
  (:export #:*empty-gnumeric-document*
	   #:empty-document))

(in-package :max-ecm/gnumeric/document)

(defparameter *empty-gnumeric-document* 
  (cxml:parse (merge-pathnames 
	       "gnumeric/empty"
	       (asdf:system-source-directory 
		:max-ecm))
	      (stp:make-builder)))

(defun empty-document ()
  (stp:copy *empty-gnumeric-document*))
					 


   

