(defpackage :max-ecm/gnumeric/document
  (:use :cl)
  (:import-from :cxml)
  (:import-from :stp)
  (:export #:*empty-gnumeric-document*
	   #:empty-document
	   #:document))

(in-package :max-ecm/gnumeric/document)

(defparameter *empty-gnumeric-xml-pathname*
  (merge-pathnames 
   "gnumeric/empty"
   (asdf:system-source-directory 
    :max-ecm)))
  
(defparameter *empty-gnumeric-document* 
  (cxml:parse *empty-gnumeric-xml-pathname*
	      (stp:make-builder)))

(defun empty-document ()
  (stp:copy *empty-gnumeric-document*))

(defun document (&key (pathname *empty-gnumeric-xml-pathname*))
  (stp:copy 
    (cxml:parse pathname
		(stp:make-builder))))






					 


   

