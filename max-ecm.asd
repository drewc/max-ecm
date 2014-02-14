;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#-asdf3 (progn 
	  #.`(
	      #+quicklisp ,@'(progn
			      (print "It is in : ~%
 (merge-pathnames \"quicklisp/local-projects/asdf/\"
		  (user-homedir-pathname)), ~%
So (asdf:upgrade-asdf) and (ql:quickload :asdf).~%
")			  #-quicklisp error	     
			      "PWAP requires ASDF 3 or later. Please upgrade your ASDF.")
	  (let ((asdf:*central-registry* 
		 (list (merge-pathnames "quicklisp/local-projects/asdf/"
					(user-homedir-pathname)))))
	    (asdf:upgrade-asdf) (ql:quickload :asdf))))
	  
(asdf:defsystem :max-ecm
  :description "Max Effective Claims Manager"
  :long-description ""
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:pwap))

(asdf:register-system-packages 
 :cxml 
 '(:sax :runes))

(asdf:register-system-packages 
 :st-json
 '(:st-json))

(asdf:register-system-packages 
 :cxml-stp 
 '(:cxml-stp :stp))


