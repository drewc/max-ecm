(defpackage :max-ecm/gnumeric/ssconvert
  (:use :cl)
  (:import-from #:uiop)
  (:export #:ssconvert-export-types
	   #:ssconvert-type-name
	   #:ssconvert-type-value
	   #:ssconvert-type-mime-type
	   #:ssconvert-type-pathname-type
	   #:find-ssconvert-type
	   #:ssconvert-type))

(in-package :max-ecm/gnumeric/ssconvert)

(defclass ssconvert-type ()
  ((name :initarg :name
	 :reader ssconvert-type-name)
   (value :initarg :value
	 :reader ssconvert-type-value)
   (mime-type :initarg :mime-type
	      :reader ssconvert-type-mime-type)
   (pathname-type :initarg :pathname-type
		  :reader ssconvert-type-pathname-type)))

(defun make-ssconvert-type (&key name value
			      (mime-type "application/octet-stream")
			      (pathname-type "bin"))
  (make-instance 'ssconvert-type
		 :name name
		 :value value
		 :mime-type mime-type
		 :pathname-type pathname-type))

(defparameter *ssconvert-export-types* nil)

(defun ssconvert-export-types ()
  (or *ssconvert-export-types* 
      (setf *ssconvert-export-types*
	    (loop :for plist :in *ssconvert-export-types-as-plists*
	       :collect (apply #'make-ssconvert-type plist)))))

(defun find-ssconvert-type (&key name value)
  (find (or value name) (ssconvert-export-types)
	:key (if value
		 #'ssconvert-type-value
		 #'ssconvert-type-name)
	:test #'string=))
		 

(defmethod print-object ((object ssconvert-type) s)
  (print-unreadable-object (object s :type t) 
    (format s "~A ~A ~A"
	    (ssconvert-type-value object)	    
	    (ssconvert-type-mime-type object)
	    (ssconvert-type-name object))))
  
(defparameter *ssconvert-export-types-as-plists* 
  '((:NAME "[MS Excel™ 2010] ; ISO/IEC 29500:2008 & ECMA 376 2nd edition (2008)"
     :VALUE "Gnumeric_Excel:xlsx2"
     :mime-type "application/vnd.ms-excel"
     :pathname-type "xlsx")
    (:NAME "[MS Excel™ 2007] ; ECMA 376 1st edition (2006); " 
     :VALUE "Gnumeric_Excel:xlsx"
     :mime-type "application/vnd.ms-excel"
     :pathname-type "xlsx")
    (:NAME "PDF export" 
     :VALUE "Gnumeric_pdf:pdf_assistant"
     :mime-type "application/pdf"
     :pathname-type "pdf")
    (:NAME "Comma separated values (CSV)" 
     :VALUE "Gnumeric_stf:stf_csv"
     :pathname-type "csv"
     :mime-type "text/comma-separated-values")
    (:NAME "HTML 4.0 (*.html)" 
     :VALUE "Gnumeric_html:html40"
     :mime-type "text/html"
     :pathname-type "html")
    (:NAME "[MS Excel™ 2007] ; ECMA 376 1st edition (2006); " 
     :VALUE "Gnumeric_Excel:xlsx"
     :mime-type "application/vnd.ms-excel"
     :pathname-type "xlsx")
    (:NAME "MS Excel™ 97/2000/XP & 5.0/95" 
     :VALUE "Gnumeric_Excel:excel_dsf"
     :mime-type "application/vnd.ms-excel"
     :pathname-type "xls")
    (:NAME "MS Excel™ 5.0/95" 
     :VALUE "Gnumeric_Excel:excel_biff7"
     :mime-type "application/vnd.ms-excel"
     :pathname-type "xls")
    (:NAME "MS Excel™ 97/2000/XP" 
     :VALUE "Gnumeric_Excel:excel_biff8"
     :mime-type "application/vnd.ms-excel"
     :pathname-type "xlsx")
    (:NAME "ODF 1.2 extended conformance (*.ods)" 
     :VALUE "Gnumeric_OpenCalc:odf"
     :pathname-type "ods"
     :mime-type "application/vnd.oasis.opendocument.spreadsheet")
    (:NAME "ODF 1.2 strict conformance (*.ods)" 
     :VALUE "Gnumeric_OpenCalc:openoffice"
     :pathname-type "ods"
     :mime-type "application/vnd.oasis.opendocument.spreadsheet")
    (:NAME "LPSolve Linear Program Solver" 
     :VALUE "Gnumeric_lpsolve:lpsolve")
    (:NAME "TROFF (*.me)" 
     :VALUE "Gnumeric_html:roff"
     :pathname-type "me"
     :mime-type "application/x-troff-me")
    (:NAME "LaTeX 2e (*.tex) table fragment" 
     :VALUE "Gnumeric_html:latex_table"
     :pathname-type "tex"
     :mime-type "text/x-tex")
    (:NAME "LaTeX 2e (*.tex)" 
     :VALUE "Gnumeric_html:latex"
     :pathname-type "tex"
     :mime-type "text/x-tex")
    (:NAME "XHTML range - for export to clipboard" 
     :VALUE "Gnumeric_html:xhtml_range"
     :mime-type "text/html"
     :pathname-type "html")
    (:NAME "XHTML (*.html)" :VALUE "Gnumeric_html:xhtml"
     :mime-type "text/html"
     :pathname-type "html")
    (:NAME "HTML (*.html) fragment" :VALUE "Gnumeric_html:html40frag"
     :mime-type "text/html"
     :pathname-type "html")
    (:NAME "HTML 4.0 (*.html)" :VALUE "Gnumeric_html:html40"
     :mime-type "text/html"
     :pathname-type "html")
    (:NAME "HTML 3.2 (*.html)" :VALUE "Gnumeric_html:html32"
     :mime-type "text/html"
     :pathname-type "html")
    (:NAME "MultiPlan (SYLK)" :VALUE "Gnumeric_sylk:sylk")
    (:NAME "GLPK Linear Program Solver" :VALUE "Gnumeric_glpk:glpk")
    (:NAME "Text (configurable)" :VALUE "Gnumeric_stf:stf_assistant")
    (:NAME "Gnumeric XML uncompressed (*.xml)" :VALUE "Gnumeric_XmlIO:sax:0"
     :mime-type "application/x-gnumeric"
     :pathname-type "xml")
    (:NAME "Gnumeric XML (*.gnumeric)" :VALUE "Gnumeric_XmlIO:sax"
     :mime-type "application/x-gnumeric"
     :pathname-type "gnumeric")
    (:NAME "Data Interchange Format (*.dif)" 
     :VALUE "Gnumeric_dif:dif")))
