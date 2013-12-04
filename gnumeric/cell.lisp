(defpackage :max-ecm/gnumeric/cell
  (:use :cl)
  (:import-from :cxml)
  (:import-from :stp)
  (:import-from :max-ecm/gnumeric/xml
		#:<>
		#:of-name
		#:adopt-children
		#:<gnm>)
  (:export ;:import-from :max-ecm/gnumeric/cell 
   #:<cell>
   #:cell-value-type
   #:cell-value-format
   #:cell-row
   #:cell-column))

(in-package :max-ecm/gnumeric/cell)

(define-symbol-macro $types 
    '((empty . 10)
      (boolean . 20)
      (integer . 30)
      (float . 40)
      (error . 50)
      (string . 60)
      (cellrange . 70)
      (array . 80)))

(defgeneric cell-value-type (thing)
    (:method ((name symbol))
      "<xs:simpleType name=\"ValueType\">
        <xs:restriction base=\"xs:string\">
            <xs:enumeration value=\"10\"/> <!-- empty     -->
            <xs:enumeration value=\"20\"/> <!-- boolean   -->
            <xs:enumeration value=\"30\"/> <!-- integer   -->
            <xs:enumeration value=\"40\"/> <!-- float     -->
            <xs:enumeration value=\"50\"/> <!-- error     -->
            <xs:enumeration value=\"60\"/> <!-- string    -->
            <xs:enumeration value=\"70\"/> <!-- cellrange -->
            <xs:enumeration value=\"80\"/> <!-- array     -->
        </xs:restriction>
    </xs:simpleType>"
      (or (cdr (assoc name $types :test #'string-equal))
	  (error "No type for ~A" name)))
    (:method ((number integer))
      (or (car (rassoc number $types))
	  (error "No type for ~A" number)))
    (:method ((cell stp:node))
      (cell-value-type 
       (parse-integer 
	(stp:attribute-value cell "ValueType")))))

(defun <cell> (value &key 
		       (row (error "must provide row"))
		       (column (error "must provide column"))
		       (value-type (cell-value-type 'string))
		       (value-format nil)
		       (expression-id nil))


  (<> `("gnm:Cell" "Row" ,row "Col" ,column
		   ,@ (when value-type 
			(list "ValueType" 
			      (etypecase value-type 
				(integer value-type)
				(string value-type)
				(symbol (cell-value-type value-type)))))
		   ,@ (when value-format 
			(list "ValueFormat" value-format))
		   ,@ (when expression-id
			(list "ExprID" expression-id)))
  (<> (:text (princ-to-string value)))))

(defun cell-value-format (cell)
  (stp:attribute-value cell "ValueFormat"))

(defun cell-column (cell)
  (read-from-string (stp:attribute-value cell "Col")))

(defun cell-row (cell) 
  (read-from-string (stp:attribute-value cell "Row")))

(defun test-cells ()
  (<> ("gnm:Cells")
    (<cell> "YAY" :row 1 :column 1)
    
    (<> ("gnm:Cell" "Row" 1 "Col" 0
		    "ValueType" 60)
      (<> "number"))
    (<> ("gnm:Cell" "Row" 1 "Col" 2
		    "ValueType" 60)
      (<> "This is a test"))

    (<cell> "1.00" 
	    :row 2 :column 2
	    :value-type 40 
	    :value-format "$#,##0_);[Red]($#,##0)")
    (<cell> "2.00" :row 3 :column 2
	    :value-type 40)))

