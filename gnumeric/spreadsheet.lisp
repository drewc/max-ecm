(defpackage :max-ecm/gnumeric/spreadsheet
  (:use :cl)
  (:import-from :cxml)
  (:import-from :stp)
  (:import-from :max-ecm/gnumeric/xml
		#:<>
		#:of-name
		#:adopt-children
		#:<gnm>)
  (:import-from #:max-ecm/gnumeric/document
		#:empty-document)
  (:import-from :max-ecm/gnumeric/cell 
		#:<cell>
		#:cell-value-type
		#:cell-value-format
		#:cell-row
		#:cell-column)
  (:import-from :max-ecm/gnumeric/style
   #:style
   #:<style-border>
   #:style-region
   #:style-alignment
   #:<font>
   #:<accounting-style-region>
   #:<spreadsheet-title-styles>
   #:<spreadsheet-header-styles>
   )
  (:import-from :alexandria #:with-output-to-file)
  (:export #:create-spreadsheet
	   #:create-spreadsheet-from-alist
	   #:make-spreadsheet-title))

(in-package :max-ecm/gnumeric/spreadsheet)

(defun make-spreadsheet (&key name)
  (let ((empty (stp:find-child-if 
		(of-name "Sheet")
		(stp:find-recursively-if 
		 (of-name "Sheets") 
		 (empty-document)))))
    (prog1 empty
      (when name (setf (spreadsheet-name empty) name)))))


(defstruct spreadsheet-title 
  title 
  (block NIL))

(defun spreadsheet-styles (sheet)
  (stp:find-child-if (of-name "Styles") sheet))

(defun spreadsheet-cells (sheet) 
  (stp:find-child-if (of-name "Cells") sheet))

(defun spreadsheet-columns (sheet)
  (stp:find-child-if (of-name "Cols") sheet))

(defun spreadsheet-rows (sheet)
  (stp:find-child-if (of-name "Rows") sheet))

(defun spreadsheet-merged-regions (sheet)
  (stp:find-child-if (of-name "MergedRegions") sheet))

(defun gnumeric-spreadsheets (&optional (document *empty-gnumeric-document*))
  (stp:find-recursively-if (of-name "Sheets")   
   document))

(defun spreadsheet-name (spreadsheet)
  "=> /name/:
    <gnm:Name>Sheet1</gnm:Name"
  (stp:data (stp:first-child 
	     (stp:find-child-if 
	      (of-name "Name") spreadsheet))))

(defun (setf spreadsheet-name) (value spreadsheet)
  (setf (stp:data (stp:first-child 
	     (stp:find-child-if 
	      (of-name "Name") spreadsheet)))
	value))
 


(defun |<NAME info>| (name number 
		      &key (unit (error "must provide unit"))
			(hard-size 1))
  (<> `(,name "No" ,number 
	      "Unit" ,unit
	      "HardSize" ,hard-size)))
  
(defun <row-info> (number &key (unit (error "must provide unit"))
			    (hard-size 1))
  (|<NAME info>| "gnm:RowInfo" number
		 :unit unit :hard-size hard-size))
  
(defun <column-info> (number 
		      &key (unit (error "must provide unit"))
			(hard-size 1))
  (|<NAME info>| "gnm:ColInfo" number
		 :unit unit :hard-size hard-size))
    
(defun rows/cols-for-headings (report-line
			       &key (start-column 0)
				 (start-row 0))
  ;; ok, so we need to declare the hight of the row itself.
  ;; That also means the width of columns. 
  
  ;; 1) find the length of words
  (let* ((word-lengths
	  (loop :for (name . value) 
	     :in report-line 
	     :collect (mapcar #'length 
			      (split-sequence:SPLIT-SEQUENCE #\Space name))))
	 ;; 1.5) What is the longest word?
	 (max-wl 
	  (loop for ls in word-lengths
	     :collect (if ls (apply #'max ls) 1)))
	 
	 ;; 2) The longest word x 11pts is the width of the column
	 ;; This is quite wrong for some things we should have a 'max
	 ;; lines' parameter and have the width be the longest word OR
	 ;; the longest LENGTH that goes at max lines.
	 (header-cols 
	  (<gnm> 
	    (<> ("gnm:Cols" "DefaultSizePts" 48)
	      (loop for ls in word-lengths
		 :for col :upfrom start-column 
		 :do (<column-info> 
		      col 
		      :unit (format 
			     nil "~$" (* 11 (if ls (apply #'max ls) 1)))
		      :hard-size 1)))))
         ;; 3) The number of lines
	 (number-of-lines
	  (loop for wls in word-lengths
	     :collect (let ((lines 0) (length 0) 
			    (max (if wls (apply #'max wls) 1)))
			(loop for n in wls 
			   do (let ((total (+ 1 length n))) 
				(if (>= total max )
				    (progn (dotimes (i (if (>= (/ total 2) max) 2 1))
					     (incf lines))
					   (setf length 0))
				    (setf length total)))) 			  
			(unless (zerop length)
			  (incf lines))
			lines)))
	 ;; 4) the hight of this row is the number of lines
	 (header-rows 
	  (<gnm> 
	    (<> ("gnm:Rows" "DefaultSizePts" "12.75")
	      (<row-info> 
	       start-row :unit (format 
				nil "~$" 
				(* (if number-of-lines 
				       (apply #'max number-of-lines)
				       1)
				   12.75)))))))
	      
	      
    (values header-rows header-cols)))

(defun create-spreadsheet-from-alist (name alist
				      &key 
                                        (format-dollarsign t)
					(create-header? t)
					(start-row 0)
					(start-column 0))
  (flet ((adopt-children (adopter adoptee)
	   (stp:do-children (c adoptee)      
	     (stp:append-child adopter (stp:copy c)))))
 	
    (let* ((report alist)
	   (report-line (first report))
	   (sheet (make-spreadsheet :name name))
	   (styles (stp:find-child-if (of-name "Styles") sheet))
	   (rows (stp:find-child-if (of-name "Rows") sheet))
	   (columns (stp:find-child-if (of-name "Cols") sheet))
	   (old-cells (stp:find-child-if (of-name "Cells") sheet))
	   (header-row start-row)
	   (cells 
	    (<gnm>   
	      (<> ("gnm:Cells")
		(when create-header? 
		  (loop :for (name . value) 
		   :in (first report) 
		   :for i upfrom 0
		   :do (<cell> 
			name 
			:row header-row
			:column i
			:value-type (cell-value-type 'string))))))))
      (multiple-value-bind (header-rows header-columns) 
	  (rows/cols-for-headings report-line 
				  :start-column start-column
				  :start-row header-row)
	(stp:replace-child sheet old-cells cells)
	(stp:delete-children styles)
	(when create-header? 
	  (adopt-children 
	   styles (<gnm> (<spreadsheet-header-styles>
			  :start-row header-row
			  :start-column start-column
			  :end-column (+ start-column (1- (length report-line)))))))
	
	(adopt-children 
	 cells 
	 (<gnm>   
	   (<> ("gnm:Cells")
	     (loop :for line in report 
		:for row upfrom (+ 1 header-row)
		:do (loop :for (name . original-value)  in line
		       :for column :upfrom start-column 
		       :do 
		       (let* ((value 
			       (cond 
				 ((typep original-value 'simple-date:timestamp)
				  (format nil "~{~A-~A-~A~}" 
					  (multiple-value-list
					   (simple-date:decode-date original-value))))
				 (t (princ-to-string original-value))))
			      (value-type 
			       (cond 
				 ((or (equal "" value)
				      (typep original-value 's-sql:db-null))
				  (cell-value-type 'empty))
				 ;; check if it is currency
				 ((or 
                                   (char= #\$ (aref value 0))
                                   (and (> (length value) 2)
                                        (string= "-$" value :end2 2)))
				  (setf value 
                                        (remove #\$ (remove #\, value)))
				  ;; set the accounting style region
                                  (stp:append-child 
                                   styles 
                                   (<gnm> 
                                     (apply #'<accounting-style-region>
                                            :start-column column
                                            :start-row row
                                            (unless format-dollarsign
                                              (list :format "0.00")))))
				  (cell-value-type 'float))
				 (t (cell-value-type 'string))))
			      (value-format 
			       (cond ((equal value-type (cell-value-type 'float))
                                      (if format-dollarsign
                                          "$#,##0_);[Red]($#,##0)"
                                          "###0_);[Red](###0)"))))
			      (column-info 
			       (stp:find-child-if 
				  (lambda (e) 
				    (string= (princ-to-string column) 
					     (stp:attribute-value e "No")))
				  header-columns))
			      (column-width 
			       (read-from-string 
				(stp:attribute-value column-info "Unit"))))
			
			 ;; now, check the column. If the width is
			 ;; to small for this, then make it bigger.
			 (setf (stp:attribute-value column-info "Unit") 
			       (format 
				nil "~$" 
				(max column-width
				     (let ((den 
					    (cond ((equal (cell-value-type 'float)
							  value-type)
						   1)
						  (t 2.5))))
				       (/ (* 10 (length (princ-to-string value))) den)))))
			 
			 
			 (when column-info
			 )	  
			 (<cell>  
			  value
			  :row row
			  :column column
			  :value-type value-type 
			  :value-format value-format)))))))
	(adopt-children columns header-columns)
	(adopt-children rows header-rows)

	(stp:copy sheet)))))

(defun column-identifier (number)
  (let ((*print-base* 36))
      (if (<= 26 number)
	  (princ-to-string (+ 334 10 number))
	  (princ-to-string (+ 10 number)))))

(defun row-identifier (number)
  (princ-to-string (1+ number)))

(defun cell-identifier (&key row column)
  (concatenate 'string (column-identifier column)
	       (row-identifier row)))
  

	   
    
(defun <merge> (&key from to)
  (<> ("gnm:Merge")
    (<> (:text (concatenate 
		'string 
		from ":" to)))))

(defun create-spreadsheet-from-title (title 
				      &key 
					(start-row 0)
					(start-column 0))
  (let* ((sheet (make-spreadsheet :name (cdr (spreadsheet-title-title title))))
	 (ss-styles (stp:find-child-if (of-name "Styles") sheet))
	 (ss-rows (stp:find-child-if (of-name "Rows") sheet))	 
	 (ss-cells (stp:find-child-if (of-name "Cells") sheet))
	 (block-sheet 
	  (create-spreadsheet-from-block (spreadsheet-title-block title)
					 :start-row (+ 2 start-row) 
					 :start-column start-column))
	 (cells
	  (<gnm>   
	    (<> ("gnm:Cells")
	      (destructuring-bind (name . value) 
		  (spreadsheet-title-title title)
		(<cell> 
		 name 
		 :row start-row
		 :column start-column ;; we will merge this with this+1
		 :value-type (cell-value-type 'string))
		(<cell> 
		 value
		 :row start-row
		 :column (+ 2 start-column)
		 :value-type (cell-value-type 'string)))

	      )))
	    
	 (ss-merged-regions 
	  (stp:find-child-if (of-name "MergedRegions") sheet))
	 (merged-regions 
	  (<gnm> 
	    (<> ("gnm:MergedRegions")
	      (<merge> :from (cell-identifier 
			      :column start-column 
			      :row start-row)
		       :to (cell-identifier 
			    :column (1+ start-column)
			    :row  start-row)))))
	 (rows 
	  (<gnm> (<> ("gnm:Rows")
		   (<row-info> start-row 
			       :unit 26)))))
    (stp:delete-children (spreadsheet-styles sheet))
    (adopt-children ss-styles 
		    (stp:find-child-if (of-name "Styles") block-sheet))
    (adopt-children ss-styles 
		    (<gnm> 
		      (<spreadsheet-title-styles> :start-row 0 
						:start-column 0
						:unit 18)))
    (adopt-children ss-styles 
		    (<gnm> 
		      (<spreadsheet-title-styles> :start-row 0 
						:start-column 2
						:unit 18
						:foreground-color "0:0:0")))


    (adopt-children cells 
		    (stp:find-child-if (of-name "Cells") block-sheet))
    
    (if ss-merged-regions 
	(adopt-children ss-merged-regions merged-regions)
	(stp:append-child sheet merged-regions))
    
    (stp:replace-child sheet ss-cells cells)
    (adopt-children ss-rows rows)
    (stp:copy sheet)))
 
(defun create-cells-from-block (block 
				   name-coords-push
				 &key 
				 (start-row 0)
				 (start-column 0))
  (let ((row start-row))
    (labels ((block-cells (block   
			      start-column)	       	       
	       (loop :for (name . value) :in block
		  :do 
		  (progn 
		    (funcall name-coords-push
			     `(:start-row ,row
					  :start-column ,start-column
					  :bold 0))
		    (<cell> 
		     name
		     :row row
		     :column start-column)		
		    (if (consp value)
			(block-cells 
			 value  (1+ start-column))
			(progn 
			  (funcall name-coords-push
			     `(:start-row ,row
					  :start-column ,(1+ start-column)
					  :foreground-color "0:0:0"
					  :bold 0))
			  (<cell> 
			 value
			 :row row
			 :column (1+ start-column))))
		    (incf row)))))
      (block-cells block start-column))))
	   
  
(defun create-spreadsheet-from-block (block 
					 &key 
				       (start-row 0)
				       (start-column 0)
				       &aux (sheet (make-spreadsheet :name "block")))
  (let (name-coords)
    (adopt-children (stp:find-child-if (of-name "Cells") sheet)
		    (<gnm> (<> ("Cells")
		     (create-cells-from-block 
		      block (lambda (c) (push c name-coords))
		      :start-row start-row 
		      :Start-column start-column))))
    (stp:delete-children (spreadsheet-styles sheet))
        (loop for c in name-coords :do 
	 (adopt-children (stp:find-child-if (of-name "Styles") sheet)
			 (<gnm> (apply
				 '<spreadsheet-title-styles>  :unit 11 c)))))
  sheet)
    
		    
(defun create-spreadsheet 
    (report 
     &key 
       title name start-row 
       (format-dollarsign t)
       (document (empty-document))
       (create-header t)
       (document-sheet-name nil))
       
  (let* ((title (when title (create-spreadsheet-from-title title)))
	 (title-cells (when title (spreadsheet-cells title)))
	 (title-max-row 
	  (if (not title) 
	      -2
	      (apply #'max (mapcar #'cell-row 
				   (remove-if-not (lambda (e) (typep e 'stp:element))
						  (stp:list-children title-cells))))))
         (start-row (or start-row (+ 2 title-max-row)))
         (sheet (create-spreadsheet-from-alist 
                 name report  
                 :start-row start-row
                 :format-dollarsign format-dollarsign
                 :create-header? create-header))
	 (sheets (stp:find-recursively-if 
		  (of-name "Sheets") document))
	 (sheet-name-index 
	  (stp:find-recursively-if (of-name "SheetNameIndex") document))
         (delete-document-sheets (not document-sheet-name)))
    
    
    (when delete-document-sheets 
      (stp:delete-children sheets)
      (stp:delete-children sheet-name-index)
      (stp:append-child sheet-name-index  
                        (<gnm> (<> ("gnm:SheetName" "Cols" "256" "Rows" "65536")
                                 (<> (:text (spreadsheet-name sheet)))))))

    (when title 
      (adopt-children (spreadsheet-rows sheet)
		      (spreadsheet-rows title))
      (adopt-children (spreadsheet-columns sheet)
		      (spreadsheet-columns title))

      (if (spreadsheet-merged-regions sheet)
	  (adopt-children (spreadsheet-merged-regions sheet)
			  (spreadsheet-merged-regions title))
	  (stp:append-child sheet (stp:copy (spreadsheet-merged-regions title))))
    
		    
      (adopt-children (spreadsheet-cells sheet) 
		      title-cells)
    
      (adopt-children (spreadsheet-styles sheet)
                      (spreadsheet-styles title)))


 (let* ((cell-rows (mapcar #'cell-row 
			      (remove-if-not (lambda (e) (typep e 'stp:element))
					     (stp:list-children (spreadsheet-cells sheet)))))
	   #+(or)(cell-cols (mapcar #'cell-column 
			      (remove-if-not (lambda (e) (typep e 'stp:element))
					     (stp:list-children (spreadsheet-cells sheet)))))
	   (cell-min-row (+ 3 title-max-row))
	   (cell-max-row (apply #'max cell-rows))
	   (max-row-cells (remove-if-not (lambda (n) (eql cell-max-row n))
					 (remove-if-not (lambda (e) (typep e 'stp:element))
							(stp:list-children (spreadsheet-cells sheet)))
					 :key #'cell-row)))
      ;; For the Totals: row
      (dolist (c max-row-cells)
	(when (eql (cell-value-type c) 'float)
	  (stp:append-child (spreadsheet-cells sheet)
			    (<gnm> (<cell> 
				    (format nil "=sum(~A~A:~A~A)"
					    (column-identifier 
					     (cell-column c))
					    (row-identifier
					     cell-min-row)
					    (column-identifier 
					     (cell-column c))
					    (row-identifier
					     (cell-row c)))
				    :value-type NIL
				    :value-format (cell-value-format c)
				    :row (1+ (cell-row c))
				    :column (cell-column c))))
          (stp:append-child 
             (spreadsheet-styles sheet)
             (<gnm> 
               (apply #'<accounting-style-region> 
                      :start-column (cell-column c)
                      :start-row (1+ (cell-row c)) 
                      :foreground-color "4444:4444:4444"
                      (unless format-dollarsign
                        (list :format "0.00"))))))))

    (if (not document-sheet-name)
        (stp:append-child sheets sheet)
        (let ((the-sheet 
               (first (stp:filter-children 
                        (lambda (sheet)
                          (and (funcall (max-ecm/gnumeric/xml:of-name "Sheet") sheet)
                               (let ((name (stp:find-child-if (max-ecm/gnumeric/xml:of-name "Name")
                                                              sheet)))
                                 (string-equal document-sheet-name (stp:string-value name)))))
                        sheets))))
          (max-ecm/gnumeric/xml:adopt-children the-sheet sheet)))
    document
   ))

#+(or)(defun create-white-oak-spreadsheet (report &key title name)
  (let* ((sheet (create-spreadsheet-from-alist 
		 name report  
		 :start-row 19
		 :create-header? nil))
	 (document (white-oak-document))
	 (sheets (stp:find-recursively-if 
		  (of-name "Sheets") document))
	 (sheet-name-index 
	  (stp:find-recursively-if (of-name "SheetNameIndex") document))
	 (first-sheet (stp:find-recursively-if 
		       (of-name "Sheet") sheets)))
    (adopt-children first-sheet sheet)
    
    document))
		  
