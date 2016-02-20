(defpackage :max-ecm/gnumeric/style
  (:use :cl)
  (:import-from :cxml)
  (:import-from :stp)
  (:import-from :max-ecm/gnumeric/xml
		#:<>
		#:of-name
		#:adopt-children
		#:<gnm>)
  (:export ;:import-from :max-ecm/gnumeric/style
   #:style
   #:<style-border>
   #:style-region
   #:style-alignment
   #:<font>
   #:<accounting-style-region>
   #:<spreadsheet-title-styles>
   #:<spreadsheet-header-styles>
   ))

(in-package :max-ecm/gnumeric/style)

(defun <style-border> (&key top left right bottom)
  (<> |gnm:StyleBorder|
    (macrolet ((border (name)
		 `(when ,name 
		    (<> `(,,(concatenate 
			     'string "gnm:" 
			     (string-capitalize (string name)))
			    ,@,name)))))
      (border top)
      (border right)
      (border bottom)
      (border left))))

(defun style-alignment (type name)
  (concatenate
   'string "GNM_" 
   (ecase type 
     ((:vertical) "VALIGN")
     ((:horizontal) "HALIGN"))
   "_" (string name)))

(defun style-region (&key (start-column 0)
		       (end-column 0)
		       (start-row 0)
		       (end-row start-row))
  (lambda (&rest children)
    (<> ("gnm:StyleRegion" :|startCol| start-column 
			   :|endCol| end-column
			   :|startRow| start-row
			   :|endRow| end-row)
      (map nil #'funcall children))))

(defmethod pwap/yasexml/tag:call-with-tag (fn (tag (eql 'style-region)) &rest args)
  (funcall (apply #'style-region args) (lambda () (funcall fn tag))))


(defun style (&key 
		(horizontal-alignment "GNM_HALIGN_LEFT")
		(vertical-alignment  "GNM_VALIGN_TOP")
		(wrap-text 0)
		(shrink-to-fit 0)
		(rotation 0)
		(shade 0)
		(indent 0)
		(locked 1)
		(hidden 0)
		(foreground-color "0:0:0")
		(background-color "FFFF:FFFF:FFFF")
		(pattern-color "0:0:0")
		(format "General"))  
  (lambda (&rest children)
    (<> ("gnm:Style" :|HAlign| horizontal-alignment
		     :|VAlign| vertical-alignment
		     :|WrapText| wrap-text
		     :|ShrinkToFit| shrink-to-fit
		  :|Rotation| rotation
		  :|Shade| shade
		  :|Indent| indent
		  :|Locked| locked
		  :|Hidden| hidden
		  :|Fore| foreground-color
		  :|Back| background-color
		  :|PatternColor| pattern-color
		  :|Format| format)
      (map nil #'funcall children))))

(defmethod pwap/yasexml/tag:call-with-tag (fn (tag (eql 'style)) &rest args)
  (funcall (apply #'style args) (lambda () (funcall fn tag))))

(defun <font> (name &key (unit 11)
		      (bold 0)
		      (italic 0)
		      (underline 0))
  (<> ("gnm:Font"
       :|Unit| unit
       :|Bold| bold
       :|Italic| italic
       :|Underline| underline
       :|StrikeThrough| 0 
       :|Script| 0)
    (<> (:text name))))

(defun <accounting-style-region>
    (&key 
       (start-column 0)
       (end-column start-column)
       (start-row 0)
       (end-row start-row)
       (border '(:|Style| 1 :|Color| "C0C0:C0C0:C0C0"))
       (foreground-color "0:0:0")
       (format "_($* #,##0.00_);_($* (#,##0.00);_($* \"-\"??_);_(@_)"))
  (<> (style-region 
       :start-column start-column
       :end-column end-column
       :start-row start-row
       :end-row end-row)
    (<> (style 
	 :format format
	 :horizontal-alignment "GNM_HALIGN_GENERAL"
	 :vertical-alignment "GNM_VALIGN_BOTTOM"	 
	 :background-color "FFFF:FFFF:FFFF" 
	 :foreground-color foreground-color)
      (<font> "Sans" :unit 10)
      (<style-border> 
       :bottom border
       :right border
       ;; :left border
       ))))

(defun <spreadsheet-title-styles> (&key 
				   (start-column 0)
				   (end-column start-column)
				   (start-row 0)
				   (end-row start-row)
				   (unit 18)
				   (foreground-color "8888:8888:8888")
				   (bold 1))
  (<> ("Styles")
    
    (<> (style-region 
	 :start-column start-column
	 :end-column end-column
	 :start-row start-row
	 :end-row end-row)
      (<> (style 	   
	   :horizontal-alignment "GNM_HALIGN_GENERAL"
	   :vertical-alignment "GNM_VALIGN_BOTTOM"	 
	   :background-color "FFFF:FFFF:FFFF"
	   :foreground-color foreground-color)
      (<font> "Calibri" :unit unit
	      :bold bold)))))
  


		       

(defun <spreadsheet-header-styles> 
    (&key 
       (start-column 0)
       (end-column 1)
       (start-row 1)
       (end-row start-row)
       (border '(:|Style| 2 :|Color| "C0C0:C0C0:C0C0")))  
  (<> ("gnm:Styles")
    (loop for things in `(,(list :start-column start-column 
				 :end-column (1- end-column)
				 :right-border nil)
			   ,(list :start-column end-column
				 :end-column end-column
				 :right-border t))
	 do (<> (style-region 
	      :start-column (getf things :start-column)
	      :end-column (getf things :end-column)
	      :start-row start-row
	      :end-row end-row)
	   (<> (style 
		:horizontal-alignment (style-alignment 
				       :horizontal :left)
		:vertical-alignment "GNM_VALIGN_TOP"
		:wrap-text 1
		:shade 1
		:background-color "FFFF:FFFF:9999")
	     (<font> "Calibri" 
		     :bold 1
		     :unit 11)
	     (apply #'<style-border> 
		     :bottom border
		     :top border
		     :left border
		     (when (getf things :right-border)
		       `(:right ,border))))))))





