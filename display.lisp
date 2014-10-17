(uiop:define-package :max-ecm/display
    (:use  :cl 
	   :max-ecm/json
	   :max-ecm/descriptions)
  (:import-from :sheeple
		#:descendantp)
  (:import-from :yasexml #:<>)
  (:export #:display-object
	   #:display-object-using-type
	   #:display-attribute-using-type
	   #:display-description
	   #:description-display-attributes))

(cl:in-package :max-ecm/display)

(defun description-display-attributes (description)
  (nreverse (loop :for attribute :in (description-attributes description)
	       :when (and (descendantp attribute =>view)
			  (attribute-view attribute))
	       :collect attribute)))

(defvar *display-descriptions*
  (make-hash-table :test #'equalp))

(defun (setf display-description) (value object &optional layer)
  (let ((existing (gethash object *display-descriptions*)))
    (setf (gethash object *display-descriptions*)
	  (if (consp existing) 
	      (let ((now? (assoc layer existing)))
		(if now?
		    (prog1 existing 
		      (setf (cdr now?)
			    value))
		    (list* (cons layer value) existing)))
	      (list (cons layer value)
		    (cons nil existing))))))

(defgeneric display-description (object display-type)
  (:method (object display-type)
    {description})
  (:method ((object jso) display-type)
    (let* ((type (getjso "type" object))
	   (d (if type (gethash type *display-descriptions*) {record})))
      (if (listp d)
	  (or (cdr (assoc display-type d))
	      (cdr (assoc nil d))
	      {record})
	  d))))

(defun display-object (object 
		       &key (type :view)
			 (description (display-description 
				       object type)))
 #+#:|This is a comment| (break "~A" description)
 (display-object-using-type object type :description description))



(defgeneric display-object-using-type (object display-type &key description
							     &allow-other-keys)
  (:method (object type &key &allow-other-keys)
    (display-object-using-type object :view))
  (:method ((string string) type &key &allow-other-keys)
    (<> (:text string)))
    (:method ((number number) type &key &allow-other-keys)
      (<> (:text number)))
  (:method (object (display-type (eql :text))
	    &key (description 
		  (display-description 
		   object display-type))
	      (stream nil))
    (let ((output-stream (or stream (make-string-output-stream))))
      (map nil (lambda (a) 
		 (let ((label (or (and (not (eql t (attribute-label a)))
				       (attribute-label a))
				  (and (eql t (attribute-label a))
				       (string-capitalize
					(string-downcase
					 (symbol-name (attribute-name a)))))))
		       (value (funcall (attribute-formatter a)
				       (object-attribute-value object a))))
		 (format output-stream "~A~A~%" 
			 (if label (concatenate 'string label ": ") "")
			 value)))			 
	   (description-display-attributes description))
      (or stream (get-output-stream-string output-stream)))))

(defmethod display-object-using-type 
    ((list list) (display-type (eql :view))
     &key &allow-other-keys)
  (<> (ul)
    (dolist (item list)
      (<> li (display-object-using-type item :view)))))

(defmethod display-object-using-type 
    ((object jso) type
     &key description &allow-other-keys)
  (let ((attributes (description-display-attributes description)))
    ;;    (break "Displaying ~A~%~%USING ~A" object attributes)
    (<> ul
      (dolist (a attributes)
	(<> li
	  (display-attribute-using-type object a type))))))

;(<> (:text (mapcar #'class-of attributes) description))))

(defgeneric display-attribute-using-type (object attribute type &key label)
  (:method (object attribute type &rest args 
	    &key (label (attribute-label attribute)) &allow-other-keys)
    (let ((value (object-attribute-value object attribute))
	  (around (ignore-errors (sheeple:property-value attribute :around))))
      (flet ((doit ()
	       (<> span 
		 (when label 
		   (<> (label :class "control-label")
		     (<> (:text label) (<> (:unescaped "&nbsp;")))))
		 (display-object value :type type))))
	(if around 
	    (apply around #'doit 
		   :value value :object object :attribute attribute :type type args)
	    (doit))))))
  

(defmethod display-object-using-type 
    (object (display-type (eql :view))
     &key &allow-other-keys)
  (<> (:text (display-object object :type :text))))

    
    
    
    
  
