(cl:defpackage :max-ecm/entity/person
  (:use :cl
	:max-ecm/database-interface
	:max-ecm/entity
	:max-ecm/json))

(cl:in-package :max-ecm/entity/person)

(defclass person (record)
  ((id :initarg :person-id)
   (first-name :initarg :first-name
	       :accessor person-first-name)
   (last-name :initarg :last-name
	       :accessor person-last-name)
   (company :initarg :company-name
	    :accessor person-company-name)))

(defmethod object-jso ((object person) &key &allow-other-keys)
  (apply #'max-ecm/json:jso 
	 "type" "person"
	 "id" (record-id object)
	 `( ,@(let ((f (person-first-name object)))
		   (when f (list "first_name" f)))
	      ,@(let ((l (person-last-name object)))
		     (when l (list "last_name" l)))
	      ,@(let ((l (person-company-name object)))
		     (when l (list "company_name" l))))))

(defun find-person (id)
  (apply #'make-instance 'person	 
	 :allow-other-keys t
	 (postmodern:query (:select '* :from 'person
				    :where (:= 'person-id id))
			   :plist)))

(setf (find-record "person")
      'find-person)

(defun search-person (query &key (offset 0) (limit 25))
  (assert (stringp query) (query) "SEARCH person does not take a JSO yet")
  (let ((names (split-sequence:split-sequence #\Space query)))
    (flet ((q (select &key
		(limit limit)
		(offset offset)
		(order-by select))
	     `(:limit 
	       (,(if order-by :order-by
		     :union-all)
		(:union-all
		 (:select ,select
		   :from person
		   :where (:and 
			   ,@(loop :for search-string :in names 
				:collect `(:ilike (person-name (:dot person *)) ,search-string))))
		 (:select ,select
		   :from person
		   :where (:and 
			   ,@(loop :for search-string :in names 
				:collect `(:or 
					   (:ilike first-name ,search-string)
					   (:ilike last-name ,search-string)
					   (:ilike company-name ,search-string)))))
		 (:select ,select
		   :from person
		   :where (:and 
			   ,@(loop :for search-string :in names 
				:collect `(:or 
					   (:% first-name ,search-string)
					   (:% last-name ,search-string)
					   (:% company-name ,search-string)
					   (:% (person-name (:dot person *)) ,search-string))))))
		  ,@(when order-by (list order-by)))
	       ,limit ,offset)))
	     (multiple-value-bind (result number)
		 (postmodern:query 
		  (s-sql:sql-compile 
		   (q 'person-id))
		  :column)
	       (when (= limit number)
		 (setf number (third (postmodern:query 
				       (s-sql:sql-compile 
					(q '(:count *) :order-by nil))
				       :column))))
	       (values (mapcar #'(lambda (id) 
				   (find-record  "person" id))
			       (remove-duplicates result)) number)))))

(setf (search/jso "person")
      'search-person)



 #+(or)     (filter-people  
       (remove-duplicates 
	(append 
	 (let* ((ex (select-objects 
		     'person 
		     :where `(:or
			  ,@ (when (second names)
			       (list
				`(:and
				  (:ilike first-name ,(first names))
				  (:ilike last-name ,(second names)))))
			     
			  (:ilike first-name ,search-string)
			  (:ilike last-name ,search-string)
			  (:ilike company-name ,search-string)
			  (:ilike (person-name (:dot person *)) ,search-string))))
	      (ex1  (select-objects 
		      'person 
		      :where 
		      `(:or
			,@(when (second names)
			     (list
			      `(:and
				(:ilike 
				 first-name 
				 ,(first names))
				(:ilike 
				 last-name 
				 ,(second names)))))		       
			(:ilike 
			 first-name 
			 ,(format nil "~A%" search-string))
			(:ilike 
			 last-name 
			 ,(format nil "~A%" search-string))
			(:ilike 
			 company-name 
			 ,(format nil "~A%" search-string))))))
	  
	  (let ((d (remove-duplicates (append ex ex1)
			     :key #'person.person-id
			     :from-end t)))
	    (or d
		(select-objects 
		 'person 
		 :where `(:ilike 
			 company-name 
			 ,(format nil "%~A%" search-string))))))
		 
	(unless exact 	
	  (select-objects 
	   'person 
	   :where `(:or
		    ,@ (when (second names)
			 (list
		       `(:and
			 (:or
			  (:ilike 
			   first-name 
			   ,(format nil "%~A%" (first names)))
			  (:% first-name ,(first names)))
			 (:or
			  (:ilike 
			   last-name 
			   ,(format nil "%~A%" (second names)))
			  (:% last-name ,(second names))))))
		 (:ilike 
		  first-name 
		  ,(format nil "%~A%" search-string))
		 (:ilike 
		  last-name 
		  ,(format nil "%~A%" search-string))
		 (:ilike 
		  company-name 
		  ,(format nil "%~A%" search-string))
		 (:% first-name ,search-string)
		 (:% last-name ,search-string)
		 (:% company-name ,search-string)
		 (:% (person-name (:dot person *)) ,search-string)))))
	:from-end t
	:key #'person.person-id))






