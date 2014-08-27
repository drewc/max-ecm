(cl:defpackage :max-ecm/entity
  (:use :cl)
  (:import-from  :max-ecm/json
		 #:jso #:getjso)
  (:export #:object-jso
	   #:search/jso

	   #:record 
	   #:find-record
	   #:id
	   #:record-id
	   #:update-record/jso))

(cl:in-package :max-ecm/entity)

(defgeneric object-jso (entity &key &allow-other-keys))



(defvar *type-searchers* (make-hash-table :test 'equalp))

(defun (setf search/jso) (function type)
  (setf (gethash type *type-searchers*) function))

(defun search/jso (jso)
  (let ((searcher (gethash (getjso "type" jso) *type-searchers*)))
    (if searcher
	(funcall searcher (getjso "q" jso))
	(error "No search for type: ~A" (getjso "type" jso)))))

(defclass record ()
  ((id :accessor record-id
       :initarg :id)))

(defvar *record-finders* (make-hash-table :test 'equalp))

(defun (setf find-record) (function type)
  (setf (gethash type *record-finders*) function))

(defgeneric find-record (type id)
  (:method (type id)
    (let ((finder (gethash type *record-finders*)))
      (if finder 
	  (funcall finder id)
	  (error "No Finder for type: ~A" type)))))

(defgeneric update-record/jso (record jso))






