(cl:defpackage :max-ecm/entity/user
  (:use :cl
	:max-ecm/database-interface
	:max-ecm/entity
	:max-ecm/json)
  (:import-from :max-ecm/database-interface
		  #:*database-connection-parameters*)
  (:export #:validate-user
	   #:call-with-user
	   #:find-user))

(cl:in-package :max-ecm/entity/user)

(defclass user (record)
  ((id :initarg :app-user-id)
   (username :initarg :username
	       :accessor user-username)
   (password :initarg :password
	     :accessor user-password)))

(defmethod object-jso ((object user) &key &allow-other-keys)
  (max-ecm/json:jso 
   "type" "user"
   "id" (record-id object) 
   "username" (user-username object)))

(defun find-user (id)
  (apply #'make-instance 'user	 
	 :allow-other-keys t
	 (postmodern:query (:select '* :from 'app-user
				    :where (:= 'app-user-id id))
			   :plist)))

(defun validate-user (username password)
  (let ((id (postmodern:query (:select 'app-user-id
		      :from 'app-user
		      :where (:and (:= 'username username)
				   (:= 'password password)))
		    :single)))
    (and id (find-user id))))

(defun call-with-user (function username password)
  (let ((user (with-database () 
		(validate-user username password))))
    (if (not user)
	(error "Username or Password not Valid")
	(let* ((*database-connection-parameters* 
		(list (first *database-connection-parameters*)
		      (format nil "mu_~a" (record-id user))
		      password 
		      (fourth *database-connection-parameters*)))
	       (postmodern:*database* nil))
	  (with-database (:writable t)
	    (funcall function user))))))

  

(setf (find-record "user")
      'find-user)






