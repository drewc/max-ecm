(cl:defpackage :max-ecm/entity/claim-transaction
  (:use :cl	
	:max-ecm/entity
	:max-ecm/database-interface
	:max-ecm/json
	:max-ecm/output-formatting))
(cl:in-package :max-ecm/entity/claim-transaction)

(defclass claim-transaction (record)
  ((id :initarg :transaction-id)
   (claim-id :initarg :claim-id 
	     :accessor claim-transaction-claim-id)
   (date :initarg :date
	 :initarg :transaction-date
	 :accessor claim-transaction-date)
   (cheque-number :initarg :cheque-number
	 :accessor claim-transaction-cheque-number)
   (payee-id :initarg :payee-id
	     :accessor claim-transaction-payee-id)))

(defmethod object-jso ((object claim-transaction) &key &allow-other-keys)
  (max-ecm/json:jso 
   "type" "claim-transaction"
   "id" (record-id object)
   "claim_id" (claim-transaction-claim-id object)
   "date" (timestamp->iso8601 (claim-transaction-date object))
   "cheque_number" (claim-transaction-cheque-number object)
   "payee" (let ((id (claim-transaction-payee-id object)))
	     (if (and id (not (eql id :null)))
		 (object-jso (find-record "person" id))
		 id))))

(defun find-claim-transaction (id)
  (apply #'make-instance 'claim-transaction 	 
	 :allow-other-keys t
	 (postmodern:query (:select '* :from 'claim-transaction 
				    :where (:= 'transaction-id id))
			   :plist)))
  
(setf (find-record "claim-transaction")
      'find-claim-transaction)
		
(defmethod update-record/jso  ((record claim-transaction) jso)
  (let ((cheque-number (getjso "cheque_number" jso)))
    (multiple-value-bind (payee payee-exists?)
	(getjso "payee" jso)
    (if (and (not cheque-number)
	     (not payee-exists?))
	(error "Nothing to update afor ~A" jso)
	(postmodern:execute 
	 (s-sql:sql-compile 
	  `(:update claim-transaction 
	    :set 
	    ,@(when cheque-number (list 'cheque-number cheque-number))
	    ,@(when payee-exists? (list 'payee-id (etypecase payee
						   (integer payee)
						   (string (read-from-string payee))
						   (symbol payee))))
	    :where (:= transaction-id
		       ,(record-id record))))))
    (find-record "claim-transaction" (record-id record)))))




