(cl:defpackage :max-ecm/binding-common
  (:documentation "Common machinery to implement custom binding forms")
  (:use :cl)
  (:export
   "PARAMETER-BINDING-FORMS"))
(cl:in-package :max-ecm/binding-common)


(defun default-parameter-name (variable-name)
  (substitute #\_ #\- (string-downcase variable-name)))

(defun parse-parameter-description (parameter-description default-default-value)
  (destructuring-bind
	(variable-name &optional
		       (parameter-name (default-parameter-name variable-name))
		       (default-value default-default-value))
      (alexandria:ensure-list parameter-description)
    (values variable-name parameter-name default-value)))

(defun make-parameter-binding-form (parameter-list parameter parameter-value-fn default-default-value)
  (multiple-value-bind (variable-name parameter-as-string default-value)
      (parse-parameter-description parameter default-default-value)
    `(,variable-name (,parameter-value-fn
		      ,parameter-as-string
		      ,parameter-list
		      ,default-value))))

(defun parameter-binding-forms (internal-parameter-list parameter-names parameter-value-fn default-default-value)
  (loop for parameter in parameter-names
     collect (make-parameter-binding-form internal-parameter-list
					  parameter
					  parameter-value-fn
					  default-default-value)))

;;; EOF
