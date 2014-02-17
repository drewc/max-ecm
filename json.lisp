(defpackage :max-ecm/json
  (:use :cl)
  (:import-from :st-json
		#:jso
		#:write-json-to-string)
  (:export #:jso
	   #:write-json-to-string
	   #:make-update-jso))
(in-package :max-ecm/json)

(defun make-update-jso (name id field value display &key (select-objects nil))
  (max-ecm/json:write-json-to-string
   (max-ecm/json:jso
    "object"
    (string-downcase name)
    "id"
    id
    "field"
    (string-downcase field)
    "value"
    (or value :null)
    "display"
    display
    "select_objects"
    (or (and select-objects (princ-to-string select-objects))
	:false))))

