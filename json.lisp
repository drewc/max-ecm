(defpackage :max-ecm/json
  (:use :cl)
  (:use :st-json)
  (:export #:jso
	   #:getjso
	   #:mapjso
	   #:write-json-to-string
	   #:read-json-from-string
	   #:as-json-bool
	   #:from-json-bool))
(in-package :max-ecm/json)


