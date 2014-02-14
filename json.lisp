(defpackage :max-ecm/json
  (:use :cl)
  (:import-from :st-json
		#:jso
		#:write-json-to-string)
  (:export #:jso
	   #:write-json-to-string))
(in-package :max-ecm/json)

