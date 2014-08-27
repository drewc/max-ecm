(cl:defpackage :max-ecm/request-utils
  (:documentation
   "Utilities for handling HTTP requests")
  (:use :cl
	:max-ecm/binding-common)
  (:import-from :max-ecm/url-scheme-variables
		"*SERVER-PUBLIC-URL*")
  (:import-from :max-ecm/json
		"READ-JSON-FROM-STRING")
  (:export
   "ADD-JSON-ENVELOPE"
   "ADD-PAGED-RESULT-ENVELOPE"
   "ENCODE-QUERY-PARAMETERS"
   "PARAMETER-BIND"
   #:with-paged-result-context
   #:get-post-body-as-jso))
(cl:in-package :max-ecm/request-utils)


(defun add-json-envelope (response)
  (st-json:jso "response" response))

(defvar *current-page-size*)
(defvar *current-page-offset*)

(defun add-paged-result-envelope (result-page base-url
				  base-query-parameters
				  total-num-results)
  (flet ((page-parameters (function)
	   `(("n" . ,(format nil "~D" *current-page-size*))
	     ("o" . ,(format nil "~D" (funcall function
					       *current-page-offset*
					       *current-page-size*)))))
	 (page-url (page-parameters)
	   (format nil "~A~A~A"
		   *server-public-url*
		   base-url
		   (encode-query-parameters
		    `(,@page-parameters
		      ,@base-query-parameters)))))
    (st-json:jso
     "result_page" result-page
      "query_info" (st-json:jso
		    ;; 		   "base_query" (page-url nil)
		    ;; 		   "next_page" (if (< (+ *current-page-size* *current-page-offset*)
		    ;; 				      total-num-results)
     ;; 				   (page-url (page-parameters #'+))
     ;; 				   :null)
     ;; 		   ;; FIXME: This doesn't produce a prev-page if the
     ;; 		   ;; start offset is less than the page size.  Ideally,
     ;; 		   ;; we should accept negative offsets up to the page
     ;; 		   ;; size, but that requires a bit more cleverness in
     ;; 		   ;; the whole paged-search interface.
     ;; 		   "prev_page" (if (>= *current-page-offset* *current-page-size*)
     ;; 				   (page-url (page-parameters #'-))
     ;; 				   :null)
     ;; 		   "page_size" *current-page-size*
     ;; 		   "page_start_offset" *current-page-offset*
		   "total_num_results" total-num-results))))

(defun encode-query-parameters (parameter-alist)
  (with-output-to-string (url)
    (loop
       for (name . value) in parameter-alist
       for prefix = #\? then #\&
       do (format url "~A~A=~A" prefix name
		  (hunchentoot:url-encode value)))))

(defun get-parameter-value (name parameter-list default)
  (or (cdr (assoc name parameter-list :test #'string=))
      default))

(defmacro parameter-bind (parameter-names parameter-list &body body)
  (let ((internal-parameter-list (gensym "PARAMETER-LIST-")))
    `(let ((,internal-parameter-list ,parameter-list))
       (let (,@(parameter-binding-forms internal-parameter-list parameter-names 'get-parameter-value nil))
	 (progn ,@body)))))

(defmacro with-paged-result-context ((get-parameters page-size offset &key (default-page-size 20)) &body body)
  (let ((raw-page-size (gensym))
	(raw-offset (gensym)))
    `(parameter-bind
	 ((,raw-page-size "n")
	  (,raw-offset "o"))
	 ,get-parameters
       (let* ((,page-size (if ,raw-page-size
			      (parse-integer ,raw-page-size)
			      ,default-page-size))
	      (,offset (if ,raw-offset
			   (parse-integer ,raw-offset)
			   0))
	      (*current-page-size* ,page-size)
	      (*current-page-offset* ,offset))
	 ,@body))))

(defun get-post-body-as-jso ()
  (let* ((raw-json (hunchentoot:raw-post-data))
	 (stringized-json
	  (if (stringp raw-json)
	      raw-json
	      (sb-ext:octets-to-string raw-json :external-format :utf-8))))
    (let ((*read-default-float-format* 'double-float))
      (read-json-from-string stringized-json))))

;;; EOF
