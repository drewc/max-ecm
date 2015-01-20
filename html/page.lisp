(cl:defpackage :max-ecm/html/page
  (:use :cl :max-ecm/entity)
  (:import-from :yasexml #:<>)
  (:export #:page))
(cl:in-package :max-ecm/html/page)

(defmethod yasexml:call-with-tag (function
				  (tag (eql 'page))
				  &rest attributes)
  (apply #'page (lambda () (funcall function tag)) attributes))

(defun page (thunk &key )
  (<> html
    (<> head
      (<> (:unescaped '|
<!-- Latest compiled and minified CSS -->
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css">

<!-- Optional theme -->
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css">

<!-- Latest compiled and minified JQuery -->
<script src="http://code.jquery.com/jquery-2.1.1.min.js"></script>

<!-- Latest compiled and minified JavaScript -->
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"></script>|)))    
    (<> body (funcall thunk))))
      


  
  
  
