(cl:defpackage :max-ecm/mail
  (:use :cl)
  (:import-from #:cl-smtp)
  (:export #:send-mail
	   #:send-forgot-password-mail))
(cl:in-package :max-ecm/mail)

(defvar *mail-authentication*)

(defun send-mail (from to subject message 
		  &key cc bcc)
  (cl-smtp:send-email 
   "smtp.gmail.com" from to subject message
   :cc cc
   :bcc bcc
   :authentication *mail-authentication* 
   :ssl :tls))

(defvar *test* 1)
(defun send-forgot-password-mail (username reason)
  (let ((subject (concatenate 
		  'string "TEST: '" username "' "
		  "has forgotten the password for ECM"))
	(body (concatenate 
		  'string "TEST: '" username "' "
		  "has forgotten the password for ECM." (string #\Newline)
		  (string #\Newline)
		  (when reason 
		    (format nil "Reason: ~%~A~%~%" reason))
		  "You should email or call them to inform them of their password.

Please reply to this if you receive it so others know it is being taken care of. There are CC's[1], so please hit \"reply to all\" or whatever your email client has so that you reply to everone that is seeing this email. If that is not available, email me@drewc.ca so it can be investigated.
	    
Signed, 
Maxwell Claims ECM"))
	(to "trish@maxwellclaims.net")
	(from "maxclaims@drewc.ca")
	(cc '("max01@maxwellclaims.net" "me@drewc.ca")))
    (send-mail from to subject body :cc cc)))
    
		  

