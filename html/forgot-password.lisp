(cl:defpackage :max-ecm/html/forgot-password
  (:use :cl :max-ecm/entity)
  (:import-from :yasexml #:<>)
  (:export #:forgot-password))
(cl:in-package :max-ecm/html/forgot-password)

(defmethod yasexml:call-with-tag (function
				  (tag (eql 'forgot-password))
				  &rest attributes)
  (apply #'forgot-password (lambda () (funcall function tag)) attributes))

(defun forgot-password (thunk &key username)
  "https://gist.github.com/bMinaise/7329874"
  (declare (ignore thunk))
  (<> (:unescaped 
       '|
<div class="container">
    <div class="row">
        <div class="col-sm-6 col-md-4 col-md-offset-4">
            <h1 class="text-center login-title">Forgot Password?</h1>

            <div class="account-wall">|))
  
  (<> (div :class "profile-img glyphicon glyphicon-remove-circle"
	   :style "font-size:5em; color:red; text-align:center"
	   )
    (<> (:text "")))

       (<> (:unescaped '|
                <form class="form-signin"
                      action="#"
                      method="POST">
                <input name="u" type="text" class="form-control" 
                       placeholder="Username" required autofocus>
                <br/>

                  If you have a reason why you no longer have a
                  password, please enter it below.

                <br/>
                <textarea name="reason" class="form-control"> </textarea>
                <br/>
                <button class="btn btn-lg btn-primary btn-block" type="submit">
                    Send Request</button>

                </form>
            </div>
        </div>
    </div>
</div>|))
  (<> style (<> (:unescaped '|

.form-signin
{
    max-width: 330px;
    padding: 15px;
    margin: 0 auto;
}
.form-signin .form-signin-heading, .form-signin .checkbox
{
    margin-bottom: 10px;
}
.form-signin .checkbox
{
    font-weight: normal;
}
.form-signin .form-control
{
    position: relative;
    font-size: 16px;
    height: auto;
    padding: 10px;			;
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
    box-sizing: border-box;
}
.form-signin .form-control:focus
{
    z-index: 2;
}
.form-signin input[type="text"]
{
    margin-bottom: -1px;
    border-bottom-left-radius: 0;
    border-bottom-right-radius: 0;
}
.form-signin input[type="password"]
{
    margin-bottom: 10px;
    border-top-left-radius: 0;
    border-top-right-radius: 0;
}
.account-wall
{
    margin-top: 20px;
    padding: 40px 0px 20px 0px;
    background-color: #f7f7f7;
    -moz-box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3);
    -webkit-box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3);
    box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3);
}
.login-title
{
    color: #555;
    font-size: 18px;
    font-weight: 400;
    display: block;
}
.profile-img
{

   color:red;
    width: 96px;
    height: 96px;
    margin: 0 auto 10px;
    display: block;
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
}
.need-help
{
    margin-top: 10px;
}
.new-account
{
    display: block;
    margin-top: 10px;
}|))))
      


  
  
  
