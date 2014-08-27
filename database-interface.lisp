(cl:defpackage :max-ecm/database-interface
  (:use :cl)
  (:export
   #:raw
   #:describe-sql
   #:query
   "WITH-DATABASE"
   "CALL-WITH-DATABASE"
   "NULL->NIL"))
(cl:in-package :max-ecm/database-interface)

(defun query (query &key 
		      (format :plists))
  (cl-postgres:exec-query
   postmodern:*database*
   (etypecase query
     (string query)
     (list (s-sql:sql-compile query)))
   (second (first (postmodern::reader-for-format format)))))
	 

(defun describe-sql (&key (table nil))
  (assert table () "DESCRIBE must have a table")
  (postmodern:query 
   "SELECT 
         a.attname AS Field,
         t.typname || '(' || a.atttypmod || ')' AS Type,
         CASE WHEN a.attnotnull = 't' THEN 'YES' ELSE 'NO' END AS Not_Null,
         CASE WHEN r.contype = 'p' THEN 'PRI' ELSE '' END AS Key,
         (SELECT substring(pg_catalog.pg_get_expr(d.adbin, d.adrelid), '.*')
                 FROM
                         pg_catalog.pg_attrdef d
                 WHERE
                         d.adrelid = a.attrelid
                         AND d.adnum = a.attnum
                         AND a.atthasdef) AS Default,
        '' as Extras
FROM
        pg_class c 
        JOIN pg_attribute a ON a.attrelid = c.oid
        JOIN pg_type t ON a.atttypid = t.oid
        LEFT JOIN pg_catalog.pg_constraint r ON c.oid = r.conrelid 
                AND r.conname = a.attname
WHERE
        c.relname = $1
        AND a.attnum > 0
        
ORDER BY a.attnum" table :plists))

;; This variable is defparametered in
(defvar *database-connection-parameters*)

;; This variable MAY be populated elsewhere based on the current
;; server deployment environment, and should be a vector of lists of
;; database parameters, but if it is not then NIL means to use
;; *DATABASE-CONNECTION-PARAMETERS* instead.
(defvar *read-only-database-connection-parameters* nil)

;; A minor cleverness here: This value is never used pre-increment, so
;; an initial value of -1 means that the first database to be used is
;; the first in the set and not the second.
(defvar *next-read-only-database-index* -1)

;; There are multiple clevernesses here.  First, we take a reference
;; to *READ-ONLY-DATABASE-CONNECTION-PARAMETERS* at the start and just
;; use that reference going forward, so if it gets replaced with an
;; array of a different length halfway through we don't get bit.
;; Second, we use the post-incremented value of
;; *NEXT-READ-ONLY-DATABASE-INDEX*, so that if the array has shrunk
;; since the last time someone queried it then we get a value that is
;; taken modulo the new size (so that we don't get bit by the array
;; changing between calls, either).  Third, it turns out that any
;; number modulo one is zero, so we don't have to special-case only
;; having a single usable read-only database.  Fourth, the CAS acts as
;; a memory barrier, both before and after, meaning that only the read
;; of *READ-ONLY-DATABASE-CONNECTION-PARAMETERS* is unprotected by a
;; memory barrier, and there are enough other barriers strewn
;; throughout the system (garbage collection's stop-the-world alone
;; counts as one, as does an allocation-overflow trap, as should most
;; system calls) that the likelyhood of the cached copy being stale is
;; minimal to say the least, and will never be stale more than once
;; per CPU per update.  Fifth, if there are no read-only databases
;; defined, er substitute the parameters for the master database.
;; Sixth, this comment is just over half again as long (by count of
;; lines) as the function that is its subject.
(defun next-read-only-database-parameters ()
  (let ((parameter-array *read-only-database-connection-parameters*))
    (when (null parameter-array)
      (return-from next-read-only-database-parameters
	*database-connection-parameters*))
    (loop
       with length = (length parameter-array)
       for old-next = *next-read-only-database-index*
       for new-next = (mod (1+ old-next) length)
       until (eq old-next
		 (sb-ext:compare-and-swap
		  (symbol-value '*next-read-only-database-index*)
		  old-next new-next))
       finally (return (aref parameter-array new-next)))))

;; True if the current database connection (presuming that there is a
;; current database connection) is "writable".  Otherwise, NIL.
(defvar *database-writable-p* nil)

(defun connect-to-database (writable-p)
  (apply #'postmodern:connect
	 (if writable-p
	     *database-connection-parameters*
	     (next-read-only-database-parameters))))

(defmacro with-database ((&key writable) &body body)
  (let ((thunk-name (gensym "WITH-DATABASE-THUNK-")))
    `(flet ((,thunk-name ()
	      ,@body))
       (declare (dynamic-extent #',thunk-name))
       (call-with-database #',thunk-name ,writable))))

(defun call-with-database (fun writable-p)
  (let ((usable-existing-database
	 (unless (and writable-p
		      (not *database-writable-p*))
	   postmodern:*database*))
        (postmodern:*database* nil)
	(*database-writable-p*
	 (or *database-writable-p*
	     writable-p)))
    (handler-bind (((or cl-postgres-error:admin-shutdown
                        cl-postgres-error:server-shutdown
                        cl-postgres-error:crash-shutdown
                        cl-postgres:database-connection-lost)
                     (lambda (e)
                       (declare (ignore e))
                       (invoke-restart :reconnect))))
      (unwind-protect
           (progn
             (setf postmodern:*database*
                   (or usable-existing-database
                       (connect-to-database writable-p)))
             (funcall fun))
        (unless usable-existing-database
          (postmodern:disconnect postmodern:*database*))))))

;; KLUDGE: Postmodern (well, S-SQL) doesn't include operators for
;; full-text search.  We just need the one, and this is no worse a
;; place to set it up than any easily accessible, and better than
;; most.
(s-sql:register-sql-operators :2+-ary :@@)

(defun null->nil (value)
  "Convert database NULLs (:NULL) to NIL."
  (if (eq value :null)
      nil
      value))

;;; EOF
