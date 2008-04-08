(in-package :clsql-pg-introspect)
(enable-sql-reader-syntax)

;;;;; Utilities
(defmacro ensure-strings ((&rest vars) &body body)
  `(let ,(loop for var in vars
	       collect `(,var (if (stringp ,var)
				  ,var
				  (symbol-name ,var))))
    ,@body))

(defun relation-oid-sql (table)
  (declare (type (or symbol string) table))
  (ensure-strings (table)
    (clsql:sql-expression :string (format nil "'~A'::regclass" (normalize-for-sql table)))))

(defun tablename-for-oid (tableoid)
  (declare (type (or integer string) tableoid))
  (let* ((t-oid (if (integerp tableoid) ; cl-sql does not currently understand that oids are integers, so it (wisely) reads them as strings
		   tableoid
		   (parse-integer tableoid)))
	 (result (clsql:select [relname]
			       :from [pg_catalog.pg_class]
			       :where [= [oid] t-oid]
			       :flatp t)))
    (if result
	(car result)
	(error "Could not find table with oid ~A" t-oid))))

(defun column-number (table column)
  (let ((result (clsql:select [attnum]
			      :from [pg_catalog.pg_attribute]
			      :where [and [= [attrelid] (relation-oid-sql table)]
			                  [= [attname] column]]
			      :flatp t)))
    (if result
	(car result)
	(error "Could not find column ~A of table ~A" column table))))

(defun colname-for-number (table colnum)
  (let ((result (clsql:select [attname]
			      :from [pg_catalog.pg_attribute]
			      :where [and [= [attrelid] (relation-oid-sql table)]
			                  [= [attnum] colnum]]
			      :flatp t)))
    (if result
	(car result)
	(error "Could not find column number ~A of table ~A" colnum table))))

(defun internup (me &optional (package *package*))
  (intern (string-upcase me) package))

(defun intern-normalize-for-lisp (me &optional (package *package*))
  "Interns a string after uppercasing and flipping underscores to hyphens"
  (intern (substitute #\- #\_ (string-upcase me)) package))

(defun singular-intern-normalize-for-lisp (me &optional (package *package*))
  "Interns a string after uppercasing and flipping underscores to hyphens"
  (intern (substitute #\- #\_
		      (string-upcase (adwutils:singularize me))) package))

(defun normalize-for-sql (string)
  (substitute #\_ #\- string))

(defun intern-normalize-for-sql (me &optional (package *package*))
  "Interns a string after uppercasing and flipping underscores to hyphens"
  (intern (normalize-for-sql (string-upcase me)) package))

(defun clsql-join-column-name (table ref-table colname)
  (declare (ignorable table)
	   (ignorable ref-table))
  (let ((colname (princ-to-string (intern-normalize-for-lisp colname))))
    (intern-normalize-for-lisp
     (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner
				  "(.*[^\-])\-?id$"
				  :case-insensitive-mode T)
				 colname "\\1"  ))))

(defun clsql-column-definitions (table &key (generate-accessors t))
  "For each user column, find out if it's a primary key, constrain it to not null if necessary,
translate its type, and declare an initarg"
  (loop for row in (user-columns table)
	for (column type length prec not-null) = row
	collect `(,(intern-normalize-for-lisp column)
		  ,@(when generate-accessors `(:accessor ,(intern-normalize-for-lisp column)))
		  ,@(or (when (primary-key-p table column) '(:db-kind :key))
			(when (unique-p table column) '(:db-constraints :unique))
			(when not-null '(:db-constraints :not-null)))
		  :type ,(clsql-type-for-pg-type type length prec)
		  :initarg ,(intern-normalize-for-lisp column "KEYWORD"))))

(defun clsql-join-definitions (table &key (generate-accessors t))
  "Creates the definitions for the joins. Note that this does not handle multi-column foreign keys at the moment.
If you wish to have those, define a class that inherits from the generated one.
For that matter, if you wish to have custom names and the like, you'd best define an inheriting class"
  (loop for (home-key join-class foreign-key) in (list-foreign-constraints table)
	collect (let ((varname (clsql-join-column-name table join-class home-key)))
		  `(,varname
		    ,@(when generate-accessors
			    `(:accessor ,varname))
		    :db-kind :join
		    :db-info (:join-class ,(singular-intern-normalize-for-lisp join-class)
			      :home-key ,(intern-normalize-for-lisp home-key)
			      :foreign-key ,(intern-normalize-for-lisp foreign-key)
			      ,@(if (unique-p join-class
					      foreign-key)
				    '(:set nil)
				    '(:set t)))))))

;;;;; External-ish functions


(defun user-columns (table)
  "Returns a list of (column name, column type, column length, column precision) for the user columns of table.
Do not confuse a table with the clsql class of a table - this needs the actual table name.
User columns are those columns which the user defines. Others are defined for various reasons. OID is often
one of these."
  (declare (type (or string symbol) table))
  (mapcar
   #'(lambda (row)
       (destructuring-bind (colname typname attlen atttypmod not-null) row
         (let ((coltype (internup typname :keyword))
               collen
               colprec)
           (setf (values collen colprec)
                 (case coltype
                   ((:numeric :decimal)
                    (if (= -1 atttypmod)
                        (values nil nil)
                        (values (ash (- atttypmod 4) -16)
                                (boole boole-and (- atttypmod 4) #xffff))))
                   (otherwise
                    (values
                     (cond ((and (= -1 attlen) (= -1 atttypmod)) nil)
                           ((= -1 attlen) (- atttypmod 4))
                           (t attlen))
                     nil))))
           (list colname coltype collen colprec (when (string-equal not-null "t") T)))))
   (ensure-strings (table)
     (clsql:select [attname] [typname] [attlen] [atttypmod] [attnotnull]
                   :from (list [pg_catalog.pg_attribute] [pg_catalog.pg_type])
                   :where [and [= [pg_type.oid] [pg_attribute.atttypid]]
                   [= [attrelid] (relation-oid-sql table)]
                   [> [attnum] 0]
                   [= [attisdropped] [false]]]))))

(defun clsql-type-for-pg-type (pg-type attlen attprec)
  "Given a postgres type and a modifier, return the clsql type"
  (declare (type (or string symbol) pg-type)
	   (type (or integer null) attlen attprec))
  (ensure-strings (pg-type)
    (unless (string-equal
             "b"
             (car (clsql:select [typtype]
                                :from [pg_catalog.pg_type]
                                :where [= [typname]
                                          (string-downcase (string pg-type))]
                                :flatp t)))
      (error "I don't know how to deal with non-basetype ~A" pg-type)))
    (ecase pg-type
	((:int2 :int4 :int8) `(integer ,attlen))
	((:float2 :float4 :float8) `(float ,attlen))
	(:char 'char)
        (:bpchar `(string ,attlen))
	(:text 'string)
	(:varchar (if attlen `(varchar ,attlen) 'varchar))
        ((:numeric :decimal) (cond ((and attlen attprec)
                                    `(number ,attlen ,attprec))
                                   (attlen `(number ,attlen))
                                   (t 'number)))
	(:timestamp 'clsql-sys::wall-time)
	(:timestamptz 'clsql-sys::wall-time)
	(:date 'date)
	(:interval 'duration)
	(:bool 'generalized-boolean)
        ((:inet :cidr) '(varchar 43)) ; 19 for IPv4, 43 for IPv6
        (:macaddr '(varchar 17))
        ))

(defun primary-key-p (table column)
  "Given a table name and a column name, return whether that column is a primary key"
  (declare (type (or string symbol) table column))
  (ensure-strings (table column)
    (let ((colnumber (column-number table column)))
      (let ((response (clsql:select 1
				    :from [pg_catalog.pg_constraint]
				    :where [and [= [conrelid] (relation-oid-sql table)]
				                [= [contype] "p"]
				                [= (clsql:sql-expression :string "conkey[1]") colnumber]])))
	(if response
	    t
	    nil)))))

(defun unique-p (table column)
  "Returns whether a column is constrainted to unique or is a primary key (therefore effectively unique)"
  (declare (type (or string symbol) table column))
  (ensure-strings (table column)
    (let ((colnumber (column-number table column)))
      (let ((response (clsql:select 1
				    :from [pg_catalog.pg_constraint]
				    :where [and [= [conrelid] (relation-oid-sql table)]
				                [or [= [contype] "u"] [= [contype] "p"]]
						[= (clsql:sql-expression :string "conkey[1]") colnumber]])))
	(if response
	    t
	    nil)))))

(defun not-null-p (table column)
  "Returns true if a column is constrained to be not-null"
  (declare (type (or string symbol) table column))
  (ensure-strings (table column)
    (let ((result
	   (clsql:select [attnotnull]
			 :from [pg_catalog.pg_attribute]
			 :where [and [= [attrelid] (relation-oid-sql table)]
			             [= [attname] column]]
			 :flatp t)))
      (unless result
	(error "Could not find column ~A for table ~A" column table))
      (let ((truth (car result)))
	(cond
	  ((string-equal "f" truth) nil)
	  ((string-equal "t" truth) t)
	  (t (error "expecting 't' or 'f' but got ~A when trying to find out if column ~A of table ~A is not-null" truth column table)))))))


(defun list-foreign-constraints (table)
  "Returns (home-key foreign-table foreign-key) for each foreign constraint for the table"
  (declare (type (or string symbol) table))
  (ensure-strings (table)
    (loop for (local-colnum foreign-table-oid foreign-colnum)
	  in (clsql:select (clsql:sql-expression :string "conkey[1]")
			   [confrelid]
			   (clsql:sql-expression :string "confkey[1]")
			   :from [pg_catalog.pg_constraint]
			   :where [and [= [conrelid] (relation-oid-sql table)]
			               [= [contype] "f"] ; we're only interested in foreign keys
				       [= 1 (clsql:sql-expression :string "array_upper(conkey,1)")]
				       [= 1 (clsql:sql-expression :string "array_upper(confkey,1)")]])
	  collect (list (colname-for-number table local-colnum)
			(tablename-for-oid foreign-table-oid)
			(colname-for-number (tablename-for-oid foreign-table-oid) foreign-colnum)))))

;;;; most often used			    
; (remember: if defaults for this macro are changed, change the defaults for the next one as well!
(defmacro gen-view-class (table &key classname
				(generate-joins t)
				(generate-accessors t)
				(inherits-from ())
				(slots ()))
  "Generate a view class for clsql, given a table
If you want to name the class differently from the table, use the :classname keyword.
If you do not want to generate join information for the class, do :generate-joins nil
Note: if you specify a classname, join generation to this table won't work properly, as it depends on
table names and class names being the same.

The join slots/accessors will be named [home key]-[target table]. If you want to have your own
naming conventions, it's best to define a class that inherits from your generated class."
  (declare (type (or symbol string) table))
  (ensure-strings (table)
    (let ((classname (or classname
			 (singular-intern-normalize-for-lisp table))))
      `(clsql:def-view-class ,classname (,@inherits-from)
	,(append
	  (clsql-column-definitions table :generate-accessors generate-accessors)
	  (when generate-joins
	    (clsql-join-definitions table :generate-accessors generate-accessors))
	  slots)
	(:base-table ,(intern-normalize-for-lisp table)))
	
	)))

(defmacro gen-view-classes-for-database ((connection-spec
					  database-type
					  &key
					  (generate-joins t)
					  (generate-accessors t)
					  (inherits-from ()))
					 &rest classes)
  "This is the function most people will use to generate table classes at compile time.
You feed it how to connect to your database, and it does it at compile time. It uses gen-view-class.
The code for this function is instructive if you're wanting to do this sort of thing at compile time."
  (let ((db (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (with-database (,db ,connection-spec :database-type ,database-type :if-exists :new :make-default nil)
	(with-default-database (,db)
	  (eval '(progn ,@(mapcar (lambda (class) `(clsql-pg-introspect:gen-view-class ,class
						    :generate-joins ,generate-joins
						    :inherits-from ,inherits-from
						    :generate-accessors ,generate-accessors))
				  classes))))))))

(disable-sql-reader-syntax)