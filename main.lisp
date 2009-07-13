(in-package :clsql-orm)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)
(defvar *schema* () "The schema we are generating from " )
(defvar *export-symbols* () "Should we export every symbol we intern? " )
(defvar *singularize* () "Should we try to singularize table names " )
(defvar *db-model-package* *package*)

;;;;; Utilities
(defmacro ensure-strings ((&rest vars) &body body)
  `(let ,(loop for var in vars
	       collect `(,var (if (stringp ,var)
				  ,var
				  (symbol-name ,var))))
    ,@body))


(defun internup (me &optional (package *db-model-package*))
  (let ((r (intern (string-upcase me) package)))
    (when *export-symbols*
      (export r package))
    r))

(defun intern-normalize-for-lisp (me &optional (package *db-model-package*))
  "Interns a string after uppercasing and flipping underscores to hyphens"
  (internup (substitute #\- #\_ me) package))

(defun singular-intern-normalize-for-lisp (me &optional (package *db-model-package*))
  "Interns a string after uppercasing and flipping underscores to hyphens"
  (intern-normalize-for-lisp
   (if *singularize*
       (adwutils:singularize me)
       me)
   package))

(defun normalize-for-sql (string)
  (substitute #\_ #\- string))

(defun clsql-join-column-name (table ref-table colname)
  (declare (ignorable table)
	   (ignorable ref-table))
  (let ((colname (princ-to-string (intern-normalize-for-lisp colname))))
    (intern-normalize-for-lisp
     (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner
				  "^(.*?)\-?(id|key)?$"
				  :case-insensitive-mode T)
				 colname "\\1-JOIN"))))

(defun accessor-name-for-column (table column)
  (let ((default-sym (intern-normalize-for-lisp column)))
    (if (eql (symbol-package default-sym) (find-package :cl))
	(intern-normalize-for-lisp #?"${table}.${column}")
	default-sym)))

(defun clsql-column-definitions (table &key
				  (schema *schema*)
				  (generate-accessors t)
				  (generate-joins T))
  "For each user column, find out if it's a primary key, constrain it to not null if necessary,
translate its type, and declare an initarg"
  (let ((cols (user-columns table schema)))
    (unless cols
      (error "Could not find any columns for table: ~a in schema: ~a.
              Are you sure you correctly spelled the table name?"
	     table schema))
    (iter (for row in cols)
	  (for (column type length is-null default key-type fkey-table fkey-col) = row)
	  (collect `(,(intern-normalize-for-lisp column)
		      ,@(when generate-accessors
			  `(:accessor ,(accessor-name-for-column table column)))
		      ,@(when (eql key-type :primary-key)
			  '(:db-kind :key))
		      ,@(unless is-null
			  '(:db-constraints :not-null))
		      ,@(when (and is-null (null default)) '(:initform nil))
		      :type ,(clsql-type-for-db-type type length)
		      :initarg ,(intern-normalize-for-lisp column :keyword)))
	  (when (and
		 generate-joins
		 (eql key-type :foreign-key))
	    (collect (clsql-join-definition table column fkey-table fkey-col
					    :generate-accessors generate-accessors))))))

(defun clsql-join-definition (home-table home-key foreign-table foreign-key
			      &key (generate-accessors t))
  "Creates the definition for the joins. Note that this does not handle multi-column foreign keys at the moment.
For that matter, if you wish to have custom names and the like, you'd best define an inheriting class"
  (let ((varname (clsql-join-column-name home-table foreign-table home-key)))
    `(,varname
      ,@(when generate-accessors
	  `(:accessor ,varname))
      :db-kind :join
      :db-info (:join-class ,(singular-intern-normalize-for-lisp foreign-table)
			    :home-key ,(intern-normalize-for-lisp home-key)
			    :foreign-key ,(intern-normalize-for-lisp foreign-key)
			    :set nil
			;   ,@(if (unique-p join-class foreign-key)
			;	  '(:set nil)
			;	  '(:set t))
			    ))))

;;;;; External-ish functions


(defun user-columns ( table &optional (schema *schema*))
  "Returns a list of
   (column type length is-null default key-type fkey-table fkey-col)
   for the user columns of table.
   Do not confuse a table with the clsql class of a table - this needs the actual table name.
   User columns are those columns which the user defines. Others are defined for various reasons. OID is often
   one of these."
  (declare (type (or string symbol) table))
  (mapcar
   #'(lambda (row)
       (destructuring-bind (column type length is-null default key-type fkey-table fkey-col)
	   row
	 (setf type (adwutils:symbolize-string type :keyword))
	 (setf is-null (string-equal is-null "YES"))
	 (setf key-type (adwutils:symbolize-string key-type :keyword))
	 (list column type length is-null default key-type fkey-table fkey-col)))
   (ensure-strings (table schema)
     (setf table (clsql-sys:sql-escape-quotes (string-upcase table)))
     (setf schema (clsql-sys:sql-escape-quotes (string-upcase schema)))
     
     (clsql:query #?"
SELECT cols.column_name, cols.data_type, 
  COALESCE(cols.character_maximum_length, 
  cols.numeric_precision), 
  cols.is_nullable, 
  cols.column_default,
  cons.constraint_type,
  fkey.table_name,
  fkey.column_name

FROM information_schema.columns as cols
LEFT JOIN information_schema.key_column_usage as keys
  ON keys.column_name = cols.column_name
  AND keys.table_name = cols.table_name
  AND keys.table_schema = cols.table_schema
LEFT JOIN information_schema.table_constraints as cons
  ON cons.constraint_name  = keys.constraint_name
  AND cons.constraint_schema = cons.constraint_schema

LEFT JOIN information_schema.referential_constraints as refs
  ON  refs.constraint_name = cons.constraint_name
  AND refs.constraint_name = cons.constraint_name

LEFT JOIN information_schema.key_column_usage as fkey
  ON fkey.constraint_schema = refs.unique_constraint_schema
  AND fkey.constraint_name = refs.unique_constraint_name

WHERE UPPER(cols.table_schema) ='${schema}'
 AND UPPER(cols.table_name) ='${table}'
"
		  :flatp T
		  ))))

(defun clsql-type-for-db-type (db-type len)
  "Given a postgres type and a modifier, return the clsql type"
  (declare (type (or string symbol) db-type)
	   (type (or integer null) len))
  
    (ecase db-type
      ((:smallint :tinyint :bigint :int :int2 :int4 :int8 :integer)
	 'integer)
      ((:float :float2 :float4 :float8 :double-precision)
	 'double-float)
      ((:text :ntext) 'varchar)
      ((:char :bpchar :varchar :nvarchar)
	 (if len `(varchar ,len) 'varchar))
      ((:numeric :decimal :money)
	 'number)
      ((:datetime :timestamptz :timestamp :timestamp-with-time-zone)
	 'clsql-sys::wall-time)
      ((:date :smalldatetime)
	 'date)
      (:interval 'duration)
      ( :uniqueidentifier 'number)
      ((:bool :boolean :bit) 'boolean)
      ((:inet :cidr) '(varchar 43))	; 19 for IPv4, 43 for IPv6
      (:macaddr '(varchar 17))
      (:image '(vector (unsigned-byte 8)))
      ))

;;;; most often used			    
; (remember: if defaults for this macro are changed, change the defaults for the next one as well!
(defun list-tables (&optional (schema *schema*))
  (clsql:select [table_name] [table_type]
    :from [information_schema.tables]
    :flatp T
    :where [= [UPPER [table_schema]] (string-upcase schema)]))

(defun gen-view-class (table &key classname
			     (is-view nil)
			     (generate-joins t)
			     (generate-accessors t)
			     (inherits-from ())
			     (view-inherits-from ())
			     (package *package*)
			     (nicknames ())
			     (singularize T)
			     (schema "public")
			     (metaclass ())
			     (slots ())
			     (export-symbols ()))
  "Generate a view class for clsql, given a table
If you want to name the class differently from the table, use the :classname keyword.
If you do not want to generate join information for the class, do :generate-joins nil
Note: if you specify a classname, join generation to this table won't work properly, as it depends on
table names and class names being the same.

The join slots/accessors will be named [home key]-[target table]. If you want to have your own
naming conventions, it's best to define a class that inherits from your generated class."
  (declare (type (or symbol string) table))
  (unless view-inherits-from (setf view-inherits-from inherits-from))
  (ensure-strings (table)
    (let* ((*schema* schema)
	   (*export-symbols* export-symbols)
	   (*singularize* singularize)
	   (*db-model-package* (or (find-package package)
				   (make-package package :nicknames (arnesi:ensure-list nicknames) :use ())))
	   (class (or classname
		      (and singularize
			   (singular-intern-normalize-for-lisp table))
		      (intern-normalize-for-lisp table)))
	   (columns (clsql-column-definitions
		     table
		     :generate-accessors generate-accessors
		     :generate-joins generate-joins)))
      (eval
       `(clsql:def-view-class ,class (,@(if is-view view-inherits-from inherits-from))
	  ,(append
	    columns
	    slots)
	  (:base-table ,(intern-normalize-for-lisp table))
	  ,@(when metaclass
	      `((:metaclass ,metaclass))))))))

(defun gen-view-classes (&key
			 (classes)
			 (generate-joins t)
			 (generate-accessors t)
			 (schema "public")
			 (package *package*)
			 (export-symbols ())
			 (nicknames ())
			 (singularize T)
			 (inherits-from ())
			 (view-inherits-from ())
			 (metaclass ()))
  "This is the function most people will use to generate table classes. It uses gen-view-class.

   This function will operate on the default clsql database
  "
  (unless view-inherits-from (setf view-inherits-from inherits-from))
  (iter (for (table type) in (or classes (list-tables schema)))
	(gen-view-class
	 table
	 :generate-joins generate-joins
	 :is-view (string-equal type "VIEW")
	 :view-inherits-from view-inherits-from
	 :inherits-from inherits-from
	 :singularize singularize
	 :export-symbols export-symbols
	 :schema schema
	 :package package
	 :nicknames (arnesi:ensure-list nicknames)
	 :metaclass metaclass
	 :generate-accessors generate-accessors)))

(defmacro gen-view-classes-for-database ((connection-spec
					  database-type
					  &key
					  (generate-joins t)
					  (generate-accessors t)
					  (schema "public")
					  (package *package*)
					  (export-symbols ())
					  (nicknames ())
					  (singularize T)
					  (inherits-from ())
					  (view-inherits-from ())
					  (metaclass ()))
					 &rest classes)
  "This is the function most people will use to generate table classes at compile time.
You feed it how to connect to your database, and it does it at compile time. It uses gen-view-class.
The code for this function is instructive if you're wanting to do this sort of thing at compile time."
  (unless view-inherits-from (setf view-inherits-from inherits-from))
  (let ((db (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (with-database (,db ,connection-spec :database-type ,database-type :if-exists :new :make-default nil)
	 (with-default-database (,db)
	   (mapcar (lambda (table-info)
		     (destructuring-bind (class type) table-info
		       (clsql-orm:gen-view-class
			class
			:generate-joins ,generate-joins
			:inherits-from ',inherits-from
			:view-inherits-from view-inherits-from
			:is-view (string-equal type "VIEW")
			:singularize ,singularize
			:export-symbols ,export-symbols
			:schema ,schema
			:package ,package
			:nicknames ',(arnesi:ensure-list nicknames)
			:metaclass ',metaclass
			:generate-accessors ,generate-accessors)))
		   (or ',classes (list-tables ,schema))))))))

(disable-sql-reader-syntax)