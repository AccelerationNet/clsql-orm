(in-package :clsql-orm)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)
(defvar *schema* () "The schema we are generating from " )
(defvar *export-symbols* () "Should we export every symbol we intern? " )
(defvar *singularize* () "Should we try to singularize table names " )
(defvar *db-model-package* *package*)

;;;;; Utilities
(defun ensure-list (x) (typecase x (list x) (t (list x))))
(defmacro awhen (cond &body body)
  `(let ((it ,cond)) (when it ,@body)))

(defmacro ensure-strings ((&rest vars) &body body)
  `(let ,(loop for var in vars
               collect `(,var (if (stringp ,var)
                                  ,var
                                  (symbol-name ,var))))
    ,@body))


(defun internup (me &optional (package *db-model-package*))
  (let ((r (intern (string-upcase me) package)))
    (when *export-symbols* (export r package))
    r))

(defun intern-normalize-for-lisp (me &optional (package *db-model-package*))
  "Interns a string after uppercasing and flipping underscores to hyphens"
  (let ((r (symbol-munger:underscores->lisp-symbol me package)))
    (when *export-symbols* (export r package))
    r))

(defun singular-intern-normalize-for-lisp (me &optional (package *db-model-package*))
  "Interns a string after uppercasing and flipping underscores to hyphens"
  (let ((words (reverse (cl-ppcre:split "-|_" (string me))))
        (cl-interpol:*list-delimiter* "-"))
    (setf (first words) (cl-inflector:singular-of (first words)))
    (internup #?"@{ (reverse words) }" package)))

(defun normalize-for-sql (s)
  (symbol-munger:lisp->underscores s :capitalize nil))

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

(defclass column-def ()
  ((schema :accessor schema :initarg :schema :initform *schema*)
   (table :accessor table :initarg :table :initform nil)
   (column :accessor column :initform nil :initarg :column)
   (db-type :accessor db-type :initform nil :initarg :db-type)
   (spec-type :accessor spec-type :initform nil :initarg :spec-type
              :documentation "the original database type rather than its clsql/lisp keyword")
   (col-length :accessor col-length :initform nil :initarg :col-length)
   (scale :accessor scale :initform nil :initarg :scale)
   (is-null :accessor is-null :initform nil :initarg :is-null)
   (default :accessor default :initform nil :initarg :default)
   (constraints :accessor constraints :initform nil :initarg :constraints)
   (fkey-schema :accessor fkey-schema :initform nil :initarg :fkey-schema)
   (fkey-table :accessor fkey-table :initform nil :initarg :fkey-table)
   (fkey-col :accessor fkey-col :initform nil :initarg :fkey-col)))

(defun col= (c1 c2)
  (and
   (string-equal (clsql-orm::table c1) (clsql-orm::table c2))
   (string-equal (clsql-orm:column c1) (clsql-orm:column c2))
   (string-equal (clsql-orm:spec-type c1) (clsql-orm:spec-type c2))))

(defmethod print-object ((o column-def) s)
  "Print the database object, and a couple of the most common identity slots."
  (print-unreadable-object (o s :type t :identity t)
    (format s "~a.~a.~A"
            (ignore-errors (schema o))
            (ignore-errors (table o))
            (ignore-errors (column o)))))

(defun column-def (schema table column db-type col-length
                   scale is-null default constraints
                   fkey-schema fkey-table fkey-col)
  (make-instance 'column-def
                 :schema schema
                 :table table
                 :column column :db-type db-type :col-length col-length
                 :scale scale
                 :is-null is-null :default default :constraints constraints
                 :fkey-schema fkey-schema :fkey-table fkey-table :fkey-col fkey-col
                 :spec-type db-type))

(defun clsql-column-definitions (table &key
                                  (schema *schema*)
                                  (generate-accessors t)
                                  (generate-joins T))
  "For each user column, find out if it's a primary key, constrain it to not null if necessary,
translate its type, and declare an initarg"
  (let ((cols (list-columns table schema)))
    (unless cols
      (error "Could not find any columns for table: ~a in schema: ~a.
              Are you sure you correctly spelled the table name?
              Are you sure you sure this database connection has access to the table?"
             table schema))
    (iter (for col in cols)
      (with-accessors ((column column) (db-type db-type) (col-length col-length)
                       (is-null is-null) (default default) (constraints constraints)
                       (fkey-table fkey-table) (fkey-col fkey-col)) col
        (when (and is-null default)
          (warn "CLSQL-ORM: The column ~a.~a.~a should not be null and have a default value (~a)"
                schema table column default))
        ;; TODO: add a "skip column" and "use explict type" restart
        (collect `(,(intern-normalize-for-lisp column)
                   :column ,column
                   ,@(when generate-accessors
                       `(:accessor ,(accessor-name-for-column table column)))
                   ,@(when (member :primary-key constraints)
                       '(:db-kind :key))
                   :db-constraints
                   (
                    ,@(unless is-null '(:not-null))
                    ,@(when (and (member :primary-key constraints)
                                 (identity-column-p table column))
                        '(:identity))
                    )
                   ,@(cond
                       ( ;; its null with no default
                        (and is-null (null default))
                        '(:initform nil))

                       ;; we have a not-nullable boolean column
                       ;; (which should have a default)
                       ((and (not is-null) (eql db-type :boolean) default)
                        `(:initform ,(not (or (string-equal default "false")
                                           (string-equal default "0"))))
                        )
                       )
                   :type ,(clsql-type-for-db-type db-type col-length)
                   :initarg ,(intern-normalize-for-lisp column :keyword)))
        (when (and
               generate-joins
               (member :foreign-key constraints))
          (if (and fkey-table fkey-col)
              (collect (clsql-join-definition table column fkey-table fkey-col
                                              :generate-accessors generate-accessors))
              (warn "CLSQL-ORM: Could not generate a join for: ~a.~a.~a because we couldnt read the relationship. Perhaps your permissions should be adjusted"
                    schema table column)))))))

(defun clsql-join-definition (home-table home-key foreign-table foreign-key
                              &key (generate-accessors t))
  "Creates the definition for the joins. Note that this does not handle multi-column foreign keys at the moment.
For that matter, if you wish to have custom names and the like, you'd best define an inheriting class"
  (let ((varname (clsql-join-column-name home-table foreign-table home-key)))
    `(,varname
      ,@(when generate-accessors
          `(:accessor ,varname))
      :db-kind :join
      :db-info (:join-class ,(if *singularize*
                                 (singular-intern-normalize-for-lisp foreign-table)
                                 (intern-normalize-for-lisp foreign-table))
                :home-key ,(intern-normalize-for-lisp home-key)
                :foreign-key ,(intern-normalize-for-lisp foreign-key)
                :set nil
                                        ;   ,@(if (unique-p join-class foreign-key)
                                        ;         '(:set nil)
                                        ;         '(:set t))
                ))))

(defun clsql-reverse-join-definition (column-def
                                      &key (generate-accessors t))
  (let* ((include-schema? (not (string-equal (schema column-def)
                                             (fkey-schema column-def))))
         (foreign-table (table column-def))
         (home-key (fkey-col column-def))
         (foreign-key (column column-def))
         (varname (clsql-orm::intern-normalize-for-lisp
                   (list
                    (when include-schema? (schema column-def))
                    foreign-table foreign-key 'join))))
    `(,varname
      ,@(when generate-accessors
          `(:accessor ,varname))
      :db-kind :join
      :db-info (:join-class ,(if *singularize*
                                 (singular-intern-normalize-for-lisp foreign-table)
                                 (intern-normalize-for-lisp foreign-table))
                :home-key ,(intern-normalize-for-lisp home-key)
                :foreign-key ,(intern-normalize-for-lisp foreign-key)
                :set t
                ))))

(defun clsql-reverse-join-definitions ( table &key (schema *schema*))
  (let ((columns (list-reverse-join-columns table :schema schema)))
    (iter (for c in columns)
      (collect (clsql-reverse-join-definition
                c)))))

;;;;; External-ish functions

(defun identity-column-p (table column)
  "a function that can determine if a key column is IDENTITY for sqlserver"
  (when (and (find-package :clsql-odbc)
             (typep clsql-sys:*default-database*
                    (intern "ODBC-DATABASE" :clsql-odbc)))
    (setf table (clsql-sys:sql-escape-quotes (normalize-for-sql table)))
    (setf column (clsql-sys:sql-escape-quotes (normalize-for-sql column)))
    (handler-case
        (eql 1 (first (clsql:query #?"select COLUMNPROPERTY(object_id('${table}'), '${column}', 'IsIdentity')"
                                   :flatp T)))
      (error (c)
        (warn "Error querying about IDENTITY ~a" c)))))

(defun list-columns (table &optional (schema *schema*))
  (case (clsql-sys::database-underlying-type clsql-sys:*default-database*)
    (:sqlite3 (sqlite3-list-columns table schema))
    (T (default-list-columns table :schema schema))))

(defun list-reverse-join-columns (table &key (schema *schema*))
  (default-list-columns table :schema schema :reverse-joins? t))

(defun default-list-columns ( table &key (schema *schema*) (reverse-joins? nil)
                              &aux where order)
  "Returns a list of
   #(column type length is-null default (key-types) fkey-table fkey-col)
   for the user columns of table.
   Do not confuse a table with the clsql class of a table - this needs the actual table name.
   User columns are those columns which the user defines. Others are defined for various reasons. OID is often
   one of these."
  (declare (type (or string symbol) table))

  (setf table (clsql-sys:sql-escape-quotes (normalize-for-sql table)))
  (setf schema (clsql-sys:sql-escape-quotes (normalize-for-sql schema)))
  (setf where
        (if reverse-joins?
            #?"fkey.table_name = '${table}' and fkey.table_schema= '${schema}'"
            #?"cols.table_schema ='${schema}' AND cols.table_name ='${table}'"))
  (setf order
        (if reverse-joins?
            #?"fkey.table_name, fkey.column_name"
            #?"cols.table_name, cols.column_name, cols.data_type"))

  (let* ((sql #?"
SELECT
  cols.table_schema,
  cols.table_name,
  cols.column_name, cols.data_type,
  COALESCE(cols.character_maximum_length,
  cols.numeric_precision),
  cols.numeric_scale,
  cols.is_nullable,
  cols.column_default,
  cons.constraint_type,
  fkey.table_schema,
  fkey.table_name,
  fkey.column_name

FROM information_schema.columns as cols
LEFT JOIN information_schema.key_column_usage as keycols
  ON keycols.column_name = cols.column_name
  AND keycols.table_name = cols.table_name
  AND keycols.table_schema = cols.table_schema
LEFT JOIN information_schema.table_constraints as cons
  ON cons.constraint_name  = keycols.constraint_name
  AND cons.constraint_schema = cons.constraint_schema

LEFT JOIN information_schema.referential_constraints as refs
  ON  refs.constraint_schema = cons.constraint_schema
  AND refs.constraint_name = cons.constraint_name

LEFT JOIN information_schema.key_column_usage as fkey
  ON fkey.constraint_schema = refs.unique_constraint_schema
  AND fkey.constraint_name = refs.unique_constraint_name

WHERE ${where}

ORDER BY ${order}
")
         (lesser-sql #?"
SELECT
  DISTINCT 
  cols.table_name,
  cols.column_name, cols.data_type,
  COALESCE(cols.character_maximum_length,
  cols.numeric_precision),
  cols.numeric_scale,
  cols.is_nullable,
  cols.column_default,
  null as constraint_type,
  null as table_name,
  null as column_name

FROM information_schema.columns as cols
LEFT JOIN information_schema.key_column_usage as keycols
  ON keycols.column_name = cols.column_name
  AND keycols.table_name = cols.table_name
  AND keycols.table_schema = cols.table_schema

WHERE ${where}
ORDER BY ${order}
")
         (results (or (clsql:query sql :flatp T)
                      (and (not reverse-joins?)
                           (clsql:query lesser-sql :flatp T)))))
                                        ;(print results)
    (iter
      (with prev-row)
      (for l-row in results)
      (for row = (apply #'column-def l-row))
      (cond
        ((not (and prev-row
                   (string-equal (table row) (table prev-row))
                   (string-equal (column row) (column prev-row))))
         (setf (db-type row) (symbol-munger:english->keyword (db-type row)))
         (setf (is-null row) (string-equal (is-null row) "YES"))
         (setf (constraints row) (list (symbol-munger:english->keyword (constraints row))))
         (collect row)
         (setf prev-row row))
        (T ;; if we got a second row it means the column has more than one constraint
         ;; we should put that in the constraints list
         (push (symbol-munger:english->keyword (constraints row))
               (constraints prev-row))
         (awhen (fkey-table row)
           (setf (fkey-table prev-row) it))
         (awhen (fkey-col row)
           (setf (fkey-col prev-row) it)))))))


;; TODO: add condition for don't know how to convert db type

(defun clsql-type-for-db-type (db-type len)
  "Given a postgres type and a modifier, return the clsql type"
  (declare (type symbol db-type)
           (type (or integer null) len))
  (ecase db-type
    ((:smallint :tinyint :bigint :int :int2 :int4 :int8 :integer)
     'integer)
    ((:float :float2 :float4 :float8 :double-precision)
     'double-float)
    ((:text :ntext :longtext) 'varchar)
    ;; weird Postgresql types
    ((:point :user-defined) 'varchar)
    ((:char :bpchar :varchar :nvarchar :character-varying :character :string)
     (if len `(varchar ,len) 'varchar))
    ((:numeric :decimal :money)
     'number)
    ((:datetime
      :timestamptz
      :timestamp
      :timestamp-with-time-zone
      :timestamp-without-time-zone)
     'clsql-sys::wall-time)
    ((:date :smalldatetime)
     'date)
    (:interval 'duration)
    (:uniqueidentifier 'number)
    ((:bool :boolean :bit) 'boolean)
    ((:inet :cidr) '(varchar 43))       ; 19 for IPv4, 43 for IPv6
    (:macaddr '(varchar 17))
    (:uuid '(varchar 36)) ; eg a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11
    (:image '(vector (unsigned-byte 8)))
    ))


(defun list-tables (&optional (schema *schema*))
  (case (clsql-sys::database-underlying-type clsql-sys:*default-database*)
    (:sqlite3 (sqlite3-list-tables :schema schema))
    (T
     (clsql:select [table_name] [table_type]
                   :from [information_schema.tables]
                   :flatp T
                   :where [= [UPPER [table_schema]] (string-upcase schema)]))))

(defun column-diff (table-1 table-2 &key (schema-1 *schema*) (schema-2 *schema*))
  (let ((cols1 (list-columns table-1 schema-1))
        (cols2 (list-columns table-2 schema-2)))
    (list
     (iter (for c in cols1)
       (unless (find c cols2 :test #'col=) (collect c)))
     (iter (for c in cols2)
       (unless (find c cols1 :test #'col=) (collect c))))
    ))

(defun gen-view-class (table &key classname
                        (is-view nil)
                        (generate-joins t)
                        (generate-reverse-joins t)
                        (generate-accessors t)
                        (inherits-from ())
                        (view-inherits-from ())
                        (package *package*)
                        (nicknames ())
                        (singularize T)
                        (schema "public")
                        (metaclass ())
                        (slots ())
                        (export-symbols ())
                        (print? nil))
  "Generate a view class for clsql, given a table
If you want to name the class differently from the table, use the :classname keyword.
If you do not want to generate join information for the class, do :generate-joins nil
Note: if you specify a classname, join generation to this table won't work properly, as it depends on
table names and class names being the same.

The join slots/accessors will be named [home key]-[target table]. If you want to have your own
naming conventions, it's best to define a class that inherits from your generated class."
  (declare (type (or symbol string) table))
  (unless view-inherits-from (setf view-inherits-from inherits-from))
  (restart-case
      (ensure-strings (table schema)
        (let* ((*schema* schema)
               (*export-symbols* export-symbols)
               (*singularize* singularize)
               (*db-model-package* (or (find-package package)
                                       (make-package package :nicknames (ensure-list nicknames) :use ())))
               (class (or classname
                          (and singularize
                               (singular-intern-normalize-for-lisp table))
                          (intern-normalize-for-lisp table)))
               (columns (clsql-column-definitions
                         table
                         :generate-accessors generate-accessors
                         :generate-joins generate-joins))
               (reverse-joins (when generate-reverse-joins
                                (clsql-reverse-join-definitions table))))
          (let ((form `(clsql:def-view-class ,class (,@(if is-view view-inherits-from inherits-from))
                        ,(append columns reverse-joins slots)
                        (:base-table ,table)
                        ,@(when metaclass
                            `((:metaclass ,metaclass))))))
            (when print? (format *trace-output* "~%~s~%" form))
            (eval form))))
    (skip-view-class ()
      :report (lambda (str) (format str "Skip generating class for table ~a" table)))))

(defun %tables-to-generate (classes excludes schema)
  (setf excludes (mapcar #'princ-to-string (ensure-list excludes)))
  (flet ((exclude? (table)
           (member table excludes :test #'string-equal)))
    (if classes
        (iter (for class in classes)
          ;; building (table-name type) where type is VIEW or TABLE
          (for c = (typecase class
                     (list (normalize-for-sql (first class))
                      (second class))
                     (symbol (list (normalize-for-sql class) nil))
                     (string (list class nil))))
          (unless (exclude? (first c)) (collect c)))
        (iter (for pair in (list-tables schema))
          (unless (exclude? (first pair))
            (collect pair))))))

(defun gen-view-classes (&key
                         (classes)
                         (excludes)
                         (generate-joins t)
                         (generate-reverse-joins t)
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
  (setf classes (%tables-to-generate classes excludes schema))
  (iter (for (table type) in classes)
        (gen-view-class
         table
         :generate-joins generate-joins
         :generate-reverse-joins generate-reverse-joins
         :is-view (string-equal type "VIEW")
         :view-inherits-from view-inherits-from
         :inherits-from inherits-from
         :singularize singularize
         :export-symbols export-symbols
         :schema schema
         :package package
         :nicknames (ensure-list nicknames)
         :metaclass metaclass
         :generate-accessors generate-accessors)))
