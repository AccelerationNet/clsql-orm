(in-package :clsql-orm)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)

(defun sqlite3-list-tables (&key (schema *schema*) (owner nil))
  (declare (ignore schema))
  (iter top
    (for (type names) in
         `(("TABLE" ,(clsql-sys:list-tables :owner owner :database clsql-sys:*default-database*))
           ("VIEW" ,(clsql-sys:list-views :owner owner :database clsql-sys:*default-database*))))
    (iter (for i in names)
      (in top (collect (list i type))))))

(defun sqlite3-column-def (table column-name type is-null? default pk?)
  (let ((db-type (let ((idx (position #\( type)))
                   (intern-normalize-for-lisp
                    (if idx (subseq type 0 idx) type)
                    :keyword))))
    (make-instance 'column-def
                   :schema *schema*
                   :table table
                   :column column-name
                   :default default
                   :db-type db-type
                   :is-null (eql is-null? 0)
                   :constraints (when (= pk? 1) (list :primary-key))
                   :spec-type db-type)))

(defun sqlite3-list-columns (table &optional (schema *schema*))
  (declare (ignore schema))
  (iter
    (for col in (clsql:query #?"PRAGMA table_info(${table});"))
    ;; (for (col-idx column-name type is-null? default pk?) = col)
    (collect (apply #'sqlite3-column-def table (cdr col)))))
