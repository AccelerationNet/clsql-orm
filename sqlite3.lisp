(in-package :clsql-orm)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)

(defun sqlite3-list-tables (&key (schema *schema*) (owner nil))
  (declare (ignore schema))
  (append
   (mapcar (lambda (name) (list name "TABLE"))
           (clsql-sys:list-tables :owner owner :database clsql-sys:*default-database*))
   (mapcar (lambda (name) (list name "VIEW"))
           (clsql-sys:list-views :owner owner :database clsql-sys:*default-database*))))

(defun sqlite3-list-columns (table &optional (schema *schema*))
  (declare (ignore schema))
  (mapcar (lambda (col &aux paren-idx db-type)
            (destructuring-bind (column-name type is-null? default pk?)
                (cdr col)
              (setf paren-idx (position #\( type)
                    db-type (intern-normalize-for-lisp
                             (if paren-idx (subseq type 0 paren-idx) type)
                             :keyword))
              (column-def column-name db-type
                          nil
                          nil
                          (= is-null? 0)
                          default
                          (list (when (= pk? 1) :primary-key))
                          nil
                          nil)))
          (clsql:query #?"PRAGMA table_info(${table});")))
