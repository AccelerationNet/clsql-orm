(in-package :clsql-orm)
(cl-interpol:enable-interpol-syntax)

(defmethod list-tables ((db-vendor (eql :sqlite3)) &optional (schema *schema*))
  (append (mapcar (lambda (name)
                    (list name "TABLE"))
                  (clsql-sqlite3::database-list-tables clsql-sys:*default-database*))
          (mapcar (lambda (name)
                    (list name "VIEW"))
                  (clsql-sqlite3::database-list-views clsql-sys:*default-database*))))

(defmethod list-columns ((db-vendor (eql :sqlite3)) table &optional (schema *schema*))
  (mapcar (lambda (col)
            (destructuring-bind (column-name type is-null? default pk?)
                (cdr col)
              (column-def column-name
                          (clsql-type-for-db-type (intern-normalize-for-lisp
                                                   (aif (position #\( type)
                                                        (subseq type 0 it)
                                                        type)
                                                   :keyword)
                                                  nil)
                          nil
                          nil
                          (= is-null? 0)
                          default
                          (when (= pk? 1) :primary-key)
                          nil
                          nil)))
          (clsql:query #?"PRAGMA table_info(${table});")))