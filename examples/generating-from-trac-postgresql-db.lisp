(defpackage :clsql-orm-pg-example
    (:use :cl :cl-user))

(in-package :clsql-orm-pg-example)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass pg-db-class (CLSQL-SYS::STANDARD-DB-CLASS)
    ())
  (defclass pg-db-obj (CLSQL-SYS::STANDARD-DB-OBJECT)
    ()
    (:METACLASS pg-db-class)))

(defmethod generate-view-classes ()  
  (clsql-sys:with-database
      (clsql-sys:*default-database*
       `("host" "database" "user" "password")
       :pool nil
       :database-type :postgresql-socket
       :if-exists :warn-new)
    ;; generate classes from the trac database into the current package
    ;;   These symbols will not automatically be 
    (clsql-orm:gen-view-classes
     ;; in the public schema
     :schema "public" 
     ;; for each of these table names
     :classes '(ticket ticket_change recent_ticket_changes recent_wiki_changes)
     :metaclass 'pg-db-class
     :inherits-from '(pg-db-obj))
    
    ))
