(defpackage :clsql-orm-mssql-example (:use :cl :cl-user))
(in-package :clsql-orm-mssql-example)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass mssql-db-class (CLSQL-SYS::STANDARD-DB-CLASS)
    ())
  (defclass mssql-db-obj (CLSQL-SYS::STANDARD-DB-OBJECT)
    ()
    (:METACLASS mssql-db-class)))

(defmethod generate-view-classes ()  
  (clsql-sys:with-database
      (clsql-sys:*default-database*
       `("DSN-NAME" "USER-NAME" "PASSWORD")
       :pool T
       :make-default nil
       :database-type :odbc
       :if-exists :warn-new)
    
    (clsql-orm:gen-view-classes
     ;; Generate all these in a new
     ;; package named :clsql-orm-mssql-example-objects
     ;;  :nicknames :adwolf-db 
     :package :clsql-orm-mssql-example-objects
     :nicknames :mssql-objs
     
     ;; set each class inheritance chain
     :inherits-from '(clsql:mssql-db-object)
     ;; set each classes metaclass
     :metaclass 'mssql-db-class

     ;; should we automatically export the symbols we create from the
     ;; package above
     :export-symbols T

     ;; SQLServer Owner
     :schema "dbo"
     
     ;; Should we try to singularize the class names we create from the table
     ;;  eg: Services -> adwolf-db:service when T
     ;;   &  Services -> adwolf-db:services when nil
     :singularize nil)))