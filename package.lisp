(defpackage #:clsql-orm
  (:use :cl :clsql :iterate)
  (:shadow #:list-tables )
  (:export
   :gen-view-class
   :list-tables
   :gen-view-classes
   :gen-view-classes-for-database
   :list-foreign-constraints
   :not-null-p
   :unique-p
   :primary-key-p
   :clsql-type-for-pg-type
   :list-columns
   :column-def
   :column
   :db-type
   :spec-type
   :is-null
   :scale
   :col-length
   :col=
   :col-spec-eql
   :column-name=
   :column-diff)
  (:documentation "This package provides methods to introspect a database
Providing features such as generating a CLSQL class based on a table name"))
