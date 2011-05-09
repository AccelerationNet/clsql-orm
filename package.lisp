(defpackage #:clsql-orm
  (:use "CL" "CLSQL" :iterate :arnesi :adwutils)
  (:shadow #:list-tables )
  (:shadowing-import-from :adwutils :join-strings)
  (:export :gen-view-class
	   :gen-view-classes
	   :gen-view-classes-for-database
	   :list-foreign-constraints
	   :not-null-p
	   :unique-p
	   :primary-key-p
	   :clsql-type-for-pg-type
	   :user-columns)
  (:documentation "This package provides methods to introspect a postgres database
Providing features such as generating a CLSQL class based on a table name"))
