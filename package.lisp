(defpackage "CLSQL-PG-INTROSPECT"
  (:use "CL" "CLSQL")
  (:export :gen-view-class
	   :gen-view-classes-for-database
	   :list-foreign-constraints
	   :not-null-p
	   :unique-p
	   :primary-key-p
	   :clsql-type-for-pg-type
	   :user-columns)
  (:documentation "This package provides methods to introspect a postgres database
Providing features such as generating a CLSQL class based on a table name"))
