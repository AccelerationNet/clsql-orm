# CLSQL-ORM #
Use this package to help with introspective operations against
databases that support information_schema . It supports generating 
view-class definitions from the database schema.

This was originally clsql-postgres-introspect, though this is a bit far removed from that.

I have used this to successfully generate view-classes for PostgreSQL,
MSSQL (through freetds and unixodbc), and mysql.

This project does not attempt to be a persistence layer. It also makes quite a few assumptions
about what should be generated and with what types that may not match your desires or existing 
notions.  As such this code might only be useful as a jumping off point for creating your own
custom clsql ORM.

## Examples ##

**Please see the examples directory**

```
(with-a-database (*application*) ;; a clsql-helper macro that sets up clsql:*default-database* 
  (clsql-orm:gen-view-classes
   :inherits-from '(pg-db-obj) ;; a class I have that I want all my pg-db-objects to inherit from
   :classes 
   ;; The tables I want to turn into classes
   '(users user_districts_and_counties titles salaries roles fiscal-years expenses budgets counties
     districts counties-with-districts reports data-entry-finalizations
     specialties bad-state-salary-input races)))
```

```
(clsql-orm:gen-view-classes
   :inherits-from '(pg-db-obj) ;; a class I have that I want all my pg-db-objects to inherit from
   :classes 
   ;; The tables I want to turn into classes
   '(users))

;;; Results in the following class definition being evaled:
(def-view-class user (pg-db-obj)
  ((date-entered :column date_entered :accessor date-entered
		 :db-constraints nil :type wall-time :initarg
		 date-entered)
   (deleted :column deleted :accessor deleted :db-constraints nil
	    :type boolean :initarg deleted)
   (email :column email :accessor email :db-constraints (not-null)
	  :type (varchar 128) :initarg email)
   (enabled :column enabled :accessor enabled :db-constraints
	    (not-null) :initform t :type boolean :initarg enabled)
   (first-name :column first_name :accessor first-name
	       :db-constraints nil :initform nil :type varchar
	       :initarg first-name)
   (id :column id :accessor id :db-kind key :db-constraints
       (not-null) :type integer :initarg id)
   (last-name :column last_name :accessor last-name :db-constraints
	      nil :initform nil :type varchar :initarg last-name)
   (password :column password :accessor password :db-constraints
	     (not-null) :type (varchar 32) :initarg password)
   (role-id :column role_id :accessor role-id :db-constraints
	    (not-null) :type integer :initarg role-id)
   (role-join :accessor role-join :db-kind join :db-info
	      (join-class role :home-key role-id :foreign-key id
			  :set nil))
   (salt :column salt :accessor salt :db-constraints (not-null) :type
	 (varchar 4) :initarg salt))
  (base-table users))

```
## Authors
### Authors of this Branch ###

* [Acceleration.net] (http://www.acceleration.net/)
 * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
 * [Nathan Bird](http://the.unwashedmeme.com/blog)
 * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

### Author of MySQL Bug Fixes ###

Scott Brown <brown131@yahoo.com>

### Original Author information ###

Alan Shields <Alan-Shields@omrf.ouhsc.edu>

This work was made possible by the Centola Lab of the Oklahoma Medical Research Foundation

License information is in the file LICENSE (LLGPL)
