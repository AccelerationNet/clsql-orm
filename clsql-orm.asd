;-*- mode: lisp -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clsql-orm.system)
    (defpackage :clsql-orm.system
      (:use :common-lisp :asdf))))

(in-package :clsql-orm.system)

(defsystem clsql-orm
  :depends-on (:clsql :cl-ppcre :cl-interpol :cl-inflector :symbol-munger :iterate)
  :version "0.2.1"
  :serial T
  :components ((:file "package")
               (:file "sqlite3")
               (:file "mysql")
               (:file "main")))
