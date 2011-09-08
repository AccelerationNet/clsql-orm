;-*- mode: lisp -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clsql-orm.system)
    (defpackage :clsql-orm.system
      (:use :common-lisp :asdf))))

(in-package :clsql-orm.system)

; For those who like that sort of thing: an ASDF package
(defsystem clsql-orm
  :depends-on (:clsql :cl-ppcre :cl-interpol :cl-inflector :symbol-munger)
  :version "0.2"
  :serial T
  :components ((:file "package")
               (:file "main")
               (:file "sqlite3")))
