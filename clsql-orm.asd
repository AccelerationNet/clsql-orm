;-*- mode: lisp -*-

; For those who like that sort of thing: an ASDF package
(defsystem clsql-orm
   :depends-on (:clsql :cl-ppcre :cl-interpol :cl-inflector :symbol-munger)
   :version "0.2"
   :serial t
   :components ((:file "package")
                (:file "main")
                (:file "clsql3")))
