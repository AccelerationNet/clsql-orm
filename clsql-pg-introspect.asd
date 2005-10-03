;-*- mode: lisp -*-

; For those who like that sort of thing: an ASDF package
(defsystem clsql-pg-introspect
   :depends-on (:clsql-postgresql)
   :version "0.1"
   :components
   ((:file "package")
    (:file "main" :depends-on ("package"))))
