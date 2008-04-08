;-*- mode: lisp -*-

; For those who like that sort of thing: an ASDF package
(defsystem clsql-pg-introspect
   :depends-on (:clsql :cl-ppcre :adwcodebase)
   :version "0.2"
   :components
   ((:file "package")
    (:file "main" :depends-on ("package"))))
