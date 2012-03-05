;;; -*- Lisp -*-

#+sbcl (require :sb-posix)

(defsystem :inferior-shell
  :depends-on (:asdf :xcvb-driver :fare-utils :fare-matcher :fare-quasiquote-readtable)
  :description "spawn local or remote processes and shell pipes"
  :components
  ((:file "pkgdcl")
   (:file "process-spec" :depends-on ("pkgdcl"))
   (:file "utilities" :depends-on ("pkgdcl"))
   (:file "macros" :depends-on ("pkgdcl"))
   (:file "host" :depends-on ("pkgdcl"))
   (:file "run" :depends-on ("process-spec" "macros"))))
