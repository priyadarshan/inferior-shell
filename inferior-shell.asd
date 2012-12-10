;;; -*- Lisp -*-

(defsystem :inferior-shell
  :defsystem-depends-on (:asdf-condition-control)
  :depends-on (:asdf :xcvb-utils :fare-quasiquote-extras :fare-mop
               #+sbcl :sb-posix)
  :description "spawn local or remote processes and shell pipes"
  :components
  ((:file "pkgdcl")
   (:file "process-spec" :depends-on ("pkgdcl"))
   (:file "utilities" :depends-on ("pkgdcl"))
   (:file "macros" :depends-on ("pkgdcl"))
   (:file "host" :depends-on ("pkgdcl"))
   (:file "run" :depends-on ("process-spec" "macros"))
   (:file "run-generic" :depends-on ("process-spec" "macros"))
   #+ (and sbcl sb-thread unix)
   (:file "run-sbcl" :depends-on ("process-spec" "macros" "run-generic"))))

(defmethod perform ((op test-op) (system (eql (find-system :inferior-shell))))
  (asdf:load-system :inferior-shell-test)
  (funcall (asdf::find-symbol* :test-suite :inferior-shell-test)))
