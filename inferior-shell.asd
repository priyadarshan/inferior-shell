;;; -*- Lisp -*-
(in-package :asdf)

(defsystem :inferior-shell
  :defsystem-depends-on (:asdf #-asdf3 :asdf-driver)
  :depends-on (#-asdf3 :asdf-driver :fare-utils :alexandria :fare-quasiquote-extras :fare-mop :optima
               #+sbcl :sb-posix)
  :description "spawn local or remote processes and shell pipes"
  :around-compile "asdf-driver:call-with-safe-io-syntax"
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
  (load-system :inferior-shell-test)
  (symbol-call :inferior-shell-test :test-suite))
