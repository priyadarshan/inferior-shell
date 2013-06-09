;;; -*- Lisp -*-

(defsystem :inferior-shell
  :defsystem-depends-on (:asdf #-asdf3 :asdf-driver)
  :depends-on (#-asdf3 :asdf-driver #+sbcl :sb-posix
               :alexandria :optima
               :fare-utils :fare-quasiquote-extras :fare-mop)
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
   #+asdf3
   (:file "run-sbcl" :depends-on ("process-spec" "macros" "run-generic")
          :if-feature (:and :sbcl :sb-thread :unix)))
  :in-order-to ((test-op (load-op inferior-shell/test)))
  :perform (test-op :after (o s)
              (symbol-call :inferior-shell-test :test-suite)))

(defsystem :inferior-shell/test
  :depends-on (:inferior-shell :hu.dwim.stefil)
  :description "testing inferior-shell"
  :components ((:file "test")))
