;;; -*- Lisp -*-

(defsystem :inferior-shell-test
  :depends-on (:inferior-shell :hu.dwim.stefil)
  :description "testing inferior-shell"
  :components
  ((:file "test")))
