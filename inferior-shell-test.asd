;;; -*- Lisp -*-

(defsystem :inferior-shell-test
  ;; adding inferior-shell *second* makes things work on asdf 1&2(!):
  :depends-on (:inferior-shell/test :inferior-shell))
