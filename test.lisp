#+xcvb (module (:depends-on ("/inferior-shell" :asdf "hu.dwim.stefil")))

(in-package :cl)

(defpackage :inferior-shell-test
  (:use :cl :inferior-shell :hu.dwim.stefil))

(in-package :inferior-shell-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing inferior-shell"))

(defmacro w-sh (&body body)
  `(let ((*force-shell* t))
     ,@body))

(defmacro wo-sh (&body body)
  `(let ((*force-shell* t))
     ,@body))

(deftest test-inferior-shell ()
  (is (equal (w-sh (run/ss "echo 1 2 3")) "1 2 3"))
  (is (equal (wo-sh (run/ss "echo 1 2 3")) "1 2 3"))
  (is (equal (w-sh (run/ss `(pipe (echo (+ hel "lo,") world)
                                  (tr "hw" "HW") (sed -e "s/$/!/"))))
             "Hello, World!"))
  (is (equal (wo-sh (run/ss `(pipe (echo (+ hel "lo,") world)
                                   (tr "hw" "HW") (sed -e "s/$/!/"))))
             "Hello, World!")))
