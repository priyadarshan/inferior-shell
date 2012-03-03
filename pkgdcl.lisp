#+xcvb (module ())

(in-package :cl)

(defpackage :inferior-shell
  (:use :cl :fare-utils :fare-memoization :fare-matcher :fare-mop 
        :xcvb-driver :λ-reader :named-readtables)
  (:export
   #:run #:run/s #:run/ss #:run/lines
   #:process-spec #:command-spec #:pipe-spec
   #:print-process-spec #:parse-process-spec
   #:command-arguments #:command-redirections
   #:redirection #:file-redirection #:fd-redirection #:close-redirection
           #:! #:- #:< #:> #:<> #:>! #:>> #:>>! #:<& #:>& #:>&! #:>>&!
           #:>& #:>> #:>>& #:pipe))
