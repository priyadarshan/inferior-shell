#+xcvb (module ())

(in-package :cl)

(defpackage :inferior-shell
  (:use :cl :fare-utils :fare-matcher :xcvb-driver :named-readtables :fare-mop)
  (:export
   #:run #:run/s #:run/ss #:run/lines
   #:process-spec #:command-spec #:pipe-spec
   #:or-spec #:and-spec #:progn-spec #:fork-spec
   #:print-process-spec #:parse-process-spec
   #:command-arguments #:command-redirections
   #:redirection #:file-redirection #:fd-redirection #:close-redirection
   #:! #:- #:< #:> #:<> #:>! #:>> #:>>! #:<& #:>& #:>&! #:>>&!
   #:>& #:>> #:>>& #:pipe #:or #:and #:progn #:fork
   #:*force-shell*))
