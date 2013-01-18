#+xcvb (module ())

(in-package :cl)

(asdf/package:define-package :inferior-shell
  (:mix :fare-utils :alexandria :asdf/driver)
  (:use :cl :optima :named-readtables :fare-mop)
  (:export
   #:run #:run/s #:run/ss #:run/lines
   #:simple-command-line-token #:token-string
   #:process-spec #:command-spec #:pipe-spec
   #:or-spec #:and-spec #:progn-spec #:fork-spec
   #:print-process-spec #:parse-process-spec
   #:command-arguments #:command-redirections
   #:redirection #:file-redirection #:fd-redirection #:close-redirection
   #:! #:- #:< #:> #:<> #:>! #:>> #:>>! #:<& #:>& #:>&! #:>>&!
   #:>& #:>> #:>>& #:pipe #:or #:and #:progn #:fork
   #:zglobcmd #:zglob
   #:*backend*))
