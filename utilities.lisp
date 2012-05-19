#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :inferior-shell)

;;--- TODO: move these to FARE-UTILS and/or XCVB-DRIVER ?

(defun make-directory (dir &optional (mode #o755))
  #+sbcl (sb-posix:mkdir dir mode)
  #-sbcl (NIY 'make-directory))

(defun setenv (var val &optional (overwritep t))
  #+sbcl (sb-posix:setenv var val (if overwritep 1 0))
  #-sbcl (NIY 'setenv))

(defmacro pipe (values &rest transformers)
  (if (null transformers)
      values
      `(pipe (multiple-value-call ,(car transformers) ,values)
             ,@(cdr transformers))))

(defun split-lines (x)
  (asdf:split-string x :separator #(#\newline)))

(defun split-lines* (x)
  (remove-if #'emptyp (split-lines x)))

(defun select-from-hash (keys hash)
  (loop :for key :in keys
    :collect (gethash key hash)))

(defun do-stream-lines (fun stream)
  (loop :for line = (read-line stream nil) :while line
    :do (funcall fun line)))

(defun do-string-lines (fun string)
  (with-input-from-string (stream string)
    (do-stream-lines fun stream)))

(defun println (x)
  (princ x) (terpri) (values))

(defun writeln (x &rest keys)
  (apply 'write x keys) (terpri) (values))

(defun stripln (x)
  (check-type x string)
  (let* ((len (length x))
         (endlfp (equal (last-char x) #\linefeed))
         (endcrlfp (and endlfp (<= 2 len) (eql (char x (- len 2)) #\return)))
         (endcrp (equal (last-char x) #\return)))
    (cond
      ((or endlfp endcrp) (subseq x 0 (- len 1)))
      (endcrlfp (subseq x 0 (- len 2)))
      (t x))))

(defun add-days (year month date days)
  (multiple-value-bind (sec min hr d m y dlsp tz)
      (decode-universal-time
       (+ (encode-universal-time 0 0 0 date month year 0) (* 60 60 24 days)) 0)
    (declare (ignore sec min hr dlsp tz))
    (values y m d)))

(defun previous-day (year month date)
  (add-days year month date -1))

(defun next-day (year month date)
  (add-days year month date 1))

(defun zglobcmd (&rest patterns)
  `(zsh --nullglob -fc ("print -l " ,(join-strings patterns :separator " "))))

(defun zglob (patterns &key host)
  (run/lines `((> 2 "/dev/null") ,@(apply 'zglobcmd (alexandria:ensure-list patterns)))
             :host host))
