#+xcvb (module (:depends-on ("macros" "process-spec")))

(in-package :inferior-shell)

(defgeneric process-redirection (redirection input output error))
(defgeneric generic-run-spec (spec input output error predicate rest resume))
(defgeneric process-result (result))

;; list of streams to close when finished
(defvar *streams-to-close* nil)

(defun close-streams ()
  (mapcar #'close *streams-to-close*)
  (setq *streams-to-close* nil))

;; interface to process a list of redirections and return redirected
;; input, output and error streams
(defun process-redirections (redirections input output error)
  (let ((r-input input)
        (r-output output)
        (r-error error))
    (loop :for r :in redirections
       :do (multiple-value-setq (r-input r-output r-error)
             (process-redirection r r-input r-output r-error)))
    (values r-input r-output r-error)))

(defmethod process-redirection ((r file-redirection) input output error)
  (with-slots (fd pathname flags) r
    (let ((stream (apply #'open pathname :direction (car flags) (cdr flags))))
      (push stream *streams-to-close*)
      (ecase fd
        (0 (values stream output error))
        (1 (values input stream error))
        (2 (values input output stream))))))

(defmethod process-redirection ((r fd-redirection) input output error)
  (with-slots (old-fd new-fd) r
    (ecase new-fd
      (1 (ecase old-fd
           (0 (values input input error))
           (2 (values input error :output))))
      (2 (ecase old-fd
           (0 (values input output input))
           (1 (values input output :output)))))))

(defmethod process-redirection ((r close-redirection) input output error)
  (with-slots (old-fd) r
    (case old-fd
      (0 (values nil output error))
      (1 (values input nil error))
      (2 (values input output error))
      (otherwise (error "Can't close arbitrary fd: ~A" old-fd)))))

(defclass result ()
  ((process
;    :type sb-impl::process
    :initform nil :initarg :process :reader result-process)
   ;; thread slurping output
   (thread
;    :type sb-thread:thread
    :initform nil
    :initarg :thread :reader result-thread)
   ;; predicate to determine whether to run the rest of the sequence
   ;; called with the process object, and expected to return t or nil
   (predicate
    :type (or function null) :initform nil
    :initarg :predicate :accessor result-predicate)
   (rest
    :type list :initform nil
    :initarg :rest :accessor result-rest)
   (input
    :initform nil :initarg :input :accessor result-input)
   (output
    :initform nil :initarg :output :accessor result-output)
   (error
    :initform nil :initarg :error :accessor result-error)
   (resume
    :initform nil :initarg :resume :accessor result-resume)))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type t)
    (with-slots (process predicate rest) result
      (format stream "~A ~A ~A" process predicate rest))))

(defmethod generic-run-spec ((spec or-spec) input output error predicate rest resume)
  (declare (ignorable predicate rest))
  (list (make-instance 'result :predicate #'result-or :input input :output output
                       :error error :rest (sequence-processes spec) :resume resume)))

(defmethod generic-run-spec ((spec and-spec) input output error predicate rest resume)
  (declare (ignorable predicate rest))
  (list (make-instance 'result :predicate #'result-and :input input :output output
                       :error error :rest (sequence-processes spec) :resume resume)))

(defmethod generic-run-spec ((spec progn-spec) input output error predicate rest resume)
  (declare (ignorable predicate rest))
  (list (make-instance 'result :predicate nil :input input :output output
                       :error error :rest (sequence-processes spec) :resume resume)))

(defmethod generic-run-spec ((spec fork-spec) input output error predicate rest resume)
  (declare (ignorable predicate rest resume))
  (loop :for p :in (sequence-processes spec) :do
     (generic-run-spec p input output error nil nil nil))
  nil)

(defmethod generic-run-spec ((spec pipe-spec) input output error predicate rest resume)
  (declare (ignorable predicate rest resume))
  (let ((processes (sequence-processes spec))
        (r-input)
        (r-output)
        (first t)
        (results)
        (*streams-to-close* nil))
    (loop :for (p . rest) :on processes :do
       (cond
         ((and (listp rest) first)  ;; first process
          (setf first nil)
          (multiple-value-setq (r-output r-input) (make-pipe))
          (push r-input *streams-to-close*)
          (push r-output *streams-to-close*)
          (push (generic-run-spec p input r-input error nil nil nil) results))
         ((null rest)       ;; last process
          (push (generic-run-spec p r-output output error nil nil nil) results))
         (t                 ;; middle of the pipeline
          (multiple-value-bind (left right) (make-pipe)
            (push left *streams-to-close*)
            (push right *streams-to-close*)
            (push (generic-run-spec p r-output right error nil nil nil) results)
            (setf r-output left)))))
    (close-streams)
    results))

(defmethod process-result ((r result))
  (labels ((eval-pred (pred rest)
             (and (consp rest)
                  (or (null pred)
                      (and (functionp pred) (funcall pred r))))))
    (with-slots (process predicate rest input output error resume) r
      (when process
        (process-wait process))
      (if (or (not process)
              (eval-pred predicate rest))
          (let ((next (car rest)))
            (generic-run-spec next input output error predicate (cdr rest)
                              (if (typep next 'sequence-spec)
                                  r
                                  resume)))
          (when resume
            (with-slots (predicate rest resume input output error) resume
              (when (and (consp rest) (consp (cdr rest)))
                (let ((remainder (cdr rest)))
                  (when (eval-pred predicate remainder)
                    (generic-run-spec (second rest) input output error
                                      predicate (cddr rest) resume))))))))))

(defun process-result-list (rl)
  (if (listp rl)
      (mapcar #'process-result-list rl)
      (let ((next (process-result rl)))
        (when next
          (alexandria:flatten (list next (process-result-list next)))))))

