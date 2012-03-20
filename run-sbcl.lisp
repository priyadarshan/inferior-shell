#+xcvb (module (:depends-on ("macros" "process-spec")))

(in-package :inferior-shell)

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
    :type sb-impl::process
    :initform nil :initarg :process :reader result-process)
   ;; thread slurping output
   (thread
    :type sb-thread:thread :initform nil
    :initarg :thread :reader result-thread)
   ;; predicate to determine whether to run the rest of the sequence
   ;; called with the process object, and expected to return t or nil
   (predicate
    :type (or function nil) :initform nil
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

(defun result-or (p)
  (not (zerop (sb-ext:process-exit-code p))))

(defun result-and (p)
  (zerop (sb-ext:process-exit-code p)))

(defmethod sbcl-run-spec ((spec command-spec) input output error predicate rest resume)
  (multiple-value-bind (r-input r-output r-error)
      (process-redirections (command-redirections spec) input output error)
    (labels ((run (out)
               (sb-ext:run-program (car (command-arguments spec))
                                   (cdr (command-arguments spec))
                                   :input r-input :output out :error r-error
                                   :wait nil :search t)))
      (if (keywordp r-output)
          (progn
            (let* ((process (run :stream))
                   (stream (sb-ext:process-output process))
                   (slurp-thread (sb-thread:make-thread (lambda ()
                                                          (unwind-protect
                                                               (slurp-input-stream output stream)
                                                            (when stream (close stream)))))))
              (list (make-instance 'result :process process :thread slurp-thread
                                   :input input :output output :error error
                                   :predicate predicate :rest rest :resume resume))))
          (list (make-instance 'result :process (run r-output)
                               :input input :output r-output :error error
                               :predicate predicate :rest rest :resume resume))))))

 
;; (defun run-collecting (spec input output error collecting)
;;   (let ((collected))
;;     (multiple-value-bind (process slurp)
;;         (sbcl-run-spec spec input output error)
;;       (when (symbolp output)
;;         (if (eq output :lines)
;;             (setf collected (concatenate 'list collecting slurp))
;;             (setf collected (concatenate 'string collecting slurp))))
;;       (values process collected))))

(defmethod sbcl-run-spec ((spec or-spec) input output error predicate rest resume)
  (list (make-instance 'result :predicate #'result-or :input input :output output
                       :error error :rest (sequence-processes spec) :resume resume)))

(defmethod sbcl-run-spec ((spec and-spec) input output error predicate rest resume)
  (list (make-instance 'result :predicate #'result-and :input input :output output
                       :error error :rest (sequence-processes spec) :resume resume)))

(defmethod sbcl-run-spec ((spec progn-spec) input output error predicate rest resume)
  (list (make-instance 'result :predicate nil :input input :output output
                       :error error :rest (sequence-processes spec) :resume resume)))

(defmethod sbcl-run-spec ((spec fork-spec) input output error predicate rest resume)
  (let ((processes (sequence-processes spec)))
    (loop :for p :in processes :do
       (sb-thread:make-thread #'sbcl-run-spec :arguments (list p input output error))))
  nil)

(defun make-pipe ()
  (multiple-value-bind (fd1 fd2) 
      (sb-unix:unix-pipe)
    (values (sb-sys:make-fd-stream fd1 :buffering :none)
            (sb-sys:make-fd-stream fd2 :buffering :none))))

(defmethod sbcl-run-spec ((spec pipe-spec) input output error predicate rest resume)
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
          (push (sbcl-run-spec p input r-input error nil nil nil) results))
         ((null rest)       ;; last process
          (push (sbcl-run-spec p r-output output error nil nil nil) results))
         (t                 ;; middle of the pipeline
          (multiple-value-bind (left right) (make-pipe)
            (push left *streams-to-close*)
            (push right *streams-to-close*)
            (push (sbcl-run-spec p r-output right error nil nil nil) results)
            (setf r-output left)))))
    (close-streams)
    results))

(defmethod process-result ((r result))
  (labels ((eval-pred (pred rest)
             (and (consp rest)
                  (or (null pred)
                      (and (functionp pred) (funcall pred (result-process r)))))))
    (with-slots (process predicate rest input output error resume) r
      (when process
        (sb-ext:process-wait process))
      (if (or (not process)
              (eval-pred predicate rest))
          (let ((next (car rest)))
            (sbcl-run-spec next input output error predicate (cdr rest)
                           (if (typep next 'sequence-spec)
                               r
                               resume)))
          (when resume
            (with-slots (predicate rest resume input output error) resume
              (when (and (consp rest) (consp (cdr rest)))
                (let ((remainder (cdr rest)))
                  (when (eval-pred predicate remainder)
                    (sbcl-run-spec (second rest) input output error
                                   predicate (cddr rest) resume))))))))))

(defun process-result-list (rl)
  (if (listp rl)
      (mapcar #'process-result-list rl)
      (let ((next (process-result rl)))
        (when next
          (alexandria:flatten (list next (process-result-list next)))))))

(defun sbcl-run (spec input output error)
  (labels ((collect-threads (r)          
             (let ((thread (result-thread r)))
               (when thread
                 (sb-thread:join-thread thread)))))
    (let* ((first-results (sbcl-run-spec spec input output error nil nil nil))
           (full-results (alexandria:flatten (nconc first-results
                                                   (loop :for r :in first-results
                                                      :nconc (process-result-list r))))))
      (when (keywordp output)
        (let ((collected (mapcar #'collect-threads full-results)))
          (case output
            (:string (apply #'concatenate 'string collected))
            (:string/stripped (apply #'concatenate 'string collected))
            (:lines (apply #'concatenate 'list collected))
            (otherwise collected )))))))
