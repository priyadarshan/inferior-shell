#+xcvb (module (:depends-on ("macros" "process-spec" "run-generic")))

(in-package :inferior-shell)

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type t)
    (with-slots (process predicate rest) result
      (format stream "~A ~A ~A" process predicate rest))))

(defmethod result-or ((r result))
  (not (zerop (sb-ext:process-exit-code (result-process r)))))

(defmethod result-and ((r result))
  (zerop (sb-ext:process-exit-code (result-process r))))

(defmethod generic-run-spec ((spec command-spec) input output error predicate rest resume)
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

(defun make-pipe ()
  (multiple-value-bind (fd1 fd2)
      (sb-unix:unix-pipe)
    (values (sb-sys:make-fd-stream fd1 :buffering :none)
            (sb-sys:make-fd-stream fd2 :buffering :none))))

(defun process-wait (p)
  (sb-ext:process-wait p))

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

(defun sbcl-run (spec &key input output error ignore-error-status)
  (declare (ignore ignore-error-status)) ;; THIS IS A BUG!
  (labels ((collect-threads (r)
             (let ((thread (result-thread r)))
               (when thread
                 (sb-thread:join-thread thread)))))
    (let* ((first-results (generic-run-spec spec input output error nil nil nil))
           (full-results (alexandria:flatten (nconc first-results
                                                    (loop :for r :in first-results
                                                          :nconc (process-result-list r))))))
      (when (keywordp output)
        (let ((collected (mapcar #'collect-threads full-results)))
          (case output
            (:string (apply #'concatenate 'string collected))
            (:string/stripped (apply #'concatenate 'string collected))
            (:lines (apply #'concatenate 'list collected))
            (otherwise collected)))))))
