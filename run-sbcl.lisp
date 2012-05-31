#+xcvb (module (:depends-on ("macros" "process-spec" "run-generic")))

(in-package :inferior-shell)

(defgeneric result-or (result))

(defgeneric result-and (result))

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

(defun sbcl-run (spec &key input output error ignore-error-status)
  (declare (ignore ignore-error-status)) ;; THIS IS A BUG!
  (labels ((collect-threads (r)
             (let ((thread (result-thread r)))
               (when thread
                 (sb-thread:join-thread thread)))))
    (let* ((first-results (generic-run-spec spec input output error nil nil nil))
           (full-results (alexandria:flatten (nconc first-results
                                                    (mapcan 'process-result-list first-results)))))
      (when (keywordp output)
        (let ((collected (mapcar #'collect-threads full-results)))
          (case output
            (:string (apply #'concatenate 'string collected))
            (:string/stripped (strcat collected))
            (:lines (apply #'concatenate 'list collected))
            (otherwise collected)))))))
