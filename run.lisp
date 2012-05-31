#+xcvb (module (:depends-on ("macros" "process-spec")))

(in-package :inferior-shell)

(defvar *backend* :auto)

;;; TODO: instead support a magic :interactive directly in driver.lisp's run-program/
;;; and/or add support for arbitrary input (and error output?) to said run-program/
(defun run-program/interactively (command &key ignore-error-status)
  #-(or clozure sbcl) (NIY 'run-program/interactively command ignore-error-status)
  #+(or clozure sbcl)
  (let* ((process
          (#+clozure ccl:run-program #+sbcl sb-ext:run-program
           (car command) (cdr command)
           :input t :output t :error t :wait t
           #+sbcl :search #+sbcl t))
         (return-code
          #+clozure (nth-value 1 (ccl:external-process-status process))
          #+sbcl (sb-ext:process-exit-code process)))
    (unless (or ignore-error-status (zerop return-code))
      (cerror "ignore error code~*~*"
              "Process ~S exited with error code ~D"
              command return-code))
    (zerop return-code)))

(defun on-host-spec (host spec)
  (if (current-host-name-p host)
      spec
      `(ssh ,host ,(print-process-spec spec))))

(defun run-spec (spec &key ignore-error-status output)
  (let ((command
         (if (and (typep spec 'command-spec)
                  (null (command-redirections spec)))
             (command-arguments spec)
             (print-process-spec spec))))
    (if (eq output t)
        (run-program/ command :ignore-error-status ignore-error-status)
        (run-program/ command
                      :ignore-error-status ignore-error-status
                      :output output))))

(defun run-process-spec (spec &rest keys &key ignore-error-status output host backend)
  (etypecase host
    (null
     (etypecase spec
       (string
        (if (eq output t)
            (run-program/ spec :ignore-error-status ignore-error-status)
            (apply 'run-program/ spec keys)))
       (cons
        (apply 'run-process-spec (parse-process-spec spec) keys))
       (process-spec
        (ecase (or backend *backend*)
          #+(and sbcl sb-thread unix)
          ((:sbcl)
           (sbcl-run
            spec :input (eq :output :interactively) :output output :error t
            :ignore-error-status ignore-error-status))
          ((:auto)
           (run-spec spec :ignore-error-status ignore-error-status :output output))))))
    (string
     (apply 'run-process-spec (on-host-spec host spec) :host nil keys))
    (function
     (apply 'run-process-spec (funcall host spec) :host nil keys))))

(defun run (cmd &key time (output t) show host (on-error (list "Command ~S failed~@[ on ~A~]" cmd host)))
  (labels ((process-time ()
             (if time (time (process-command)) (process-command)))
           (process-command ()
             (handler-case
                 (run-process-spec
                  cmd
                  :ignore-error-status nil :output output :host host)
               (subprocess-error () (error-behaviour on-error)))))
    (when show
      (format *trace-output* "; ~A~%" (print-process-spec cmd)))
    (process-time)))

(defun run/s (cmd &rest keys &key on-error time show host)
  "run command CMD, return its standard output results as a string."
  (declare (ignore on-error time show host))
  (apply 'run cmd :output 'string keys))

(defun run/ss (cmd &rest keys &key on-error time show host)
  "Like run/s, but strips the line ending off the result string;
very much like `cmd` or $(cmd) at the shell"
  (declare (ignore on-error time show host))
  (apply 'run cmd :output :string/stripped keys))

(defun slurp-stream-string/stripped (input-stream)
  (stripln (slurp-stream-string input-stream)))

(defmethod slurp-input-stream ((x (eql :string/stripped)) input-stream
                               &key &allow-other-keys)
  (slurp-stream-string/stripped input-stream))

(defun run/lines (cmd &rest keys &key on-error time show host)
  "Like run/s, but return a list of lines rather than one string"
  (declare (ignore on-error time show host))
  (apply 'run cmd :output :lines keys))
