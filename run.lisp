#+xcvb (module (:depends-on ("macros" "process-spec")))

(in-package :inferior-shell)

(defvar *backend* :auto)

(defun on-host-spec (host spec)
  (if (current-host-name-p host)
      spec
      `(ssh ,host ,(print-process-spec spec))))

(deftype direct-command-spec ()
  '(and command-spec (satisfies direct-command-spec-p)))

(defun direct-command-spec-p (spec)
  (and (typep spec 'command-spec)
       (null (command-redirections spec))))

(defun run-spec (spec &rest keys
                 &key ignore-error-status output element-type external-format &allow-other-keys)
  (declare (ignore ignore-error-status output element-type external-format))
  (let* ((command
          (if (consp spec)
            (parse-process-spec spec)
            spec))
         (command
          (etypecase command
            (direct-command-spec
             (command-arguments spec))
            (process-spec
             (print-process-spec spec))
            (string
             spec))))
    (apply 'run-program command :output (case output ((t) nil) (otherwise output)) keys)))

(defun run-process-spec (spec &rest keys &key ignore-error-status output host backend)
  (etypecase host
    (null
     (etypecase spec
       (string
        (run-spec spec :ignore-error-status ignore-error-status :output output))
       (cons
        (apply 'run-process-spec (parse-process-spec spec) keys))
       (process-spec
        (ecase (or backend *backend*)
          #+(and sbcl sb-thread unix)
          ((:sbcl)
	   (let ((interactive (eq :output :interactive)))
	     (sbcl-run
	      spec :input interactive :output (or interactive output) :error t
		   :ignore-error-status ignore-error-status)))
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
               (subprocess-error () (error-behavior on-error)))))
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
