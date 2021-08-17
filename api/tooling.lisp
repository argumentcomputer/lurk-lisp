(in-package rhododendron.api.tooling)

(def-suite* api-tooling-suite :in rhododendron:master-suite)

(defparameter *prompt* "> ")
(defparameter *repl-package* (find-package :rhododendron.user))
(defparameter *default-field-order* 1009)

(defstruct repl-state package env evaluator p prompt in out)

(defun run-repl (&rest args)
  (declare (ignore args))
  (repl))

(defun repl (&key (in *standard-input*) (out *standard-output*) (package *repl-package*) (p *default-field-order*) (prompt *prompt*))
  (rhododendron.macros:display package)
  (let ((state (make-repl-state :package package
                                :env (impl:empty-env)
                                :evaluator (impl:make-evaluator p)
                                :p p
                                :prompt prompt
                                :in in
                                :out out)))
    (format out "Rhododendron REPL.~%:help for help.~%")
    (loop while state do
      (handler-case (let* ((*package* (repl-state-package state))
                           (input (read-with-prompt state :in in :out out)))
                      (multiple-value-bind (new-state input command-p)
                          (maybe-handle-repl-command state input)
                        (cond
                          (command-p (setq state new-state))
                          (t (let ((evaled (eval-expr input state)))
                               (format out "~%~S" evaled))))))
        (error (e) (format (repl-state-out state) "ERROR: ~A" e))
        (condition (c) (format (repl-state-out state) "~A" c))
        ))
    (sb-ext:exit)))

(defun eval-expr (expr state)
  (funcall (repl-state-evaluator state) expr (repl-state-env state)))

(defun read-with-prompt (repl-state &key (in *standard-input*) (out *standard-output*))
  (let ((*package* (repl-state-package repl-state)))
    (format out "~%~A" (repl-state-prompt repl-state))
    (force-output out)
    (read in)))

(defun maybe-handle-repl-command (state input)
  (case input
    (:help
     (print-help (repl-state-out state))
     (values state input t))
    (:quit
     (values nil input t))
    (:echo
     (let ((to-echo (read-form state)))
       (format (repl-state-out state) "ECHO: ~S" to-echo)
       (values state input t)))
    (:load
     (let* ((pathname (read-form state))
            (new-state (load-lib state pathname)))
       (values new-state input t)))
    (:run
     (let ((pathname (read-form state)))
       (let ((to-run (read-from-file state pathname)))
         (format (repl-state-out state) "Run: ~S~%" to-run)
         ;; This is a hack. Pretend this was not a command, and replace the input with expression from file.
         (values state to-run nil))))
    (:clear
     (let ((new-state (copy-repl-state state)))
       (setf (repl-state-env new-state) (impl:empty-env))
       (values new-state input t)))
    (t (values state input nil))))

(defun read-form (state)
  (let* ((*package* (repl-state-package state)))
    (read (repl-state-in state))))

(defun load-lib (state pathname)
  (let* ((to-load (read-from-file state pathname))
         (new-env (eval-expr to-load state))
         (new-state (copy-repl-state state)))
    (setf (repl-state-env new-state) new-env)
    new-state))

(defun read-from-file (state pathname)
  (with-open-file (in pathname :direction :input)
    (format (repl-state-out state) "Reading from ~A.~%" pathname)
    (let* ((*package* (repl-state-package state))
           (result (read in)))
      result)))

(defparameter *commands*
  '((:help () "Print this text.")
    (:quit () ":Quit REPL.")
    (:echo (form) " Read one form and echo it.")
    (:load (path) "Load a library.")
    (:clear () "Clear loaded libraries.")
    (:run (path) "Evaluate expression from file.")))

(defun print-help (out)
  (loop for (command args help-text) in *commands*
        do (progn
             (format out "~&~S" command)
             (loop for arg in args do
               (format out " <~A>" arg))
             (format out " => ~A~%" help-text))))
