(in-package lurk.tooling.repl)

(def-suite* api-tooling-suite :in lurk:master-suite)

(defparameter *prompt* "> ")
(defparameter *default-field-order* 1009)
(defparameter *repl-type* :api)

(defstruct repl-state package env evaluator field-order prompt in out)

(defclass repl () ())
(defclass api-repl (repl) ())
(defclass impl-repl (repl) ())

(defgeneric* repl-package ((repl repl))
  (:method ((repl api-repl))
    (find-package :lurk.api-user))
  (:method ((repl impl-repl))
    (find-package :lurk.impl-user)))

(defgeneric* identifier ((repl repl))
  (:method ((repl api-repl)) :api)
  (:method ((repl impl-repl)) :impl))

(defgeneric* empty-env ((repl repl))
  (:method ((repl api-repl))
    (api-impl:empty-env))
  (:method ((repl impl-repl))
    ;; TODO: make this an external symbol, but not in LURK.LANG.
    (lurk.lang::empty-sym-env)))

(defgeneric* make-evaluator ((repl repl) (field-order integer))
  (:method ((repl api-repl) (field-order integer))
    (api-impl:make-evaluator field-order))
  (:method ((repl impl-repl) (field-order integer))
    (lurk.lang::make-evaluator field-order)))

(defgeneric* format-output ((repl repl) (out t) (format-string string) &rest format-args)
  (:method ((repl repl) (out t) (format-string string) &rest format-args)
    (apply #'format out format-string format-args))
  (:method :around ((repl impl-repl) (out t) (format-string string) &rest format-args)
    (declare (ignore format-args))
    (let ((lurk.lang::*print-legibly* t)
          (lurk.lang::*elide-function-envs* t))
      (call-next-method))))

(defgeneric* format-result-values ((repl repl) (state repl-state) (values list))
  (:method ((repl repl) (state repl-state) (values list))
    (let ((evaled (car values)))
      (format-output repl (repl-state-out state) "~%~S" evaled)))

  (:method :after ((repl impl-repl) (state repl-state) (values list))
    (destructuring-bind (evaled new-env iterations)
        values
      (declare (ignore evaled new-env))
      (format-output repl (repl-state-out state) "~%count: ~D" iterations))))

(defun run-repl (&rest args)
  (let* ((type-arg (cadr (assoc "--TYPE" args :test #'string= :key #'string-upcase)))
         (type (if type-arg
                   (intern (string-upcase type-arg) :keyword)
                   *repl-type*)))
    (repl :cli t :type type)))

(defun* make-repl ((type keyword))
  (ecase type
    (:api (make-instance 'api-repl))
    (:impl (make-instance 'impl-repl))))

(defun repl (&key (in *standard-input*) (out *standard-output*) (field-order *default-field-order*) (prompt *prompt*)
               cli (type *repl-type*))
  (let* ((repl (make-repl type))
         (package (repl-package repl))
         (state (make-repl-state :package package
                                 :env (empty-env repl)
                                 :evaluator (make-evaluator repl field-order)
                                 :field-order field-order
                                 :prompt prompt
                                 :in in
                                 :out out)))
    (format out "Lurk REPL [~A].~%:help for help.~%" (identifier repl))
    (loop while state do
      (handler-case (let* ((*package* (repl-state-package state))
                           (input (read-with-prompt state :in in :out out)))
                      (multiple-value-bind (new-state input command-p)
                          (maybe-handle-repl-command state input)
                        (cond
                          (command-p (setq state new-state))
                          (t (let ((result-values (multiple-value-list (eval-expr input state))))
                               (format-result-values repl state result-values))))))
        (error (e) (format (repl-state-out state) "ERROR: ~A" e))
        (condition (c) (format (repl-state-out state) "~A" c))))
    (when cli
      (sb-ext:exit))))

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
     (let* ((pathname (read-form state))
            (to-run (read-from-file state pathname)))
       ;; This is a hack. Pretend this was not a command, and replace the input with expression from file.
       (values state to-run nil)))
    (:clear
     (let ((new-state (copy-repl-state state)))
       (setf (repl-state-env new-state) (api-impl:empty-env))
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
      (format (repl-state-out state) "Read: ~S~%" result)
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