(in-package lurk.tooling.repl)

(def-suite* api-tooling-suite :in lurk:master-suite)

(defparameter *prompt* "> ")
(defparameter *default-field-order* lurk.api.impl:*default-p*)
(defparameter *repl-type* :api)

(defstruct repl-state package env evaluator field-order prompt in out readtable)

(defclass repl () ())
(defclass api-repl (repl) ())
(defclass impl-repl (repl) ())

(defun bang-reader (stream char)
  (declare (ignore char))
  (list '! (read stream t nil t)))

(defun make-repl-readtable ()
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\! #'bang-reader)
    *readtable*))

(defgeneric* repl-package ((repl repl))
  (:method ((repl api-repl))
    (find-package :lurk.api-user))
  (:method ((repl impl-repl))
    (find-package :lurk.impl-user)))

(defgeneric* identifier ((repl repl))
  (:method ((repl api-repl)) :api)
  (:method ((repl impl-repl)) :impl))

(defgeneric* repl-nil ((repl repl))
  (:method ((repl api-repl)) nil)
  (:method ((repl impl-repl)) (lurk.lang::sym nil)))


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
      (format-output repl (repl-state-out state) "~S~%" evaled)))

  (:method :after ((repl impl-repl) (state repl-state) (values list))
    (destructuring-bind (evaled new-env iterations)
        values
      (declare (ignore evaled new-env))
      (format-output repl (repl-state-out state) "count: ~D~%" iterations))))

(defun keywordize (string)
  (intern (string-upcase string) :keyword))

(opts:define-opts
    (:name :help
           :description "print this help text"
           :short #\h
           :long "help")
    (:name :nb
           :description "here we want a number argument"
           :short #\n
           :long "nb"
           :arg-parser #'parse-integer) ;; <- takes an argument
    (:name :type
           :description "REPL type"
           :short #\t
           :long "type"
           :arg-parser #'keywordize))

(defun run-repl (&rest args)
  (declare (ignore args))
  (multiple-value-bind (opts free-args)
      (opts:get-opts)
    (declare (ignore free-args))
    (let* ((type-arg (getf opts :type))
           (type (if type-arg
                     (intern (string-upcase type-arg) :keyword)
                     *repl-type*)))
      (repl :cli t :type type))))

(defun* make-repl ((type keyword))
  (ecase type
    (:api (make-instance 'api-repl))
    (:impl (make-instance 'impl-repl))))


(defun repl (&key (in *standard-input*) (out *standard-output*) (field-order *default-field-order*) (prompt nil prompt-p)
               cli (type *repl-type*))
  (let* ((repl (make-repl type))
         (package (repl-package repl))
         (readtable (make-repl-readtable))
         (prompt (if prompt-p prompt (format nil "~A~A" (identifier repl) *prompt*)))
         (state (make-repl-state :package package
                                 :env (empty-env repl)
                                 :evaluator (make-evaluator repl field-order)
                                 :field-order field-order
                                 :prompt prompt
                                 :in in
                                 :out out
                                 :readtable readtable)))
    (format out "Lurk REPL [~A].~%:help for help.~%" (identifier repl))
    (loop while state do
      (handler-case (let* ((*package* (repl-state-package state))
                           (input (read-with-prompt state :in in :out out)))
                      (multiple-value-bind (new-state input command-p)
                          (maybe-handle-repl-command repl state input)
                        (cond
                          (command-p (setq state new-state))
                          (t (handle-expr repl input state)))))
        (error (e) (format (repl-state-out state) "ERROR: ~A" e))
        (condition (c) (format (repl-state-out state) "~A" c))))
    (when cli
      (sb-ext:exit))))

(deftype meta-form () '(cons (eql !) t))

(defun handle-expr (repl input state)
  (typecase input
    (meta-form
     (destructuring-bind (bang meta-form)
         input
       (declare (ignore bang))
       (handle-meta-form repl state meta-form)))
    (t (let ((result-values (multiple-value-list (eval-expr input state))))
         (format-result-values repl state result-values)
         (values state result-values nil)))))

(defun handle-meta-form (repl state form)
  (typecase form
    (cons
     (destructuring-bind (head . rest)
         form
     (case head
       (:assert
        (assert (not (eq (repl-nil repl) (eval-expr (car rest) state))))
        state
        )
       (:assert-eq
        (assert (eq (eval-expr (first rest) state) (eval-expr (second rest) state)))
        state)
       (:load
        (let ((new-state (load-lib state (car rest))))
          new-state))
       (t (format (repl-state-out state) "Unhandled: ~S" head)
        state))))
    (t (format (repl-state-out state) "Unhandled: ~S" form)
     state)))

(defun eval-expr (expr state)
  (funcall (repl-state-evaluator state) expr (repl-state-env state)))

(defun read-with-prompt (repl-state &key (in *standard-input*) (out *standard-output*))
  (let ((*package* (repl-state-package repl-state))
        (*readtable* (repl-state-readtable repl-state)))
    (format out "~%~A" (repl-state-prompt repl-state))
    (force-output out)
    (read in)))

(defun maybe-handle-repl-command (repl state input)
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
     (let* ((pathname (pathname (read-form state)))
            (pathname (if *load-truename*
                          (merge-pathnames pathname *load-truename*)
                          pathname))
            (*load-truename* (truename pathname)))
       (with-open-file (in pathname :direction :input)
         (loop for (to-run inputp) = (multiple-value-list (read-one-form state in pathname))
               while inputp do (setf state (handle-expr repl to-run state)))
         ;; This is a hack. Pretend this was not a command, and replace the input with expression from file.
         (values state nil t))))
    (:clear
     (let ((new-state (copy-repl-state state)))
       (setf (repl-state-env new-state) (api-impl:empty-env))
       (values new-state input t)))
    (t (values state input nil))))


(defun read-form (state)
  (let* ((*package* (repl-state-package state))
         (*readtable* (repl-state-readtable state))
         (form (read (repl-state-in state) nil 'eof)))
    ;; NOTE: 'LURK.TOOLING.REPL::EOF is not in *PACKAGE*.
    (if (eq form 'eof)
        (values nil nil)
        (values form t))))

(defun load-lib (state pathname)
  (let* ((pathname (pathname pathname))
         (pathname (if *load-truename*
                       (merge-pathnames pathname *load-truename*)
                       pathname))
         (*load-truename* (truename pathname))
         (to-load (read-from-file state pathname))
         (new-env (eval-expr to-load state))
         (new-state (copy-repl-state state)))
    (setf (repl-state-env new-state) new-env)
    new-state))

(defun read-from-file (state pathname)
  (with-open-file (in pathname :direction :input)
    (read-one-form state in pathname)))

(defun read-one-form (state in pathname)
  (let* ((*package* (repl-state-package state))
         (*readtable* (repl-state-readtable state))
         (form (read in nil 'eof)))
    (cond
      ((eq form 'eof) (values nil nil))
      (t (format (repl-state-out state) "Read from ~A: ~S~%" pathname form)
         (values form t)))))

(defparameter *commands*
  '((:help () "Print this text.")
    (:quit () ":Quit REPL.")
    (:echo (form) " Read one form and echo it.")
    (:load (path) "Load a library.")
    (:clear () "Clear loaded libraries.")
    (:run (path) "Evaluate expressions from file.")))

(defun print-help (out)
  (loop for (command args help-text) in *commands*
        do (progn
             (format out "~&~S" command)
             (loop for arg in args do
               (format out " <~A>" arg))
             (format out " => ~A~%" help-text))))
