(in-package lurk.tooling.repl)

(def-suite* api-tooling-suite :in lurk:master-suite)

(defparameter *prompt* "> ")
(defparameter *default-field-order* lurk.api.impl:*default-p*)
(defparameter *repl-type* :api)

(defstruct repl-state subset package env ram evaluator field-order prompt in out readtable)

(defclass repl () ())
(defclass api-repl (repl) ())

(defun bang-reader (stream char)
  (declare (ignore char))
  (list '! (read stream t nil t)))

;; TODO: It's a little sketchy that we read using these symbols but don't define the corresponding macros. We will
;; eventually need to include some set of built-in macros as part of the minimal default RAM. The quasiquote macros
;; should be in the required set, given that these built-in reader macros depend on them.
(defun backquote-reader (stream char)
  (declare (ignore char))
  (list 'lurk.api.ram:quasi (read stream t nil t)))

(defun comma-reader (stream char)
  (declare (ignore char))
  (case (peek-char nil stream t nil t)
    ((#\@)
     (read-char stream nil nil t)
     (list 'lurk.api.ram:uqs (read stream t nil t)))
    (otherwise
     (list 'lurk.api.ram:uq (read stream t nil t)))))

(defgeneric enhance-readtable-for-subset (subset)
  (:method ((subset api.impl:subset)) (progn))
  (:method ((subset api.impl:ram-subset))
    (set-macro-character #\` #'backquote-reader)
    (set-macro-character #\, #'comma-reader))
  (:method :after ((subset api.impl:subset))
    (dolist (contained (api.impl:directly-contains subset))
      (enhance-readtable-for-subset contained))))

(defun make-subset-readtable (subset)
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\! #'bang-reader)
    (enhance-readtable-for-subset subset)
    *readtable*))

(defgeneric* repl-package ((repl repl))
  (:method ((repl api-repl))
    (find-package :lurk.api-user)))

(defgeneric* identifier ((repl repl))
  (:method ((repl api-repl)) :api))

(defgeneric* repl-nil ((repl repl))
  (:method ((repl api-repl)) nil))

(defgeneric* empty-ram ((repl repl))
  (:method ((repl api-repl))
    (api-impl:empty-ram)))

(defgeneric* empty-env ((repl repl))
  (:method ((repl api-repl))
    (api-impl:empty-env)))

(defgeneric* make-evaluator ((repl repl) (field-order integer))
  (:method ((repl api-repl) (field-order integer))
    (api-impl:make-evaluator field-order)))

(defgeneric* format-output ((repl repl) (out t) (format-string string) &rest format-args)
  (:method ((repl repl) (out t) (format-string string) &rest format-args)
    (apply #'format out format-string format-args)))

(defgeneric* format-result-values ((repl repl) (state repl-state) (values list))
  (:method ((repl repl) (state repl-state) (values list))
    (let ((evaled (car values)))
      (lurk.api.impl:emit-out (repl-state-out state) evaled))))

(defun keywordize (string)
  (intern (string-upcase string) :keyword))

(defun subsetize (string)
  (case (keywordize string)
    (:min (api.impl:intern-subset 'api.impl:min-subset))
    (:core (api.impl:intern-subset 'api.impl:core-subset))
    (:ram (api.impl:intern-subset 'api.impl:ram-subset))))

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :type
   :description "REPL type (api, impl)"
   :short #\t
   :long "type"
   :arg-parser #'keywordize)
  (:name :subset
   :description "Lurk Subset (min, core, ram)"
   :short #\t
   :long "subset"
   :arg-parser #'subsetize))

(defparameter *default-subset* (api.impl:intern-subset 'api-impl:core-subset))

(defun run-repl (&rest args)
  (declare (ignore args))
  (multiple-value-bind (opts free-args)
      (opts:get-opts)
    (let* ((type-arg (getf opts :type))
           (type (if type-arg
                     (intern (string-upcase type-arg) :keyword)
                     *repl-type*))
           (subset-arg (getf opts :subset))
           (subset (or subset-arg *default-subset*))
           (to-run (mapcar #'pathname free-args)))
      (cond
        (to-run
         (multiple-value-bind (repl state)
             (make-repl-and-state :type type :subset subset)
           (dolist (run-pathname to-run)
             (setf state (run repl state run-pathname)))
           (uiop:quit :code 0)))
        (t
         (repl :cli t :type type :subset subset))))))

(defun* make-repl ((type keyword))
  (ecase type
    (:api (make-instance 'api-repl))))


(defun make-repl-and-state (&key (subset *default-subset*) (type *repl-type*) (prompt nil prompt-p) (field-order *default-field-order*) (in *standard-input*)
                                 (out *standard-output*) &allow-other-keys)
  (let* ((repl (make-repl type))
         (package (api.impl:subset-package subset))
         (readtable (make-subset-readtable subset))
         (prompt (if prompt-p prompt (format nil "~A~A" (identifier repl) *prompt*)))
         (state (make-repl-state :subset subset
                                 :package package
                                 :env (empty-env repl)
                                 :ram (empty-ram repl)
                                 :evaluator (make-evaluator repl field-order)
                                 :field-order field-order
                                 :prompt prompt
                                 :in in
                                 :out out
                                 :readtable readtable)))
    (values repl state)))

(defun repl (&rest keys &key (in *standard-input*) (out *standard-output*) (field-order *default-field-order*) (prompt nil prompt-p)
                          cli (type *repl-type*) (subset (api.impl:intern-subset *default-subset-type*)))
  (multiple-value-bind (repl state)
      (apply #'make-repl-and-state keys)
    (format out "Lurk REPL [~A].~%:help for help.~%" (identifier repl))
    (loop while state do
      (handler-case (let* ((*package* (repl-state-package state))
                           (input (read-with-prompt state :in in :out out)))
                      (multiple-value-bind (new-state input command-p)
                          (maybe-handle-repl-command repl state input)
                        (cond
                          (command-p (setq state new-state))
                          (t (setq state (handle-expr repl input state))))))
        (error (e) (format (repl-state-out state) "ERROR: ~A" e))
        (condition (c) (format (repl-state-out state) "~A" c))))
    (when cli
      (uiop:quit :code 0))))

(deftype meta-form () '(cons (eql !) t))

(defun handle-expr (repl input state)
  (typecase input
    (meta-form
     (destructuring-bind (bang meta-form)
         input
       (declare (ignore bang))
       (handle-meta-form repl state meta-form)))
    (t
     (let ((result-values (multiple-value-list (eval-expr input state))))
         (format-result-values repl state result-values)
         (let ((new-state (copy-repl-state state)))
           (setf (repl-state-ram new-state) (third result-values))
           (values new-state result-values nil))))))

(defun handle-meta-form (repl state form)
  (typecase form
    (cons
     (destructuring-bind (head . rest)
         form
     (case head
       (:assert
        (assert (not (eq (repl-nil repl) (eval-expr (car rest) state))))
        state)
       (:assert-eq
        (assert (eq (eval-expr (first rest) state) (eval-expr (second rest) state)))
        state)
       (:assert-error
        (assert (handler-case (prog1 nil (eval-expr (first rest) state))
                  (error () t)))
        state)
       (:clear
        (let ((new-state (copy-repl-state state)))
          (setf (repl-state-env new-state) (api-impl:empty-env))
          new-state))
       (:load
        (let ((new-state (load-lib state (car rest))))
          new-state))
       (t (format (repl-state-out state) "Unhandled: ~S" head)
        state))))
    (t (format (repl-state-out state) "Unhandled: ~S" form)
     state)))

(defun eval-expr (expr state)
  (funcall (repl-state-evaluator state) expr (repl-state-env state) (repl-state-ram state)))

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
     (run repl state (read-form state)))
    (:clear
     (let ((new-state (copy-repl-state state)))
       (setf (repl-state-env new-state) (api-impl:empty-env))
       (values new-state input t)))
    (t (values state input nil))))

(defun run (repl state pathname)
  (let* ((pathname (if *load-truename*
                       (merge-pathnames pathname *load-truename*)
                       pathname))
         (*load-truename* (truename pathname)))
    (with-open-file (in pathname :direction :input)
      (loop for (to-run inputp) = (multiple-value-list (read-one-form state in pathname))
            while inputp do (setf state (handle-expr repl to-run state)))
      ;; This is a hack. Pretend this was not a command, and replace the input with expression from file.
      (values state nil t))))

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
         (*load-truename* (truename pathname)))
    (with-open-file (in pathname :direction :input)
      (loop for (to-load inputp) = (multiple-value-list (read-one-form state in pathname))
            while inputp do
              (let ((new-state (copy-repl-state state)))
                (multiple-value-bind  (new-env dummy-env new-ram)
                    (eval-expr to-load state)
                  (if (eq new-ram (repl-state-ram state))
                      (setf (repl-state-env new-state) new-env)
                      (setf (repl-state-ram new-state) new-ram))
                  (setq state new-state))))))
  state)

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
