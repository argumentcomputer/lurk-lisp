(in-package lurk.example)

(def-suite* example-suite :in lurk:master-suite)

(defparameter *example-tests* (list "test.lurk"
                                    "micro-tests.lurk"
                                    "meta-tests.lurk"
                                    "meta-letrec-tests.lurk"))

(defparameter *repl-types-to-test* '(:api
                                     ;; FIXME: Uncomment when :impl tests are passing.
                                     ;; :impl
                                     ))

(defparameter *project-dir* nil)

(test examples-internally
  (let* ((root-dir (or *project-dir* (uiop/os:getcwd)))
         (example-dir (merge-pathnames "example/" root-dir)))
    (dolist (test-file *example-tests*)
      (let ((merged (merge-pathnames test-file example-dir)))
        (dolist (type *repl-types-to-test*)
          (let ((out (make-string-output-stream)))
            (multiple-value-bind (repl state)
                (repl:make-repl-and-state :type type :out out)
              (is (not (null
                        (handler-case
                            ;; Return T if Lurk file is run successfully.
                            (prog1 t
                              (repl:run repl state merged))
                          ;; Return NIL if an error is signaled while running.
                          (error () nil))))
                  "~A" (get-output-stream-string out)))))))))
