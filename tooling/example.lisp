(in-package lurk.example)

(def-suite* example-suite :in lurk:master-suite)

(defparameter *lurk-tests* '((:core "test.lurk")
                             (:core "micro-tests.lurk")
                             (:core "meta-tests.lurk")
                             (:core "meta-letrec-tests.lurk")
                             (:core "fibonacci-tests.lurk")
                             (:core "tests/auto-curry.lurk")
                             (:core "tests/spec.lurk")
                             (:ram "ram/ram-tests.lurk")
                             (:ram "ram/macro-tests.lurk")
                             (:ram "ram/quasi-tests.lurk")
                             (:ram "ram/reader-tests.lurk")))

(defparameter *repl-types-to-test* '(:api))

(defparameter *project-dir* nil)

(defun test-example-file (pathname)
  (dolist (type *repl-types-to-test*)
    (let ((out (make-string-output-stream)))
      (multiple-value-bind (repl state)
          (repl:make-repl-and-state :type type :out out)
        (is (not (null
                  (handler-case
                      ;; Return T if Lurk file is run successfully.
                      (prog1 t
                        (repl:run repl state pathname))
                    ;; Return NIL if an error is signaled while running.
                    (error () nil))))
            "~A" (get-output-stream-string out))))))

(test examples-internally
  (unless (uiop:getenv "ON_CI_SERVER")
    (let* ((root-dir (or *project-dir* (uiop/os:getcwd)))
           (example-dir (merge-pathnames "lurk-lib/example/" root-dir)))
      (dolist (spec *lurk-tests*)
        (destructuring-bind (subset test-file)
            spec
          (let ((subset (case subset
                          (:core (api.impl:intern-subset 'api.impl:core-subset))
                          (:ram (api.impl:intern-subset 'api.impl:ram-subset)))))
            (let ((merged (merge-pathnames test-file example-dir)))
              (dolist (type *repl-types-to-test*)
                (let ((out (make-string-output-stream)))
                  (multiple-value-bind (repl state)
                      (repl:make-repl-and-state :type type :out out :subset subset)
                    (is (not (null
                              (handler-case
                                  ;; Return T if Lurk file is run successfully.
                                  (prog1 t
                                    (repl:run repl state merged))
                                ;; Return NIL if an error is signaled while running.
                                (error () nil))))
                        "~A" (get-output-stream-string out))))))))))))

