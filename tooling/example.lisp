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

(defparameter *project-dir* nil); (make-pathname :directory '(:absolute :home "fil" "lurk")))

(test examples
  (let* ((root-dir (or *project-dir* (uiop/os:getcwd)))
         (bin-dir (merge-pathnames "bin/" root-dir))
         (example-dir (merge-pathnames "example/" root-dir))
         (lurk-bin (merge-pathnames "lurkx" bin-dir)))
    (dolist (test-file *example-tests*)
      (let ((merged (merge-pathnames test-file example-dir)))
        (when (member :api *repl-types-to-test*)
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program (format nil "~A ~A --type API" (namestring lurk-bin) merged))
            (when (not (zerop exit-code))
              (write error-output *error-output*))
            (is (zerop exit-code))))
        (when (member :impl *repl-types-to-test*)
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program (format nil "~A ~A --type IMPL" (namestring lurk-bin) merged))
            (when (not (zerop exit-code))
              (write error-output *error-output*))
            (is (zerop exit-code))))))))
