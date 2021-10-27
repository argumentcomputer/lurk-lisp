(in-package lurk.example)

(def-suite* example-suite :in lurk:master-suite)

(defvar *example-tests* (list "test.lurk"
                              "micro-tests.lurk"
                              "meta-tests.lurk"
                              "meta-letrec-tests.lurk"))

(defvar *repl-types-to-test* '(:api
                               ;; FIXME: Uncomment when :impl tests are passing.
                               ;; :impl
                               ))

(test examples
  (let* ((bin-dir (merge-pathnames "bin/" (uiop/os:getcwd)))
         (example-dir (merge-pathnames "example/" (uiop/os:getcwd)))
         (lurk-bin (merge-pathnames "lurkx" bin-dir)))
    (dolist (test-file *example-tests*)
      (let ((merged (merge-pathnames test-file example-dir)))
        (when (member :api *repl-types-to-test*)
          (uiop:run-program (format nil "~A ~A --type API" (namestring lurk-bin) merged)))
        (when (member :impl *repl-types-to-test*)
          (uiop:run-program (format nil "~A ~A --type IMPL" (namestring lurk-bin) merged))
          )))))
