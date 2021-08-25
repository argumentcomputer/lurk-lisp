(defsystem "lurk"
  :description "lurk"
  :version "0.0.1"
  :author "porcuquine <porcuquine@gmail.com>"
  :licence "MIT"
  :depends-on ("defstar" "fiveam")
  :components ((:module "base"
                        :serial t
                        :components
                        ((:file "base")
                         (:file "macros")))
               (:module "api"
                        :depends-on ("base")
                        :serial t
                        :components
                        ((:file "package")
                         (:file "api")
                         (:file "tooling"))))
  :in-order-to ((test-op (load-op "lurk")))
  :perform (test-op (o c)
		    (flet ((run-suite (suite) (symbol-call :fiveam :run! suite)))
		      (let* ((suite-specs '((#:master-suite #:lurk)))
                             (failed (loop for spec in suite-specs
                                           for (name-spec package-spec) = spec
                                           for suite = (find-symbol (string name-spec) (string package-spec))
                                           unless (run-suite suite)
                                             collect suite)))
			(when failed
			  (error "Some tests failed: ~A." failed))))))
