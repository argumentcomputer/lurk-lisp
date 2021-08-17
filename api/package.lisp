(in-package common-lisp)
(defpackage rhododendron.api
  (:nicknames :api)
  (:use :common-lisp)
  ;; QUOTE and NIL are not shadowed, to ease list syntax handling.
  (:shadow #:car #:cdr #:cons #:eq #:if #:lambda #:list #:t #:+ #:- #:* #:/ #:=)
  (:export #:car #:cdr #:cons #:current-env #:eq #:if #:lambda #:let* #:letrec* #:list #:nil #:quote #:t #:+ #:- #:* #:/ #:=))

(defpackage rhododendron.api.impl
  (:nicknames :api.impl)
  (:local-nicknames (#:api #:rhododendron.api))
  (:use :common-lisp)
  (:import-from :it.bese.FiveAm #:def-suite #:def-suite* #:in-suite #:test #:is #:run! #:signals #:finishes #:skip)
  (:import-from :defstar #:defvar* #:defun* #:defgeneric* #:defmethod* #:lambda* #:nlet #:*let #:result
                #:-> #:labels*)
  (:import-from :rhododendron.macros #:display #:symbolconc #:awhen #:it)
  (:shadow atom)
  (:export #:eval-expr #:empty-env #:make-evaluator #:*default-p*))

(defpackage rhododendron.api.tooling
  (:nicknames :tooling)
  (:local-nicknames (#:api #:rhododendron.api)
                    (#:impl #:rhododendron.api.impl))
  (:use :common-lisp)
  (:import-from :it.bese.FiveAm #:def-suite #:def-suite* #:in-suite #:test #:is #:run! #:signals #:finishes #:skip)
  (:import-from :defstar #:defvar* #:defun* #:defgeneric* #:defmethod* #:lambda* #:nlet #:*let #:result
                #:-> #:labels*)
  (:import-from :rhododendron.macros #:display #:symbolconc #:awhen #:it)
  (:shadow atom)
  (:export #:repl #:load-lib #:clear-libs))

(defpackage rhododendron.user
  (:nicknames :rh-user)
  (:use :rhododendron.api.tooling :rhododendron.api))
