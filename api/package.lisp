(cl:in-package common-lisp)

(defpackage lurk.api
  (:nicknames :api)
  (:use :common-lisp)
  ;; QUOTE and NIL are not shadowed, to ease list syntax handling.
  (:shadow #:atom #:car #:cdr #:cons #:eq #:if #:lambda #:t #:+ #:- #:* #:/ #:=)
  (:export #:atom #:car #:cdr #:cons #:current-env #:eq #:if #:lambda #:lambda-macro #:let* #:letrec* #:nil #:quote #:t #:+ #:- #:* #:/ #:=))

(defpackage lurk.api.impl
  (:nicknames :api.impl)
  (:local-nicknames (#:api #:lurk.api))
  (:use :common-lisp)
  (:import-from :it.bese.FiveAm #:def-suite #:def-suite* #:in-suite #:test #:is #:run! #:signals #:finishes #:skip)
  (:import-from :defstar #:defvar* #:defun* #:defgeneric* #:defmethod* #:lambda* #:nlet #:*let #:result
                #:-> #:labels*)
  (:import-from :lurk.macros #:display #:symbolconc #:awhen #:it)
  (:shadow #:atom #:eval)
  (:export #:eval #:eval-expr #:empty-env #:make-evaluator #:*default-p*))

