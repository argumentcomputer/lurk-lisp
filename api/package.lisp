(cl:in-package common-lisp)

(defpackage lurk.api
  (:nicknames :api)
  (:use :common-lisp)
  ;; QUOTE and NIL are not shadowed, to ease list syntax handling.
  (:shadow #:atom #:car #:cdr #:cons #:eq #:if #:lambda #:t #:+ #:- #:* #:/ #:=)
  (:export #:atom #:car #:cdr #:cons #:current-env #:emit #:eq #:if #:lambda #:let #:letrec #:nil #:quote #:macroexpand
           #:t #:+ #:- #:* #:/ #:=))

(defpackage lurk.api.ram
  (:nicknames :api.ram)
  (:use :common-lisp :lurk.api)
  (:shadowing-import-from :lurk.api  #:atom #:car #:cdr #:cons #:defmacro #:eq #:if #:lambda #:t #:+ #:- #:* #:/ #:=)
  (:export #:atom #:car #:cdr #:cons #:current-env #:current-ram #:eq #:define #:defmacro #:if #:lambda #:let #:letrec #:nil #:quote
           #:t #:+ #:- #:* #:/ #:=
           #:quasi #:uq #:uqs))

(defpackage lurk.api.impl
  (:nicknames :api.impl)
  (:local-nicknames (#:api #:lurk.api)
                    (#:api.ram #:lurk.api.ram))
  (:use :common-lisp)
  (:import-from :it.bese.FiveAm #:def-suite #:def-suite* #:in-suite #:test #:is #:run! #:signals #:finishes #:skip)
  (:import-from :defstar #:defvar* #:defun* #:defgeneric* #:defmethod* #:lambda* #:nlet #:*let #:result
                #:-> #:labels*)
  (:import-from :lurk.macros #:display #:symbolconc #:awhen #:it)
  (:shadow #:atom #:eval)
  (:export #:eval #:eval-expr #:empty-env #:empty-ram #:find-subset #:make-evaluator #:*default-p*
           #:directly-contains #:subset #:intern-subset #:subset-package #:min-subset #:core-subset #:ram-subset))

