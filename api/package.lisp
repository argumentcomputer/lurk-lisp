(cl:in-package common-lisp)

(defpackage lurk.api
  (:nicknames :api)
  (:use :common-lisp)
  ;; QUOTE and NIL are not shadowed, to ease list syntax handling.
  (:shadow #:atom #:car #:cdr #:cons #:eq #:eval #:if #:lambda #:t #:+ #:- #:* #:/ #:=)
  (:export #:atom #:begin #:car #:cdr #:cons #:current-env #:emit #:eq #:eval #:if #:lambda #:let #:letrec  #:macroexpand #:procedure #:nil #:quote
           #:t #:+ #:- #:* #:/ #:=))

(defpackage lurk.api.ram
  (:nicknames :api.ram)
  (:use :common-lisp :lurk.api)
  (:shadowing-import-from :lurk.api #:atom #:car #:cdr #:cons #:compile #:defmacro #:eq #:eval #:if #:lambda  #:macroexpand #:t #:+ #:- #:* #:/ #:=)
  (:export #:atom #:car #:cdr #:cons #:compile #:compile-closure #:current-env #:current-ram #:define #:defmacro #:eq #:eval #:if #:lambda  #:macroexpand #:procedure #:let #:letrec #:nil #:quote
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
  (:shadow #:atom)
  (:export #:eval-expr #:emit-out #:empty-env #:empty-ram #:find-subset #:make-evaluator #:*default-p*
           #:directly-contains #:subset #:intern-subset #:subset-package #:min-subset #:core-subset #:ram-subset))

