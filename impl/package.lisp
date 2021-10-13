(defpackage lurk.lang
  (:nicknames :lang)
  (:use :common-lisp)
  (:import-from :it.bese.FiveAm #:def-suite #:def-suite* #:in-suite #:test #:is #:run! #:signals #:finishes #:skip)
  (:import-from :defstar #:defvar* #:defun* #:defgeneric* #:defmethod* #:lambda* #:nlet #:*let #:result
                #:-> #:labels*)
  (:import-from :lurk.macros #:display #:symbolconc)
  (:shadow #:type #:cons)
  (:export #:atom #:car #:cdr #:cons #:current-env #:eq #:if #:lambda #:let* #:letrec* #:nil #:quote #:t #:+ #:- #:* #:/ #:=))
