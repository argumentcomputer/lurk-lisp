(cl:in-package common-lisp)

(defpackage lurk.tooling.repl
  (:nicknames :repl)
  (:local-nicknames (#:api #:lurk.api)
                    (#:api-impl #:lurk.api.impl))
  (:use :common-lisp)
  (:import-from :it.bese.FiveAm #:def-suite #:def-suite* #:in-suite #:test #:is #:run! #:signals #:finishes #:skip)
  (:import-from :defstar #:defvar* #:defun* #:defgeneric* #:defmethod* #:lambda* #:nlet #:*let #:result
                #:-> #:labels*)
  (:import-from :lurk.macros #:display #:symbolconc #:awhen #:it)
  (:shadow atom)
  (:export #:repl #:load-lib #:clear-libs #:make-repl-and-state #:run #:quasi #:uq))

(defpackage lurk.api-user
  (:nicknames :lau)
  (:use :lurk.tooling.repl :lurk.api :lurk.api.impl))

(defpackage lurk.impl-user
  (:nicknames :liu)
  (:use :lurk.tooling.repl :lurk.lang))

(defpackage lurk.example
  (:use :common-lisp)
  (:local-nicknames (#:repl #:lurk.tooling.repl))
  (:import-from :it.bese.FiveAm #:def-suite #:def-suite* #:in-suite #:test #:is #:run! #:signals #:finishes #:skip))
