(defpackage lurk
  (:use :common-lisp)
  (:nicknames :rd)
  (:import-from :it.bese.FiveAm #:in-suite #:def-suite #:test #:is #:run! #:signals)
  (:export
   #:master-suite
   #:random-elt
   ))

(in-package lurk)

(def-suite master-suite)

(defgeneric random-elt (x &optional random-state)
  (:method ((seq sequence)  &optional (*random-state* *random-state*))
    (elt seq (random (length seq) *random-state*))))

