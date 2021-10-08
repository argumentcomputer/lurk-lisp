;;; Historical note: this is the orignal Lurk implementation/prototype. It
;;; supports (approximately) the api of api.lisp and (approximately) the
;;; iteration/continuation semantics of lurk-rs/src/eval.rs.
;;;
;;; TODO: This has slightly diverged from eval.rs and may not quite
;;; support all of api.lisp yet. The next step is to rework the types to be fully
;;; compatible with eval.rs, with all logic duplicated as closely as possible.
;;;
;;; The goal is to eliminate the 'approximately's from the description above.

(in-package lurk.lang)

(def-suite* lang-suite :in lurk:master-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(deftype hash () 'fixnum)
(deftype symbol-list () '(or null (cons symbol symbol-list)))

;; This is underpsecified, since recursive type definitions are not supported,
;; but the intention is that it be used only to refer to proper lists of symbols.
(deftype symbol-list () '(cl:cons symbol list))

(deftype store () 'hash-table)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum types [TODO: move this general-purpose infrastructure elsewhere once adequately incubated.]

(defmacro defsum (name (&body handler) &body body)
  `(let ((variants ())
         (other-args ()))
     ,@(loop for definition in body
             collect `(destructuring-bind (&rest args)
                          (multiple-value-list ,definition)
                        (push (car args) variants)
                        (push args other-args)))
     (prog1
         (deftype ,name () `(or ,@variants))
       (funcall (progn ,@handler) other-args)
       (setf (get ',name 'variants) variants))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging

(defparameter *evaluation-iteration-limit* 100)
(defparameter *trace* nil)

(defmacro disp (&rest rest)
  `(when *trace*
     (display ,@rest)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variant case.
(defmacro vcase ((expr sum-type) &body cases)
  (let* ((types (mapcar #'car cases))
         (types-union `(or ,@types)))
    (loop for variant-type in types
          do (assert (subtypep variant-type sum-type)))
    (assert (subtypep 'expression types-union) () "VCASE variants are not exhaustive for ~S: ~S" sum-type types)
    `(progn
       (etypecase ,expr
           ,@cases))))

(defun* (variants -> symbol-list) ((sum-type-name symbol))
  (get sum-type-name 'variants))

(defun* variant-p ((variant symbol) (sum symbol))
  (when (subtypep variant sum)
    (assert (member variant (variants sum)) () "~S is not a defined variant of ~S" variant sum)
    t))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Placeholder. HASH must be collision-resistant, which SXHASH is not.
;; FIXME: Large calculations are producing errors, most likely due to hash collisions.
(defgeneric* (hash -> fixnum) (thing)
  ;; (:method ((f fn))
  ;;   (sxhash (list 'fn f)))
  #+(or)(:method ((s structure-object))
    (let ((*print-legibly* nil)
          (*elide-function-envs* t))
      (sxhash (list (type-of s) (write-to-string s)))))
  (:method ((thing t))
    (sxhash (list :default thing))))

(defgeneric* (expression-type -> symbol) (thing))

(defmacro define-expression-type (name tag inner-type)
  `(progn
     (assert (typep ,tag 'integer))  ;; This might later need to become a general field element.
     (let ((type (make-expression-type :name ',name :tag ,tag)))
       (defmethod expression-type ((thing ,name)) ',name)
       (setf (gethash ',name *types*) type
             (get ',name 'tag) ',tag)
       (values ',name ,inner-type)
       )))

(defun* (tag -> integer) ((expression-type symbol))
  (get expression-type 'tag))

(defun* (find-expression-type -> (or expression-type null)) ((name symbol))
  (gethash name *types*))

(defstruct fn arg body closed-env)

(defclass continuation ()
  (;(value :initarg :value :accessor continuation-value :type expression)
   (continuation :initarg :continuation :reader continuation :type continuation)))

(defclass thunk ()
  ((value :initarg :value :reader thunk-value :type expression)
   (continuation :initarg :continuation :reader thunk-continuation :type continuation)))

(defmacro define-hashable (name tag (value-initform value-type))
  `(progn
     (defstruct ,name hash (value ,value-initform :type ,value-type))
     (define-expression-type ,name ,tag ',value-type)))

(defmacro define-atomic (name tag (value-initform value-type))
  `(progn
       (defstruct ,name (value ,value-initform :type ,value-type))
     (define-expression-type ,name ,tag ',value-type)))

;; NOTE: We should probably have a separate table per tag type.
;; More simply, we can make the key a pair of (TAG . HASH) and just a single table.
(defclass content-store ()
  ((hash-store :initarg :hash-store :initform (make-hash-table :test 'equal) :accessor hash-store)
   (atom-store :initarg :atom-store :initform (make-hash-table :test 'eql) :accessor atom-store)))

(defgeneric store-size (content-store)
  (:method ((cs content-store))
    (hash-table-size (hash-store cs))))

(defvar *content-store* (make-instance 'content-store))

;; We need the sum type definition to be evaluated at compile time, so it can
;; be used in later VCASE expressions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar* (*types* hash-table) (make-hash-table))

  (defstruct expression-type name tag)

  (defun define-expressible-type (defsum-args)
    (let ((all-inner-types (mapcan #'cdr defsum-args)))
      (deftype expressible () `(or ,@all-inner-types))))

  (defsum expression (#'define-expressible-type)
    (define-atomic nill 0 (nil null))
    (define-hashable hcons 1 (() cl:cons))
    (define-hashable sym 2 (nil symbol))
    (define-hashable fun 3 (() fn))
    (define-atomic num 4 (0 number))
    (define-hashable cont 5 (() thunk)))

  (defun* (carcdr -> (values expression expression)) (l)
    (values (hcar l) (hcdr l)))

  (defmacro let-cons (((car-var cdr-var) cons) &body body)
    `(multiple-value-bind (,car-var ,cdr-var)
         (carcdr ,cons)
       ,@body))

  (test let-cons
    (let-cons ((a b) (cons 1 2))
      (is (eq (num 1) a))
      (is (eq (num 2) b))))

  (defmacro with-fresh-stores (&body body)
    `(let ((*content-store* (make-instance 'content-store
                                           :hash-store (make-hash-table :test 'equal)
                                           :atom-store (make-hash-table :test 'eql))))
       ,@body)))

(defparameter *print-legibly* nil)

;; This is a hack until we can detect circularity in printing. Otherwise,
;; functions closed over environments with a binding to that function result in
;; infinite loops when printing.
(defparameter *elide-function-envs* nil)

(defun set-trace (&optional (tracep t))
  (setq *trace* tracep *print-legibly* tracep *elide-function-envs* tracep))

(defun hcons-tree (thing)
  (typecase thing
    (hcons (let-cons ((car cdr) thing)
             (cl:cons (hcons-tree car)
                      (hcons-tree cdr))))
    (t thing)))

(defun map-tree (f tree)
  (etypecase tree
    (atom (disp tree (atom tree)) (funcall f tree))
    (cl:cons (cl:cons (map-tree f (car tree))
                      (map-tree f (cdr tree))))))

(defun hmapcar (f hlist)
  (if (hnull hlist)
      hlist
      (cons (funcall f (hcar hlist))
            (hmapcar f (hcdr hlist)))))

(defmethod print-object ((hcons hcons) (stream t))
  (if *print-legibly*
      (format stream "<~S>" (hcons-tree hcons))
      (call-next-method)))

(defmethod print-object ((s sym) (stream t))
  (if *print-legibly*
      (format stream "<~S>" (sym-value s))
      (call-next-method)))

(defmethod print-object ((n num) (stream t))
  (if *print-legibly*
      (format stream "<~S>" (num-value n))
      (call-next-method)))

(defmethod print-object ((n nill) (stream t))
  (if *print-legibly*
      (format stream "<~S>" (nill-value n))
      (call-next-method)))


(defmethod print-object ((c continuation) (stream t))
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "~S height: ~d; continuation: ~S" (class-name (class-of c)) (continuation-height c) (when (slot-boundp c 'continuation) (continuation c)))))

(deftype canonicalizable () '(or expression expressible))

(defgeneric* (canonicalize -> expression) ((thing canonicalizable) (content-store content-store))
  (:method ((e structure-object) (content-store content-store))
    (etypecase e
      (expression e)))
  (:method ((nill null) (content-store content-store))
    (canonicalize-atom nil (atom-store content-store) (lambda (ignore) (declare (ignore ignore)) (make-nill))))
  (:method ((number number) (content-store content-store))
    (canonicalize-atom number (atom-store content-store) (lambda (value) (make-num :value value))))
  (:method ((f fn) (content-store content-store))
    (canonicalize-hashable f (atom-store content-store) (lambda (hash value) (make-fun :hash hash :value value))))
  (:method ((cons cl:cons) (content-store content-store))
    (let ((car (canonicalize (car cons) content-store))
          (cdr (canonicalize (cdr cons) content-store)))
      (canonicalize-hashable (cl:cons car cdr) (hash-store content-store)
                             (lambda (hash value) (make-hcons :hash hash :value value)))))
  (:method ((s symbol) (content-store content-store))
    (canonicalize-hashable s (hash-store content-store)
                           (lambda (hash value) (make-sym :hash hash :value value))))
  (:method ((thunk thunk) (content-store content-store))
    (canonicalize-hashable thunk (atom-store content-store) (lambda (hash value) (make-cont :hash hash :value value)))))

(defun* (canonicalize-hashable -> expression) (thing (store store) ctor)
  (let ((hash (hash thing)))
    (multiple-value-bind (val present-p)
        (gethash hash store)
      (if present-p
          val
          (setf (gethash hash store) (funcall ctor hash thing))))))

(defun* (canonicalize-atom -> expression) ((thing atom) (store store) ctor)
  (multiple-value-bind (val present-p)
      (gethash thing store)
    (if present-p
        val
        (setf (gethash thing store) (funcall ctor thing)))))

(defun* expression<- ((thing canonicalizable) &optional (content-store *content-store*))
  (:returns (values expression &optional))
  (canonicalize thing content-store))

(defun* cons (a b &optional (store *content-store*))
  (:returns (values expression &optional))
  ;; This is just a sketch, intended to work reasonably with sxhash and define an interface.
  ;; It will need to be reworked when using an appropriate hash function and data representation.
  (canonicalize (cl:cons a b) store))

(defun* hcar (expr &optional (store *content-store*))
  (if (eq expr (sym nil store))
      (sym nil store)
      (car (hcons-value expr))))

(defun* hcdr (expr &optional (store *content-store*))
  (if (eq expr (sym nil store))
      (sym nil store)
      (cdr (hcons-value expr))))

(test cons-car-cdr
  (with-fresh-stores
    (is (eq (cons 2 1) (cons (hcdr (cons 1 2)) (hcar (cons 1 2)))))))

(defun hnull (expression)
  (eq (sym nil) expression))

(defun hassoc (item alist)
  (if (hnull alist)
      alist
      (let ((head (hcar alist)))
        (if (eq item (hcar head))
            head
            (hassoc item (hcdr alist))))))

(test hassoc
  (with-fresh-stores
    (is (eq (num 4)
            (hcdr (hassoc (num 1) (expression<- '((2 . 3)(1 . 4)))))))))

(defun* nill (&optional (content-store *content-store*))
  (canonicalize nil content-store))

(defun* sym (s &optional (content-store *content-store*))
  (:returns (values expression &optional))
  (canonicalize s content-store))

(defun* num (n &optional (content-store *content-store*))
  (:returns (values expression &optional))
  (canonicalize n content-store))

(defun* fun ((f fn) &optional (content-store *content-store*))
  (:returns (values expression &optional))
  (canonicalize f content-store))

(defun* cont ((thunk thunk) &optional (content-store *content-store*))
  (:returns (values expression &optionAL))
  (canonicalize thunk content-store))

(defmethod print-object ((f fn) (stream t))
  (if *elide-function-envs*
      (format stream "#S(FN :ARG ~S :BODY ~S :CLOSED-ENV <VARS-ONLY ~S>"
              (fn-arg f)
              (fn-body f)
              (hmapcar #'hcar (fn-closed-env f)))
      (call-next-method f stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval

(deftype simple-env (var-type val-type) `(or null (cl:cons (cl:cons ,var-type ,val-type))))
(deftype recursive-env (var-type val-type) `(or null (cl:cons (simple-env ,var-type ,val-type)
                                                              list ;(env ,var-type ,val-type)
                                                              )))
    
(deftype env (var-type val-type) `(or (simple-env ,var-type ,val-type) (recursive-env ,var-type ,val-type)))

(defun empty-symbol-env () ())

;; TODO: Handle NULL as empty list better.
(defun empty-sym-env () (sym nil))

(defgeneric* (extend -> (env t t)) ((env list) (var t) (val t))
  (:method ((env list) (var symbol) (val t))
    (check-type val expressible)
    (cl:cons (cl:cons var val) env))
  (:method ((env t) (var sym) (val t))
    ;; (check-type val expression)
    (cons (cons var val) env)))

(defun* (extend-closure -> fun) ((f fun) (rec-env hlist))
  (let* ((fn (copy-fn (fun-value f)))
         (closed-env (fn-closed-env fn))
         (extended (cons rec-env closed-env))) 
    (disp :extending-closure fn closed-env extended)
    (setf (fn-closed-env fn) extended)
    (fun fn)))

(defgeneric lookup (env var)
  (:method ((env t) (var symbol))
    (check-type env (env symbol expressible))
    (cdr (assoc var env)))
  (:method ((env t) (var sym))
;    (check-type env (env sym expression))
    (hcdr (hassoc var env))))

(defgeneric* (extend-rec -> (recursive-env t t)) ((env list) (var t) (val t))
  (:method ((env list) (var symbol) (val t))
    (check-type val expressible)
    (extend-rec-aux env var val))
  (:method ((env t) (var sym) (val t))
    (disp :extending-with-recursive var val env)
    ;; (check-type val expression)
    (extend-rec-aux2 env var val)))

(defun* (extend-rec-aux -> (recursive-env t t)) ((env (env t t)) (var t) (val t))
  (etypecase env
    ((recursive-env t t) (push (cl:cons var val) (car env)))
    ((env t t) (cl:cons (list (cl:cons var val)) env))))

(defun* extend-rec-aux2 ((env hlist) (var sym) (val expression))
  (let-cons ((binding-or-env rest) env)
    (declare (ignore rest))
    (let-cons ((var-or-binding val-or-more-bindings) binding-or-env)
      (declare (ignore val-or-more-bindings))
      (etypecase var-or-binding
        ;; It's a var, so we are extending a simple env with a recursive env.
        ((or sym nill) (cons (hlist (cons var val)) env))

        (hcons
         ;; It's a binding, so we are extending a recursive env.
         (hlist (cons (cons var val) binding-or-env)))))))

(defclass tail-continuation-mixin () ())

(defclass saved-env-mixin ()
  ((saved-env :initarg :saved-env :reader saved-env)))


(defclass restore-env-mixin (saved-env-mixin) ())

(defclass outermost-continuation (continuation) ())
(defclass terminal-continuation (continuation) ())
(defclass dummy-continuation (continuation) ())

(defgeneric* (continuation-height -> integer) ((continuation continuation))
  (:method ((c null))
    0)
  (:method ((c dummy-continuation))
    1)
  (:method ((c outermost-continuation))
    1)
  (:method ((c terminal-continuation))
    0)
  (:method ((c continuation))
    (1+ (continuation-height (continuation c)))))

;; We are evaluating a call, first evaluate function position then invoke this CALL-CONTINUATION.
(defclass call-continuation (continuation)
  ((saved-env :initarg :saved-env :reader saved-env)
   (args :initarg :args :reader call-args)))

;; We have the function. Next evaluate the argument and invoke the CALL2-CONTINUATION.
(defclass call2-continuation (continuation)
  ((saved-env :initarg :saved-env :reader saved-env)
   (fun :initarg :fun :reader call2-fun)))


;; We have the evaluated argument. Finally evaluate the function body in the extended environment,
;; and invoke the CALL3-CONTINUATION with the result. The CALL3-CONTINUATION should have as its
;; continuation, the one which was the continuation of the original CALL-CONTINUATION.

;; NOTE: In fact, we don't need this at all. We can use a generic CONTINUATION
;; containing the current continuation as its continuation -- but this is ONLY necessary
;; if the current continuation is the OUTERMOST-CONTINUATION. Otherwise we can just use that.

(defclass call3-continuation (tail-continuation-mixin restore-env-mixin continuation) ())

(defclass let-continuation (continuation)
  ((var :initarg :var :reader let-var)
   (saved-env :initarg :saved-env :reader saved-env)
   (body :initarg :body :reader let-body)))

(defclass letrec*-continuation (continuation)
  ((var :initarg :var :reader let-var)
   (saved-env :initarg :saved-env :reader saved-env)
   (body :initarg :body :reader let-body)))

(defun* maybe-wrap-continuation ((cont continuation))
  (:returns (values continuation &optional))
  (if (typep cont 'outermost-continuation)
      (progn (disp :wrapping) (make-instance 'continuation :continuation cont))
      cont))

(defclass error-continuation (continuation)
  ((message :initarg :message :initform "ERROR-CONTINUATION" :reader error-message)
   (args :initarg :args :initform () :reader error-args)))

(defclass lookup-continuation (restore-env-mixin continuation) ())

(defclass args-mixin ()
  ((more-args :initarg :more-args :reader more-args)))

(defclass binop-continuation (continuation args-mixin) ())

(defclass binop2-continuation (continuation)
  ((arg1 :initarg :arg1 :reader arg1)))

(defgeneric next-continuation-type (cont)
  (:method ((cont binop-continuation)) 'binop2-continuation))

(defgeneric binop-operator (specialized-binop-continuation))

;; This will let us have BINOPs with different signatures.
(defgeneric* apply-binop ((cont binop2-continuation) (arg1 expression) (arg2 expression))
  (:method ((cont binop2-continuation) (arg1 t) (arg2 t))
    (let ((operator (binop-operator cont)))
      (num (funcall operator (num-value (arg1 cont)) (num-value arg2))))))

(defmacro def-binop (name operator-function)
  (let ((mixin (symbolconc name '-continuation-mixin))
        (continuation (symbolconc name '-continuation))
        (continuation2 (symbolconc name '2-continuation)))
    `(progn
       (defclass ,mixin () ())
       (defclass ,continuation (binop-continuation ,mixin) ())
       (defclass ,continuation2 (binop2-continuation ,mixin) ())
       (defmethod next-continuation-type ((cont ,continuation)) ',continuation2)
       (defmethod binop-operator ((cont ,mixin)) ,operator-function))))

(defclass relop-continuation (continuation args-mixin) ())

(defclass relop2-continuation (continuation)
  ((arg1 :initarg :arg1 :reader arg1)))

;;; IF
(defclass if-continuation (continuation)
  ((more-args :initarg :more-args :reader more-args)))

(defgeneric relational-operator (specialized-binop-continuation))

(defmacro def-relop (name operator-function)
  (let ((mixin (symbolconc name '-continuation-mixin))
        (continuation (symbolconc name '-continuation))
        (continuation2 (symbolconc name '2-continuation)))
    `(progn
       (defclass ,mixin () ())
       (defclass ,continuation (relop-continuation ,mixin) ())
       (defclass ,continuation2 (relop2-continuation ,mixin) ())
       (defmethod next-continuation-type ((cont ,continuation)) ',continuation2)
       (defmethod relop-operator ((cont ,mixin)) ,operator-function))))

(def-binop sum #'+)
(def-binop diff #'-)
(def-binop product #'*)
(def-binop quotient #'/)
(def-binop cons #'cons)
(def-binop eq #'eq)

;; There is a tradeoff between a minimal and richer set of operators.
;; When we can define and compile these instead, then a smaller circuit will be required.
(def-relop num-equal #'=)

(defclass car-continuation (continuation) ())
(defclass cdr-continuation (continuation) ())

(deftype hlist () `(or nill hcons))

(defun hlist (&rest args)
  (if args
      (cons (car args) (apply #'hlist (cdr args)))
      (nill)))

(test hlist
  (with-fresh-stores
    (is (typep (nill) 'hlist))))

(test hlist-bug
  ;; Regression test for fixed bug. Previously, NIL was treated as a normal symbol,
  ;; so identity was not preserved across stores.
  (with-fresh-stores
    (is (typep (sym nil) 'hlist))
    (is (typep (nill) 'hlist)))

  ;; NOTE that (SYM NIL) still denotes (NILL) and is of type NILL.
  ;; However, it is not a SYM.
  (is (eq (sym nil) (nill)))

  (is (typep (sym nil) 'nill))
  (is (not (typep (sym nil) 'sym))))

#|

Recursive Environments

We currently implement environments as simple alists. At the risk of perhaps
being too clever (but perhaps in the service of maintaining simplicity), can we
extend this model to handle recursive environments while still implementing with
lists?

Idea: Since a normal environment must be a list of pairs, with each pair's CAR
being a symbol, represent recursive environments as environments which are the
CAR of a single-element list.

Example:

(let ((a 1)
      (b 2))
  ;; Don't do this in real life!
  (letrec ((x (lambda (y) (z y)))
           (z (lambda (y) (x y))))
    (x a)))

This will yield the following environment:

(( ;; This is the recursive environment.
  (z . (lambda (x) (x y)))
  (x . (lambda (y) (z y)))
  )
 ;; This is the normal environment.
 (b . 2)
 (a . 1))

|#

(defgeneric* (make-thunk -> (values thunk expression continuation))
    ((continuation continuation) (result expression) (env expression))
  (:method :around ((continuation restore-env-mixin) (result t) (env t))
    (disp :fulfilling-saved-env-continuation)
    (disp (type-of continuation) :restoring-saved-env (saved-env continuation))
    (call-next-method continuation result (saved-env continuation)))
  (:method ((continuation tail-continuation-mixin) (result t) (env t))
    (disp :fulfilling-tail-continuation)
    (make-thunk (continuation continuation) result env))
  (:method ((continuation outermost-continuation) (result t) (env t))
    (values result env (make-instance 'terminal-continuation)))
  (:method ((continuation continuation) (result t) (env t))
    (disp :fulfilling-continuation)
    (let ((thunk (make-instance 'thunk :continuation continuation :value result)))
      (values (cont thunk) env (make-instance 'dummy-continuation)))))

(defun* eval-expr (expr env cont)
  (:returns (values expression t continuation &optional))
  ;; Return the next values to the caller (normally OUTER-EVALUATE),
  ;; which should call EVAL-EXPR again, feeding these outputs in as
  ;; input in a loop until an 'OUTERMOST-CONTINUATION is received. This
  ;; structure makes EVAL-EXPR suitable for use as a circuit
  ;; implementing the core logic of the compliance predicate for an IVC
  ;; program (or a pseudo-IVC program -- one that is computationally
  ;; like IVC, even if there is a cost linear in the number of proofs
  ;; in the final proof aggregating individual node proofs).
  (vcase (expr expression)
    (cont
     (disp :evaling-cont expr cont)
     (assert (slot-boundp expr 'value))
     (let ((thunk (cont-value expr)))
       (invoke-continuation (thunk-continuation thunk) (thunk-value thunk) env)))
    (nill
     (disp :evaling-nil expr)
     (make-thunk cont expr env))
    (sym
     (let-cons ((binding smaller-env) env)
       (cond
         ;; TODO: the NIL and T cases are really just special cases of self-evaluating symbols.
         ;; This may include keywords in the future.
         ((eq expr (nill)) (make-thunk cont expr env))
         ((eq expr (sym t)) (make-thunk cont expr env))
         ((eq binding (nill))
          (values expr env (make-instance 'error-continuation
                                          :message "Unbound variable ~A"
                                          :args (list expr)
                                          :continuation cont)))
         (t ;(let-cons ((var val) binding)
          (let-cons ((var-or-rec-binding val-or-more-rec-env) binding)
            (disp :evaling-sym-lookup expr var-or-rec-binding val-or-more-rec-env)
            (etypecase var-or-rec-binding
              ;; In a SIMPLE-ENV.
              (sym (let ((var var-or-rec-binding)
                         (val val-or-more-rec-env))
                     (if (eq var expr)
                         (make-thunk cont val env)
                         ;; We invoke the current continuation and pass the same
                         ;; continuation as the next continuation. In other words, we are
                         ;; recursing through the env and not growing the nested 'stack'
                         ;; of continuations.
                         (if (typep cont 'lookup-continuation)
                             (values expr smaller-env cont)
                             (values expr smaller-env (make-instance 'lookup-continuation
                                                                     :continuation cont
                                                                     :saved-env env))))))
              ;; Start of a RECURSIVE-ENV.
              (hcons
               (let ((rec-env binding)
                     (smaller-rec-env val-or-more-rec-env))
                 (let-cons ((var val) var-or-rec-binding)
                   (if (eq var expr)
                       (let ((val-to-use (typecase val
                                           (fun
                                            ;; We just found a closure in a recursive env.
                                            ;; We need to extend its environment to include that recursive env.
                                            (extend-closure val rec-env))
                                           (t val))))
                         (make-thunk cont val-to-use env))
                       (let ((env-to-use (if (hnull smaller-rec-env)
                                             smaller-env
                                             (cons smaller-rec-env smaller-env))))
                         (if (typep cont 'lookup-continuation)
                             (values expr env-to-use cont)
                             (values expr env-to-use
                                     (make-instance 'lookup-continuation
                                                    :continuation cont
                                                    :saved-env env))))))))))))))
    (num
     (disp :evaling-num expr cont)
     (make-thunk cont expr env))
    (fun
     (disp :evaling-fun expr cont env)
     (make-thunk cont expr env))
    (hcons
     ;; TODO: Some pattern matching and syntactic sugar will go along way here.
     (let-cons ((head rest) expr)
       (disp :evaling-hcons head rest)
       (cond
         ((eq (sym 'lambda) head)
          ;; (lambda (args) &body body)
          (let-cons ((args body) rest)
            (let ((arg (hcar args)))
              (let* ((inner-body (if (hnull (hcdr args))
                                     body
                                     ;; (LAMBDA (A B) STUFF)
                                     ;; becomes (LAMBDA (A) (LAMBDA (B) STUFF))
                                     (expression<- `((lambda ,(hcdr args) . ,body)))))
                     (function (fun (make-fn :arg arg :body inner-body :closed-env env))))
                (disp :interpreting-as-lambda head function inner-body env)
                (make-thunk cont function env)))))
         ((eq (sym 'let) head)
          (disp :interpreting-as-let)
          (let-cons ((bindings body) rest)
            (disp bindings)
            (let-cons ((body1 rest-body) body)
              ;; Only single body form allowed for now.
              (assert (hnull rest-body))
              (if (hnull bindings)
                  ;; TODO: Refactor.
                  (values body1 env (maybe-wrap-continuation cont))
                  (let-cons ((binding1 rest-bindings) bindings)
                    (disp binding1 rest-bindings)
                    (let-cons ((var more-vals) binding1)
                      (let-cons ((val end) more-vals)
                        (assert (hnull end))
                        (disp var val)
                        (let ((expanded (if (hnull rest-bindings)
                                            body1
                                            (expression<- `(let ,rest-bindings
                                                             ,body1)))))
                          (values val env (make-instance 'let-continuation
                                                         :var var
                                                         :body expanded
                                                         :saved-env env
                                                         :continuation (maybe-wrap-continuation cont)))))))))))
         ((eq (sym 'letrec*) head)
          (disp :interpreting-as-letrec*)
          (let-cons ((bindings body) rest)
            (disp bindings)
            (let-cons ((body1 rest-body) body)
              ;; Only single body form allowed for now.
              (assert (hnull rest-body))
              (if (hnull bindings)
                  ;; TODO: Refactor.
                  (values body1 env (maybe-wrap-continuation cont))
                  (let-cons ((binding1 rest-bindings) bindings)
                    (disp binding1 rest-bindings)
                    (let-cons ((var more-vals) binding1)
                      (let-cons ((val end) more-vals)
                        (assert (hnull end))
                        (disp var val)
                        (let ((expanded (if (hnull rest-bindings)
                                            body1
                                            (expression<- `(letrec* ,rest-bindings
                                                                    ,body1)))))
                          (values val env (make-instance 'letrec*-continuation
                                                         :var var
                                                         :body expanded
                                                         :saved-env env
                                                         :continuation (maybe-wrap-continuation cont)))))))))))
         ;; TODO: Implement LIST.
         ((eq (sym 'cons) head)
          (let-cons ((arg1 more) rest)
            (values arg1 env (make-instance 'cons-continuation
                                            :more-args more
                                            :continuation cont))))
         ((eq (sym 'eq) head)
          (let-cons ((arg1 more) rest)
            (values arg1 env (make-instance 'eq-continuation
                                            :more-args more
                                            :continuation cont))))
         ((eq (sym '+) head)
          (let-cons ((arg1 more) rest)
            (values arg1 env (make-instance 'sum-continuation
                                            :more-args more
                                            :continuation cont))))
         ((eq (sym '-) head)
          (let-cons ((arg1 more) rest)
            (values arg1 env (make-instance 'diff-continuation
                                            :more-args more
                                            :continuation cont))))
         ((eq (sym '*) head)
          (let-cons ((arg1 more) rest)
            (values arg1 env (make-instance 'product-continuation
                                            :more-args more
                                            :continuation cont))))
         ((eq (sym '/) head)
          (let-cons ((arg1 more) rest)
            (values arg1 env (make-instance 'quotient-continuation
                                            :more-args more
                                            :continuation cont))))
         ((eq (sym '=) head)
          (let-cons ((arg1 more) rest)
            (values arg1 env (make-instance 'num-equal-continuation
                                            :more-args more
                                            :continuation cont))))
         ((eq (sym 'if) head)
          (let-cons ((condition more) rest)
            (values condition env (make-instance 'if-continuation
                                                 :more-args more
                                                 :continuation cont))))
         (t
          ;; (fn . args)
          (let ((fn head)
                (args rest))
            (let-cons ((arg more-args) args)
              (cond
                ;; Zero-arg function
                ;;(fn)
                ((eq (nill) args)
                 (disp :interpreting-as-zero-arg-call fn)
                 (let ((continuation (make-instance 'call-continuation
                                                    :continuation cont
                                                    :saved-env env
                                                    :args ())))
                   (values fn env continuation)))
                ((eq (nill) more-args)
                 (disp :interpreting-as-unary)
                 ;; (fn arg1)
                 (cond
                   ;; Built-in unary operators.
                   ((eq head (sym 'quote))
                    (disp :interpreting-as-quote arg cont)
                    ;; Quoted expressions are not evaluated. Just invoke the continuation.
                    (make-thunk cont arg env))
                   ((eq head (sym 'car))
                    (values arg env (make-instance 'car-continuation :continuation cont)))
                   ((eq head (sym 'cdr))
                    (values arg env (make-instance 'cdr-continuation :continuation cont)))    
                   (t (disp :interpreting-as-call fn arg)
                      (let ((continuation (make-instance 'call-continuation
                                                         :continuation cont
                                                         :saved-env env
                                                         :args (list arg))))
                        (values fn env continuation)))))
                (t
                 ;; Because built-in operators/functions are matched first
                 ;; (above), they cannot be shadowed. If this were not the
                 ;; case, we would be forced to traverse the whole env on
                 ;; every function call -- in order to rule out the
                 ;; possibility that a built-in be shadowed.
                 (let ((expanded (expression<- `((,fn ,arg) . ,more-args))))
                   (disp :interpreting-as-multiarg-call fn arg more-args expanded)
                   (values expanded env (maybe-wrap-continuation cont)))))))))))))

(defgeneric* invoke-continuation ((cont continuation) (result expression) (new-env (or nill hcons)))
  (:method :around ((cont t) (result t) (new-env t))
    (multiple-value-bind (cont-result cont-new-env cont-new-cont)
        (call-next-method)
      (disp cont-result cont-new-env cont-new-cont)
      (values cont-result cont-new-env cont-new-cont)))
  (:method ((cont terminal-continuation) (result t) (new-env t))
    (error "TERMINAL-CONTINUATION should never be invoked."))
  (:method ((cont outermost-continuation) (result t) (new-env t))
    (values (continuation-value cont) new-env (make-instance 'terminal-continuation)))
  (:method ((cont dummy-continuation) (result t) (new-env t))
    (error "DUMMY-CONTINUATION should never be invoked."))
  (:method ((cont call-continuation) (result t) (new-env t))
    (etypecase result
      (fun
       (cond
         ((call-args cont)
          ;; We are calling a function with more than zero arguments.
          (let* ((function result)
                 (next-expr (car (call-args cont))) ;; We need to evaluate the function's argument.
                 (newer-cont
                  (make-instance 'call2-continuation
                                 :fun result
                                 ;; Return to the previous continuation after the new one.
                                 ;; We never need to return to the current continuation.
                                 ;; MAYBE-WRAP-CONTINUATION wraps the previous continuation,
                                 ;; in case it is the OUTERMOST-CONTINUATION, so we don't return
                                 ;; prematurely from OUTER-EVALUTE.
                                 :saved-env (saved-env cont)
                                 :continuation (maybe-wrap-continuation (continuation cont)))))
            (disp :invoking-call-continuation function next-expr)
            ;; Only one arg supported for now.
            (check-type (cdr (call-args cont)) null)
            (values next-expr new-env newer-cont)))
         (t
          (let* ((function result)
                 (fn (fun-value function))
                 (body-form (hcar (fn-body fn))))
            (disp :invoking-zero-arg-call-continuation)
            ;; We are calling a zero-arg function.
            (assert (hnull (fn-arg fn)))
            (assert (hnull (hcdr (fn-body fn))))
            (values body-form new-env (make-instance 'call3-continuation :continuation (continuation cont) :saved-env (saved-env cont)))))))
      (t (when *trace*
           (disp :invoking-a-non-function result new-env))
         (values result new-env (make-instance 'error-continuation
                                               :message "bad function ~S"
                                               :args (list result))))))
  (:method ((cont call2-continuation) (result t) (new-env t))
    (let* ((evaled-arg result)
           (fun (call2-fun cont))
           (fn (fun-value fun))
           (body-form (hcar (fn-body fn)))
           (newer-env (extend (fn-closed-env fn) (fn-arg fn) evaled-arg)))
      (disp :call2-continuation (continuation cont) fn new-env)
      ;; For the moment, support only a single expression in function body.
      (assert (typep (hcdr (fn-body fn)) 'nill))
      ;; TODO: We can create a new function whose body is the CDR of this FN-BODY.
      ;; Then create and return a new CALL2-CONTINUATION whose continuation is (CONTINUATION-CONT),
      ;; and whose FUN is the new function. However, since there are no side effects for now,
      ;; this will have no effect -- so forbidding it is probably the best strategy.
      (typecase (continuation cont)
        ;; If the previous continuation was a terminal call continuation
        ;; then this is in tail position, and we can call its continuation
        ;; directly. This is the tail-call elimination optimization.
        (tail-continuation-mixin
         (values body-form newer-env (continuation cont)))
        (t
         (values body-form newer-env (make-instance 'call3-continuation :continuation (continuation cont) :saved-env (saved-env cont)))))))
  (:method ((cont let-continuation) (result t) (new-env t))
    (disp :let-continuation cont (continuation cont) result new-env)
    (let ((extended-env (extend new-env (let-var cont) result))
          (c (make-instance 'call3-continuation
                            :continuation (continuation cont)
                            :saved-env (saved-env cont))))
      (values (let-body cont) extended-env
              c)))
  (:method ((cont letrec*-continuation) (result t) (new-env t))
    (disp :letrec*-continuation cont result new-env)    
    (let ((extended-env (extend-rec new-env (let-var cont) result))
          (c (make-instance 'call3-continuation
                            :continuation (continuation cont)
                            :saved-env (saved-env cont))))
      (values (let-body cont) extended-env c)))
  (:method ((cont binop-continuation) (arg1 t) (new-env t))
    (disp :binop-continuation cont (binop-operator cont) arg1 new-env)
    (let-cons ((arg2 rest) (more-args cont))
      (check-type rest nill)
      (values arg2 new-env (make-instance (next-continuation-type cont)
                                          :arg1 arg1
                                          :continuation (continuation cont)))))
  (:method ((cont binop2-continuation) (arg2 t) (env t))
    (disp :binop2-continuation arg2)
    (let (;; TODO: Some operations (at least division) will require a run-time check
          ;; and should return an explicit error (for example, divide-by-zero).
          (result
           (apply-binop cont (arg1 cont) arg2)))
      (make-thunk (continuation cont) result env)))
  (:method ((cont cons2-continuation) (arg2 t) (env t))
    (disp :cons2-continuation arg2)
    (make-thunk (continuation cont) (cons (arg1 cont) arg2) env))
  (:method ((cont eq2-continuation) (arg2 t) (env t))
    (disp :eq2-continuation)
    (make-thunk (continuation cont) (sym (eq (arg1 cont) arg2)) env))
  (:method ((cont car-continuation) (arg t) (env t))
    (disp :car-continuation)
    (make-thunk (continuation cont) (hcar arg) env))
  (:method ((cont cdr-continuation) (arg t) (env t))
    (disp :cdr-continuation arg)
    (make-thunk (continuation cont) (hcdr arg) env))
  (:method ((cont relop-continuation) (arg1 t) (new-env t))
    (disp :relop-continuation cont (relop-operator cont) arg1 new-env)
    (let-cons ((arg2 rest) (more-args cont))
      (check-type rest nill)
      (values arg2 new-env (make-instance (next-continuation-type cont)
                                          :arg1 arg1
                                          :continuation (continuation cont)))))
  (:method ((cont relop2-continuation) (arg2 t) (env t))
    (disp :relop2-continuation)
    (let* ((operator (relop-operator cont))
           ;; TODO: Should we have an explicit boolean type?
           ;; Treating result as a SYM follows Common Lisp.
           (result (sym (funcall operator (num-value (arg1 cont)) (num-value arg2)))))
      (make-thunk (continuation cont) result env)))
  (:method ((cont if-continuation) (condition t) (env t))
    (disp :if-continuation)
    (let-cons ((arg1 more) (more-args cont))
      (if (hnull condition)
          (let-cons ((arg2 end) more)
            (check-type end nill)
            (values arg2 env (continuation cont)))
          (values arg1 env (continuation cont)))))
  (:method ((cont continuation) (result t) (new-env t))
    (disp :invoking-continuation cont result new-env)
    ;; TODO: Maybe we should make this an error again and use an explicit continuation type
    ;; for the simple case. That makes it easier to catch failure to implement a new continuation type.
                                        ;    (error "Unhandled continuation: ~S" (class-of cont))
    (make-thunk (continuation cont) result new-env)))

(defun* outer-evaluate ((expr expression) (env t) &key (limit *evaluation-iteration-limit*))
  (disp :outer-top)
  (let ((next-cont (make-instance 'outermost-continuation))
        (next-expr expr)
        (next-env env))
    (loop for i from 1 to limit do
         (multiple-value-bind (new-expr new-env new-cont)
             (eval-expr next-expr next-env next-cont)
           (disp :outer-evaluate-loop i new-expr new-env new-cont)
           (typecase new-cont
             (terminal-continuation
              (return-from outer-evaluate
                                      (values new-expr new-env i)))
             (error-continuation
              (error (make-instance 'simple-error
                                    :format-control (error-message new-cont)
                                    :format-arguments (error-args new-cont)))))

           (setq next-expr new-expr
                 next-cont new-cont
                 next-env new-env)))
    (values nil nil limit next-cont)))

(test cons-symbol-env
  (with-fresh-stores
    (let ((env (empty-symbol-env)))
      (is (null (lookup env 'x)))
      (let ((extended (extend env 'x 123)))
        (is (eq (lookup extended 'x) 123)))
      (signals error (extend env 'x (num 123))))))

(test hcons-sym-env
  (with-fresh-stores
    (let ((env (empty-sym-env)))
      (is (eq (empty-sym-env) (lookup env (sym 'x))))
      (let ((extended (extend env (sym 'x) (num 123))))
        (is (eq (lookup extended (sym 'x)) (num 123)))))))

(test eval-expr
  (with-fresh-stores
    (is (eq (num 123)
            (eval-expr (num 123) (empty-sym-env) (make-instance 'outermost-continuation))))
    (let ((extended (extend (empty-sym-env) (sym 'x) (num 123))))
      (is (eq (num 123)
              (eval-expr (sym 'x) extended (make-instance 'outermost-continuation)))))))

(def-suite* outer-evaluate-suite :in lang-suite)

(test outer-evaluate
  (with-fresh-stores
    (let ((limit 20)
          (val (num 999)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `((lambda (x)
                                            x)
                                          ,val))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 7 iterations))
        (is (eq val result-expr))
        (is (null continuation))))))

(test outer-evaluate2
  (with-fresh-stores
    (let ((limit 20)
          (val (num 999))
          (unused (num 888)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `((lambda (y)
                                            ((lambda (x) y)
                                             ;; FIXME: This should fail when ,UNUSED is removed, but it doesn't.
                                             ,unused))
                                          ,val))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 14 iterations))
        (is (eq val result-expr))
        (is (null continuation))))))

(test outer-evaluate3
  (with-fresh-stores
    (let ((limit 30)
          (val (num 999)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `((lambda (y)
                                            ((lambda (x)
                                               ((lambda (z) z)
                                                x))
                                             y))
                                          ,val))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 17 iterations))
        (is (eq val result-expr))
        (is (null continuation))))))

(test outer-evaluate4
  (let ((limit 30)
        (val (num 999))
        (val2 (num 888)))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `((lambda (y)
                                          ((lambda (x)
                                             ((lambda (z) z)
                                              x))
                                           ;; NOTE: We pass a different value here.
                                           ,val2))
                                        ,val))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 17 iterations))
      ;; And it is matched correctly here.
      (is (eq val2 result-expr))
      (is (null continuation)))))

(test outer-evaluate5
  (with-fresh-stores
    (let ((limit 30)
          (val (num 999)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(((lambda (fn)
                                             (lambda (x) (fn x)))
                                           (lambda (y) y))
                                          ,val))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 19 iterations))
        (is (eq val result-expr))
        (is (null continuation))))))

(test outer-evaluate-sum
  (with-fresh-stores
    (let ((limit 20)
          (a (num 2))
          (b (num 3))
          (c (num 4)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(+ ,a (+ ,b ,c)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 9 iterations))
        (is (eq (num 9) result-expr))
        (is (null continuation))))))

(test outer-evaluate-diff
  (with-fresh-stores
    (let ((limit 20)
          (a (num 9))
          (b (num 5)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(- ,a ,b))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 5 iterations))
        (is (eq result-expr (num 4)))
        (is (null continuation))))))

(test outer-evaluate-product
  (with-fresh-stores
    (let ((limit 20)
          (a (num 9))
          (b (num 5)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(* ,a ,b))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 5 iterations))
        (is (eq result-expr (num 45)))
        (is (null continuation))))))

(test outer-evaluate-quotient
  (with-fresh-stores
    (let ((limit 20)
          (a (num 21))
          (b (num 7)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(/ ,a ,b))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 5 iterations))
        (is (eq result-expr (num 3)))
        (is (null continuation))))))

(test outer-evaluate-num-equal
  (with-fresh-stores
    (let ((limit 20)
          (a (num 5))
          (b (num 5))
          (c (num 6)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(= ,a ,b))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 5 iterations))
        (is (eq result-expr (sym t))
            (is (null continuation))))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(= ,a ,c))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 5 iterations))
        (is (eq result-expr (nill))
            (is (null continuation)))))))

(test outer-evaluate-adder
  (with-fresh-stores
    (let ((limit 30)
          (a (num 2))
          (b (num 3)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(((lambda (x)
                                             (lambda (y)
                                               (+ x y)))
                                           ,a)
                                          ,b))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 18 iterations))
        (is (eq result-expr (num 5)))
        (is (null continuation))))))

(test outer-evaluate-let-simple
  (with-fresh-stores
    (let ((limit 50)
          (a (num 1)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let ((a ,a))
                                           a))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 5 iterations))
        (is (eq result-expr (num 1)))
        (is (null continuation))))))

(test outer-evaluate-let-bug
  (with-fresh-stores
    (let ((limit 50)
          (a (num 1))
          (b (num 2)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let (   )
                                           (cons ,a ,b)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 7 iterations))
        (is (eq (cons (num 1) (num 2)) result-expr))
        (is (null continuation))))))

(test outer-evaluate-let
  (with-fresh-stores
    (let ((limit 50)
          (a (num 1))
          (b (num 2))
          (c (num 3)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let ((a ,a)
                                               (b ,b)
                                               (c ,c))
                                           ;; TOOD: Some variadic operators?
                                           (+ a (+ b c))))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 24 iterations))
        (is (eq result-expr (num 6)))
        (is (null continuation))))))

(test outer-evaluate-arithmetic
  (with-fresh-stores
    (let ((limit 50)
          (a (num 2))
          (b (num 3))
          (c (num 4)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          ;; TODO: Use let?
          (outer-evaluate (expression<- `((((lambda (x)
                                              (lambda (y)
                                                (lambda (z)
                                                  (* z
                                                     (+ x y)))))
                                            ,a)
                                           ,b)
                                          ,c))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 30 iterations))
        (is (eq (num 20) result-expr))
        (is (null continuation))))))

(test outer-evaluate-arithmetic-let-simplest
  (with-fresh-stores
    (let ((limit 20)
          (a (num 2)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          ;; Compare legibility with the original version from before LET was implemented
          ;; in OUTER-EVALUATE-ARITHMETIC
          (outer-evaluate (expression<- `(let ((x ,a))
                                           x))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        ;; NOTE, however, that this come swith extra iterations for now (two per
        ;; binding).
        ;;
        ;; Eventually, implement macros, make LET be one, and expand
        ;; during a one-time compilation phase. This will not help ad-hoc
        ;; evaluations, but if compiled functions are stored once in a
        ;; machine/store, then the cost won't be repeated on every call. In fact,
        ;; compilation can help even ad-hoc functions iff calls happen repeatedly
        ;; to the extent that compilation overhead is paid for by the savings.
        (is (= 5 iterations))
        (is (eq (num 2) result-expr))
        (is (null continuation))))))

(test outer-evaluate-arithmetic-let
  (with-fresh-stores
    (let ((limit 50)
          (a (num 2))
          (b (num 3))
          (c (num 4)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          ;; Compare legibility with the original version from before LET was implemented
          ;; in OUTER-EVALUATE-ARITHMETIC
          (outer-evaluate (expression<- `(let ((x ,a)
                                               (y ,b)
                                               (z ,c))
                                           (* z
                                              (+ x y))))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        ;; NOTE, however, that this come swith extra iterations for now (two per
        ;; binding).
        ;;
        ;; Eventually, implement macros, make LET be one, and expand
        ;; during a one-time compilation phase. This will not help ad-hoc
        ;; evaluations, but if compiled functions are stored once in a
        ;; machine/store, then the cost won't be repeated on every call. In fact,
        ;; compilation can help even ad-hoc functions iff calls happen repeatedly
        ;; to the extent that compilation overhead is paid for by the savings.
        (is (= 24 iterations))
        (is (eq (num 20) result-expr))
        (is (null continuation))))))

(test outer-evaluate-arithmetic-comparison
  (with-fresh-stores
    (let ((limit 40)
          (a (num 2))
          (b (num 3))
          (c (num 4))
          (d (num 20)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let ((x ,a)
                                               (y ,b)
                                               (z ,c))
                                           (= ,d (* z
                                                    (+ x y)))))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 28 iterations))
        (is (eq (sym t) result-expr))
        (is (null continuation))))))

;; If we are willing to let true and false values be functions, we can implement
;; conditionals from pure lambda calculus. In that case, we could remove special
;; handling for (SYM T) and (NILL).
(test outer-evaluate-fundamental-conditional
  (with-fresh-stores
    (let ((limit 100)
          (x (num 5))
          (y (num 6)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let ((true (lambda (a)
                                                       (lambda (b)
                                                         a)))
                                               (false (lambda (a)
                                                        (lambda (b)
                                                          b)))
                                               ;; NOTE: We cannot shadow IF because it is built-in.
                                               (if- (lambda (a)
                                                     (lambda (c)
                                                       (lambda (cond)
                                                         ((cond a) c))))))
                                           (((if- ,x) ,y) true)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 46 iterations))
        (is (eq x result-expr))
        (is (null continuation)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let ((true (lambda (a)
                                                       (lambda (b)
                                                         a)))
                                               (false (lambda (a)
                                                        (lambda (b)
                                                          b)))
                                               (if- (lambda (a)
                                                     (lambda (b)
                                                       (lambda (cond)
                                                         ((cond a) b))))))
                                           (((if- ,x) ,y) false)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 43 iterations))
        (is (eq y result-expr))
        (is (null continuation))))))

(test outer-evaluate-fundamental-conditional-bug
  (with-fresh-stores
    (let ((limit 100)
          (x (num 5))
          (y (num 6)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let ((true (lambda (a)
                                                       (lambda (b)
                                                         a)))
                                               (if- (lambda (a)
                                                     (lambda (c)
                                                       (lambda (cond)
                                                         ((cond a) c))))))
                                           (((if- ,x) ,y) true)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 42 iterations))
        (is (eq x result-expr))
        (is (null continuation))))))

#|
Digression about Lisp-1 vs Lisp-2

Would we rather (Lisp-1)

(let ((true (lambda (a)
              (lambda (b)
                a)))
      (false (lambda (a)
               (lambda (b)
                 b)))
      (if (lambda (a)
            (lambda (b)
              (lambda (cond)
                ((cond a) b))))))
  (((if ,x) ,y) true))

Or?

(let ((true (lambda (a)
              (lambda (b)
                a)))
      (false (lambda (a)
               (lambda (b)
                 b)))
      (if (lambda (a)
            (lambda (b)
              (lambda (cond)
                (funcall (funcall cond a) b))))))
  (((if ,x) ,y) true))

Is there any coherent model in which we support both -- perhaps as distinct
syntaxes? This seems problematic, but there are some benefits of each,
especially considering the efficiency benefit of terseness in the setting of a
circuit-based evaluator.

|#

(test outer-evaluate-if
  (let ((limit 100)
        (x (num 5))
        (y (num 6)))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(if ,(sym t) ,x ,y))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 4 iterations))
      (is (eq x result-expr))
      (is (null continuation)))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(if ,(nill) ,x ,y))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 4 iterations))
      (is (eq y result-expr))
      (is (null continuation)))))

(test outer-evaluate-fully-evaluates
  (let ((limit 100)
        (x (num 5))
        (y (num 6)))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(if ,(sym t) (+ ,x ,x) ,y))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 8 iterations))
      (is (eq (num 10) result-expr))
      (is (null continuation)))))

(defvar *limit* 1000)
(defun evaluate (form &key (limit *limit*))
  (outer-evaluate (expression<- form) (empty-sym-env) :limit limit))

(test outer-evaluate-recursion
  (with-fresh-stores
    (let ((limit 200)
          (zero (num 0))
          (one (num 1))
          (base (num 5))
          (exponent (num 3)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(letrec* ((exp (lambda (base)
                                                          (lambda (exponent)
                                                            (if (= ,zero exponent)
                                                                ,one
                                                                (* base ((exp base) (- exponent ,one))))))))
                                                  ((exp ,base) ,exponent)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        ;; FIXME: Make all of these be formal tests so they don't have to be
        ;; kept in sync manually.
        ;;
        ;; EXPONENT => (ITERATIONS, STORE-SIZE)
        ;; 1 => (51, 81)
        ;; 2 => (80, 81)
        ;; 3 => (126, 81)
        ;; 5 => (167, 81)
        ;; 7 => (225, 81)
        ;; 9 => (283, 81)
        ;; 11 => (341, 81)
        ;; 100 => (2922, 325)
        ;; 1000 => (29022, 2610)
        ;; ITERATIONS = BASE * 29 + 22
        (is (= 118 iterations))
        (is (= 81 (store-size *content-store*)))
        (is (eq (num (expt (num-value base) (num-value exponent)))
                result-expr))
        (is (null continuation))))))

(test outer-evaluate-recursion-multiarg
  (with-fresh-stores
    (let ((limit 200)
          (zero (num 0))
          (one (num 1))
          (base (num 5))
          (exponent (num 3)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(letrec* ((exp (lambda (base exponent)
                                                          (if (= ,zero exponent)
                                                              ,one
                                                              (* base (exp base (- exponent ,one)))))))
                                                  (exp ,base ,exponent)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        ;; NOTE that the multiarg call sugar incurs a cost of one iteration
        ;; (presumably per arg) in the current implementation. Maybe there is a
        ;; way to eliminate. Alternately: we could introduce a reader syntax.
        ;; This would require authors to be aware of the issue (ugly). Or, it
        ;; could be an automatic transformation made by a preprocessor
        ;; (sub-optimal for this to be required for the base language). Or, when
        ;; we introduce compilation, this expansion can happen only once (fine).
        ;;
        ;; EXPONENT => (ITERATIONS, STORE-SIZE)
        ;; 1 => (53, 81)
        ;; 2 => (83, 81)
        ;; 3 => (130, 81)
        ;; 5 => (173, 81)
        ;; 7 => (233, 81)
        ;; 9 => (293, 162)
        ;; 11 => (353, 108)
        ;; 100 => (3023, 325)
        ;; 1000 => (30023, 2610)
        ;; ITERATIONS = BASE * 30 + 23
        (is (= 122 iterations))
        (is (= 81 (store-size *content-store*)))
        (is (eq (num (expt (num-value base) (num-value exponent)))
                result-expr))
        (is (null continuation))))))

(test outer-evaluate-recursion-optimized
  (with-fresh-stores
    (let ((limit 300)
          (zero (num 0))
          (one (num 1))
          (base (num 5))
          (exponent (num 3)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(let ((exp (lambda (base)
                                                      (letrec* ((base-inner
                                                                 (lambda (exponent)
                                                                   (if (= ,zero exponent)
                                                                       ,one
                                                                       (* base (base-inner (- exponent ,one)))))))
                                                               base-inner))))
                                           ((exp ,base) ,exponent)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        ;; EXPONENT => (ITERATIONS, STORE-SIZE)
        ;; 1 => (48, 81)
        ;; 2 => (71, 81)
        ;; 3 => (105, 81)
        ;; 5 => (140, 81)
        ;; 7 => (186, 81)
        ;; 9 => (232, 108)
        ;; 11 => (278, 108)
        ;; 100 => (2325, 325)
        ;; 1000 => (23025, 2610)
        ;; ITERATIONS = BASE * 23 + 25
        (is (= 100 iterations))
        (is (= 81 (store-size *content-store*)))
        (is (eq (num (expt (num-value base) (num-value exponent)))
                result-expr))
        (is (null continuation))))))

(test outer-evaluate-tail-recursion
  (with-fresh-stores
    (let ((limit 300000)
          (zero (num 0))
          (one (num 1))
          (base (num 5))
          (exponent (num 3)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(letrec* ((exp (lambda (base)
                                                          (lambda (exponent-remaining)
                                                            (lambda (acc)
                                                              (if (= ,zero exponent-remaining)
                                                                  acc
                                                                  (((exp base) (- exponent-remaining ,one)) (* acc base))))))))
                                                  (((exp ,base) ,exponent) ,one)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        ;; EXPONENT => (ITERATIONS, STORE-SIZE)
        ;; 1 => (67, 81)
        ;; 2 => (106, 81)
        ;; 3 => (173, 81)
        ;; 5 => (223, 108)
        ;; 7 => (301, 108)
        ;; 9 => (379, 108)
        ;; 11 => (457, 162)
        ;; 100 => (3928, 652)
        ;; 1000 => (39028, 5221)
        ;; ITERATIONS = BASE * 39 + 28
        (is (= 161 iterations))
        (is (= 81 (store-size *content-store*)))
        (is (eq (num (expt (num-value base) (num-value exponent)))
                result-expr))
        (is (null continuation))))))

;; NOTE that these attempts to be properly tail-recursive don't yet show any
;; benefit. TODO: Understand whether we're missing an opportunity to optimize.
;; It seems this should be possible.
(test outer-evaluate-tail-recursion-somewhat-optimized
  (with-fresh-stores
    (let ((limit 50000)
          (zero (num 0))
          (one (num 1))
          (base (num 5))
          (exponent (num 3)))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate
           (expression<-
            `(letrec* ((exp (lambda (base)
                         (letrec* ((base-inner
                                    (lambda (exponent-remaining)
                                      (lambda (acc)
                                        (if (= ,zero exponent-remaining)
                                            acc
                                            ((base-inner (- exponent-remaining ,one)) (* acc base)))))))
                                   base-inner))))
               (((exp ,base) ,exponent) ,one)))
           (empty-sym-env)
           :limit limit)
        (declare (ignore new-env))
        ;; EXPONENT => (ITERATIONS, STORE-SIZE)
        ;; 1 => (63, 108)
        ;; 2 => (95, 108)
        ;; 3 => (149, 108)
        ;; 5 => (191, 108)
        ;; 7 => (255, 108)
        ;; 9 => (319, 162)
        ;; 11 => (383, 162)
        ;; 100 => (3231, 652)
        ;; 1000 => (32031, 5221)
        ;; ITERATIONS = BASE * 32 + 31
        (is (= 140 iterations))
        (is (= 108 (store-size *content-store*)))
        (is (eq (num (expt (num-value base) (num-value exponent)))
                result-expr))
        (is (null continuation))))))

(test outer-evaluate-no-mutual-recursion
  (with-fresh-stores
    (let ((limit 50))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(letrec* ((even (lambda (n)
                                                           (if (= 0 n)
                                                               t
                                                               (odd (- n 1)))))
                                                   (odd (lambda (n)
                                                          (even (- n 1)))))
                                                  ;; NOTE: This is not true mutual-recursion. However, it exercises the
                                                  ;; behavior of LETREC*.
                                                  (odd 1)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 32 iterations))
        (is (eq (sym t) result-expr))
        (is (null continuation)))


      (signals simple-error
          (outer-evaluate (expression<- `(letrec* ((even (lambda (n)
                                                           (if (= 0 n)
                                                               t
                                                               (odd (- n 1)))))
                                                   (odd (lambda (n)
                                                          (even (- n 1)))))
                                                  ;; This requires mutual recursion, which LETREC* does not provide.
                                                  ;; To make this work, implement LABELS/LETREC, which will require
                                                  ;; a new LOOKUP-RECURSIVE-CONTINUATION, which remembers the current
                                                  ;; recursive env, so functions defined later in the binding order
                                                  ;; can be found within the bodies of those defined earlier.
                                                  (odd 2)))
                          (empty-sym-env)
                          :limit limit)))))

(test outer-evaluate-let-scope
  (let ((limit 30)
        (n (num 1)))
    (signals simple-error
      (outer-evaluate (expression<- `(let ((closure (lambda (x)
                                                      ;; This use of CLOSURE is unbound.
                                                      closure)))
                                       (closure ,n)))
                                   (empty-sym-env)
                                   :limit limit))))

(test outer-evaluate-let-scope2
  (let ((limit 30)
        (n (num 1)))
    (signals simple-error
      (outer-evaluate (expression<- `(let ((a (let ((b 123))
                                                b)))
                                       ;; B should be out of scope here.
                                       b))
                                   (empty-sym-env)
                                   :limit limit))))
(test outer-evaluate-let-no-body
  (let ((limit 30)
        (n (num 1)))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(let ((a 9))))
                        (empty-sym-env)
                        :limit limit)
      (is (= 5 iterations))
      (is (hnull result-expr))
      (is (null continuation))
      )))

(test outer-evaluate-cons
  (let ((limit 10))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(car (cons 1 2)))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 7 iterations))
      (is (eq (num 1) result-expr))
      (is (null continuation)))

    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(cdr (cons 1 2)))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 7 iterations))
      (is (eq (num 2) result-expr))
      (is (null continuation)))))

(test outer-evaluate-cons-in-function
  (let ((limit 100))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(((lambda (a)
                                           (lambda (b)
                                             (car (cons a b))))
                                         2)
                                        3))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 20 iterations))
      (is (eq (num 2) result-expr))
      (is (null continuation)))

    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(((lambda (a)
                                           (lambda (b)
                                             (cdr (cons a b))))
                                         2)
                                        3))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 20 iterations))
      (is (eq (num 3) result-expr))
      (is (null continuation)))))

(test multiarg-eval-bug
  ;; The buggy behavior is that returns an error because it tries to evaluate '(2 3 4)
  ;; as a function call and 2 is not a valid function.
  (is (eq (sym 2) (evaluate '(car (cdr '(1 2 3 4)))))))

(test outer-evaluate-zero-arg-lambda
  (let ((limit 20))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `((lambda () 123)))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 4 iterations))
      (is (eq (num 123) result-expr))
      (is (null continuation)))
    (multiple-value-bind (result-expr new-env iterations continuation)
        (outer-evaluate (expression<- `(let ((x 9) (f (lambda () (+ x 1)))) (f)))
                        (empty-sym-env)
                        :limit limit)
      (declare (ignore new-env))
      (is (= 17 iterations))
      (is (eq (num 10) result-expr))
      (is (null continuation)))))

(test minimal-tail-call
  (with-fresh-stores
    (let ((limit 10000))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(letrec*
                                          ((f (lambda (x)
                                                (if (= x 140)
                                                    123
                                                    (f (+ x 1))))))
                                          (f 0)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (eq (num 123) result-expr))
        (is (null continuation))
        ))))

(test multiple-letrec*-bindings
  (with-fresh-stores
    (let ((limit 10000))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(letrec*
                                          ((x 888)
                                           (f (lambda (x)
                                                (if (= x 5)
                                                    123
                                                    (f (+ x 1))))))
                                          (f 0)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 110 iterations))
        (is (eq (num 123) result-expr))
        (is (null continuation))))))

(test tail-call2
  (with-fresh-stores
    (let ((limit 10000))
      (multiple-value-bind (result-expr new-env iterations continuation)
          (outer-evaluate (expression<- `(letrec*
                                          ((f (lambda (x)
                                                (if (= x 5)
                                                    123
                                                    (f (+ x 1)))))
                                           (g (lambda (x) (f x))))
                                          (g 0)))
                          (empty-sym-env)
                          :limit limit)
        (declare (ignore new-env))
        (is (= 118 iterations))
        (is (eq (num 123) result-expr))
        (is (null continuation))
        ))))

(test let-restore-saved-env
  (with-fresh-stores
    (let ((limit 100))
      (signals simple-error
        (outer-evaluate (expression<- '(+ (let ((a 1)) a) a))
                        (empty-sym-env)
                        :limit limit)))))

(test let-restore-saved-env2
  (with-fresh-stores
    (let ((limit 100))
      (signals simple-error
        (outer-evaluate (expression<- '(+ (let ((a 1) (a 2)) a) a))
                        (empty-sym-env)
                        :limit limit)))))

(test letrec*-restore-saved-env
  (with-fresh-stores
    (let ((limit 100))
      (signals simple-error
        (outer-evaluate (expression<- '(+ (letrec* ((a 1)(a 2)) a) a))
                        (empty-sym-env)
                        :limit limit)))))

(test lookup-restore-saved-env
  (with-fresh-stores
    (let ((limit 100))
      (signals simple-error
        (outer-evaluate (expression<- '(+ (let ((a 1))
                                            a)
                                        a))
                        (empty-sym-env)
                        :limit limit)))))

(test tail-call-restore-saved-env
  (with-fresh-stores
    (let ((limit 100))
      (signals simple-error
        (outer-evaluate (expression<- `(let ((outer (letrec*
                                                     ((x 888)
                                                      (f (lambda (x)
                                                           (if (= x 2)
                                                               123
                                                               (f (+ x 1))))))
                                                     f)))
                                         ;; This should be an error. X should not be bound here.
                                        (+ (outer 0) x)))
                        (empty-sym-env)
                        :limit limit)))))

(test binop-restore-saved-env
  (with-fresh-stores
    (let ((limit 10000))
      (signals simple-error
        (outer-evaluate (expression<- `(let ((outer (let
                                                     ((f (lambda (x)
                                                           (+ (let ((a 9)) a) x))))
                                                      f)))
                                         ;; This should be an error. X should not be bound here.
                                        (+ (outer 1) x)))
                        (empty-sym-env)
                        :limit limit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(def-suite* interface-suite :in lang-suite)

(defun make-library () (make-hash-table))
(defvar *library* (make-library))

(defmacro with-library ((library) &body body)
  `(let ((*library* ,library))
     ,@body))

(defmacro def-lib-fun (name (&rest  args) &body body)
  (let ((definition `(lambda (,@args) ,@body)))
    `(setf (gethash ',name *library*)
           ',definition)))

(defun install (function-name env &key (library *library*))
  (let ((definition (expression<- (gethash function-name library)))
        (key (sym function-name)))
    ;; FIXME: This fails because definition needs to be evaluated.
    ;; Library creation therefore needs to be explicit.
    ;; Run a first-class program to enhance an environment.
    (extend env key definition)))

(defun expand-with-definition (function-name &rest body)
  (let ((definition (gethash function-name *library*))
        (key function-name))
    `(letrec* ((,key ,definition))
              ,@body)))

;; Verify this works.
(defun pos (key set)
  (labels ((p-aux (key set pos)
             (if (null set)
                 nil
                 (if (eq key (car set))
                     pos
                     (p-aux key (cdr set) (+ 1 pos))))))
    (p-aux key set 0)))

(test reference-pos
  (is (= 3 (pos 4 '(1 2 3 4)))))

(defun create-demo-library ()
  (let ((library (make-library)))
    (with-library (library)
      (def-lib-fun position (key set)
        (letrec* ((p-aux (lambda (key set pos)
                           (if (eq nil set)
                               nil
                               (if (eq key (car set))
                                   pos
                                   (p-aux key (cdr set) (+ 1 pos)))))))
                 (p-aux key set 0)))

      *library*)))

(test demo-library
  (with-fresh-stores
    (let* ((limit 1000)
           (demo-library (create-demo-library)))
      (with-library (demo-library)
        (multiple-value-bind (result-expr new-env iterations continuation)
            (outer-evaluate (expression<- (expand-with-definition 'position '(position 4 '(1 2 3 4))))
                            (empty-sym-env)
                            :limit limit)
          (declare (ignore new-env))
          (is (= 229 iterations))
          (is (eq (num 3) result-expr))
          (is (null continuation))))
      (with-library (demo-library)
        (multiple-value-bind (result-expr new-env iterations continuation)
            (outer-evaluate (expression<- (expand-with-definition 'position '(position 5 '(1 2 3 4))))
                            (empty-sym-env)
                            :limit limit)
          (declare (ignore new-env))
          (is (= 270 iterations))
          (is (eq (nill) result-expr))
          (is (null continuation)))))))

(in-suite lang-suite)


;; End interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(test types
  (is (subtypep 'hcons 'expression))
  (is (subtypep 'sym 'expression))
  (is (not (subtypep 'expression 'hcons)))
  (let ((c (cons 1 2)))
    (is (typep c 'hcons))
    (is (typep c 'expression))
    (is (not (typep c 'sym))))
  (let ((s (sym 'aardavark)))
    (is (typep s 'sym))
    (is (typep s 'expression))
    (is (not (typep s 'hcons)))))

(test variants
  (is (variant-p 'hcons 'expression))
  (is (variant-p 'sym 'expression))
  (is (variant-p 'num 'expression))
  (is (not (variant-p 'vector 'expression))))

(test vcase
  (with-fresh-stores
    (signals error (macroexpand '(vcase ((sym 'x) expression)
                                  (sym 123))))
    (signals error (macroexpand '(vcase ((sym 'x) expression)
                                  (sym 123)
                                  (hcons 321))))
    (finishes (macroexpand '(vcase ((sym 'x) expression)
                             (nill 0)
                             (sym 123)
                             (hcons 321)
                             (num 999)
                             (fun 888)
                             (cont 456))))
    (is (= 123 (vcase ((sym 'x) expression)
                 (nill 0)
                 (sym 123)
                 (hcons 321)
                 (num 999)
                 (fun 888)
                 (cont 456))))
    (is (= 321 (vcase ((cons 'x 'y) expression)
                 (nill 0)
                 (sym 123)
                 (hcons 321)
                 (num 999)
                 (fun 888)
                 (cont 456))))
    (is (= 999 (vcase ((num 888) expression)
                 (nill 0)
                 (sym 123)
                 (hcons 321)
                 (num 999)
                 (fun 888)
                 (cont 456))))
    (is (= 888 (vcase ((fun (make-fn)) expression)
                 (nill 0)
                 (sym 123)
                 (hcons 321)
                 (num 999)
                 (fun 888)
                 (cont 456))))
    (is (= 0 (vcase ((make-nill) expression)
               (nill 0)
               (sym 123)
               (hcons 321)
               (num 999)
               (fun 888)
               (cont 456)))
    (is (= 456 (vcase ((cont (make-instance 'continuation)) expression)
               (nill 0)
               (sym 123)
               (hcons 321)
               (num 999)
               (fun 888)
               (cont 456)
               ))))))

(test cons
  (with-fresh-stores
    (let ((x (cons 1 2))
          (y (cons 1 2)))
      (is (eq x y))
      (is (eql (expression-type-name (find-expression-type (expression-type x))) 'hcons))
      (is (= (expression-type-tag (find-expression-type (expression-type x))) 1))
      )))

(test sym
  (with-fresh-stores
    (let ((x (sym 'asdf))
          (y (sym 'asdf)))
      (is (eq x y))
      (is (eql (expression-type-name (find-expression-type (expression-type x))) 'sym))
      (is (= (expression-type-tag (find-expression-type (expression-type x))) 2)))))

(test compound
  (with-fresh-stores
    (let ((x (cons (cons 1 2) (cons 3 4)))
          (y (cons (cons 1 2) (cons 3 4)))
          (z (cons '(1 . 2) '(3 . 4))))
      (is (eq x y))
      (is (eq x z))
      (is (eq z (canonicalize z *content-store*))))))
