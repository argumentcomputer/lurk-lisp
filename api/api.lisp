(in-package lurk.api.impl)
(def-suite* api-impl-suite :in lurk:master-suite)

(defconstant api:t 'api:t)

(defun emit-out (out v)
  (let ((*print-circle* t))
    (format out "~S~%" v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Subsets

(defclass subset ()
  ((package :initarg :package :reader subset-package)))

(defmethod print-object ((subset subset) (stream t))
  (print-unreadable-object (subset stream :type t)))

;; Not actually implemented yet.
(defclass min-subset (subset)
  ;; Will need its own package.
  ((package :initform (find-package :lurk.api))))

(defclass core-subset (subset)
  ((package :initform (find-package :lurk.api))))

(defclass ram-subset (subset)
  ((package :initform (find-package :lurk.api.ram))))

(defparameter *subsets* ())

(defun find-subset (name) (find (find-class name) *subsets* :key #'class-of))

(defun intern-subset (name)
  (or (find-subset name)
      (let ((subset (make-instance (find-class name))))
        (pushnew subset *subsets*)
        subset)))

(defgeneric directly-contains (subset)
  (:method ((subset t)) '())
  (:method ((subset ram-subset)) (list (intern-subset 'core-subset)))
  (:method ((subset core-subset)) (list (intern-subset 'min-subset))))

;;; True if B is a (non-strict) subset of A.
(defgeneric contains-p (a b)
  (:method ((a t) (b t))
    (or (eql (class-of a) (class-of b))
        (member b (directly-contains a))
        (some (lambda (x) (contains-p x b))
              (directly-contains a)))))

(test contains-p
  (is (not (null (contains-p (intern-subset 'ram-subset) (intern-subset 'core-subset)))))
  (is (not (null (contains-p (intern-subset 'ram-subset) (intern-subset 'min-subset)))))
  (is (not (null (contains-p (intern-subset 'core-subset) (intern-subset 'min-subset)))))

  ;; Subsets contain themselves.
  (is (not (null (contains-p (intern-subset 'ram-subset) (intern-subset 'ram-subset)))))
  (is (not (null (contains-p (intern-subset 'core-subset) (intern-subset 'core-subset)))))
  (is (not (null (contains-p (intern-subset 'min-subset) (intern-subset 'min-subset)))))

  (is (null (contains-p (intern-subset 'core-subset) (intern-subset 'ram-subset))))
  (is (null (contains-p (intern-subset 'min-subset) (intern-subset 'ram-subset))))
  (is (null (contains-p (intern-subset 'min-subset) (intern-subset 'core-subset)))))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct closure env function)

(defun* (extend-closure -> closure) ((c closure) (rec-env env))
  (let ((extended (hcons rec-env (closure-env c))))
    (make-closure :env extended :function (closure-function c))))

;; The following types loosely specify the language which EXPR allows.

;; A FIELD-ELEMENT is a natural number below P, where P must be prime. If P is
;; unsupplied, then no upper bound is placed on the value.
(deftype field-element (&optional p) (if (or (not p) (eql p '*))
                                         `(integer 0)
                                         `(integer 0 ,(- p 1))))

;; An ATOM is a FIELD-ELEMENT, SYMBOL, FUNCTION, or NIL.
(deftype atom (&optional p) `(or (field-element ,p) symbol nil function closure character string))

;; Since it is awkward to express both the constraint on field size and the
;; recursive type in the COMMON LISP type system, we provide a second type,
;; EXPR*.
(defun expr-p (x)
  (typecase x
    (cons (and (expr-p (car x)) (expr-p (cdr x))))
    (t (typep x 'atom))))

;; Although there are still unhandled edge cases (e.g. function signatures are
;; unrestricted by this type), the intent is that EXPR is a tree of CONS cells
;; terminating in ATOMs. EXPRs need not be balanced or homogenous in shape. A
;; single ATOM is a zero-height tree.
(deftype expr () `(satisfies expr-p))

(test atom
  (let* ((p 11)
         (type `(atom ,p)))
    (is (equal '(atom 11) type))
    (is (not (null (typep 7 type))))))

(deftype env () 'list)

(defstruct ram defs macros)

(deftype built-in-unary () '(member api:atom api:car api:cdr api:emit api:quote api:macroexpand))
(deftype built-in-binary () '(member api:+ api:- api:/ api:* api:= api:eq api:cons api:strcons))
(deftype self-evaluating-symbol () '(member api:nil api:t))

(defvar *cons-table* (make-hash-table :test #'equal))

(defun* (hcons -> cons) ((car expr) (cdr expr))
  (let ((cons (cons car cdr)))
    (multiple-value-bind (existing present-p)
        (gethash cons *cons-table*)
      (if present-p
          existing
          (setf (gethash cons *cons-table*) cons)))))

(defun* (hlist* -> list) (exprs)
  (if exprs
      (hcons (car exprs) (hlist* (cdr exprs)))
      nil))

(defun* (hlist -> list) (&rest exprs)
  (hlist* exprs))

(defun* (is-macro-in-ram -> boolean) ((expr expr) (ram ram))
  (and (symbolp expr)
       (multiple-value-bind (result found-p)
           (lookup-find expr (ram-macros ram))
         found-p)))

(defun* (nest-apps -> expr) ((xs expr))
  (if (or (null xs) (null (cdr xs)) (null (cddr xs)))
      xs
      (nest-apps (cons (list (car xs) (cadr xs)) (cddr xs)))))

(defun* (macro-expand-macro -> expr) ((expr expr) (ram ram))
  (let ((closure (lookup (car expr) (ram-macros ram))))
    (etypecase closure
      (closure (let* ((args (cdr expr))
                      (quoted-args (mapcar (lambda (unevaled) `(quote ,unevaled)) args))
                      (result (apply (closure-function closure) ram (closure-env closure) quoted-args)))
                 result)))))

(defun* (macro-expand-for-p -> expr) ((p integer) (expr expr) (ram ram))
  (labels ((macro-expand (expr)
             (macro-expand-for-p p expr ram))
           (is-macro (expr)
             (is-macro-in-ram expr ram)))
    (etypecase expr
      ((or closure self-evaluating-symbol) expr)
      (symbol expr)
      (atom (unless (typep expr `(atom ,p))
              (error "~S is out of range [0, ~S)." expr p))
       expr)
      (list
       (destructuring-bind (head &rest rest) expr
         (etypecase head
           (closure expr)
           ((eql api.ram:define)
            (destructuring-bind (var rhs) rest
              `(api.ram:define ,var ,(macro-expand rhs))))
           ((eql api.ram:defmacro)
            (destructuring-bind (var params body) rest
              `(api.ram:defmacro ,var ,params ,(macro-expand body))))
           ((eql api:let)
            (destructuring-bind (bindings body-expr) rest
              `(api:let
                   ,(mapcar #'(lambda (b)
                                (assert (eq nil (cddr b)))
                                `(,(car b) ,(macro-expand (cadr b))))
                     bindings)
                 ,(macro-expand body-expr))))
           ((eql api:letrec)
            (destructuring-bind (bindings body-expr) rest
              `(api:letrec
                ,(mapcar #'(lambda (b)
                             (assert (eq nil (cddr b)))
                             `(,(car b) ,(macro-expand (cadr b))))
                         bindings)
                ,(macro-expand body-expr))))
           ((eql api:lambda)
            (destructuring-bind (args body-expr) rest
              (if (or (null args) (null (cdr args)))
                  `(api:lambda ,args ,(macro-expand body-expr))
                  `(api:lambda (,(car args)) ,(macro-expand `(api:lambda ,(cdr args) ,body-expr)))
                  )))
           ((eql api:if)
            (destructuring-bind (condition a b) rest
              `(api:if ,(macro-expand condition) ,(macro-expand a) ,(macro-expand b))))
           ((eql api:current-env)
            (assert (eq nil rest))
            expr)
           ((eql api.ram:current-ram)
            (assert (eq nil rest))
            expr)
           ((eql api:eval)
            `(api:eval ,@(mapcar #'macro-expand rest)))
           ((eql api:begin)
            `(api:begin ,@(mapcar #'macro-expand rest)))
           (built-in-unary
            (destructuring-bind (arg) rest
              (case head
                (api:quote expr)
                (t `(,head ,(macro-expand arg))))))
           (built-in-binary
            (destructuring-bind (a b) rest
              `(,head ,(macro-expand a) ,(macro-expand b))))
           (t
            (if (is-macro head)
                (macro-expand (macro-expand-macro expr ram))
                (nest-apps (mapcar #'macro-expand expr))))))))))

(defparameter *emitted* :uninitialized)

(defmacro with-emission-captured (&body body)
  `(let ((*emitted* (if :uninitialized
                        ()
                        *emitted*)))
     (multiple-value-bind (new-expr new-env new-ram)
         (progn ,@body)
       (values new-expr new-env new-ram (nreverse *emitted*)))))

(defun* (eval-expr-for-p -> (values expr env ram)) ((p integer) (expr expr) (env env) (ram ram))
  (with-emission-captured
    (inner-eval-expr-for-p p expr env ram)))

(defun* (inner-eval-expr-for-p -> (values expr env ram)) ((p integer) (expr expr) (env env) (ram ram))
  (labels ((eval-expr (expr env)
             (inner-eval-expr-for-p p expr env ram))
           (apply-closure (closure args env)
             (let* ((evaled-args (mapcar (lambda (x) (eval-expr x env)) args))
                    (quoted-args (mapcar (lambda (evaled) `(quote ,evaled)) evaled-args)))
               (values (apply (closure-function closure) ram (closure-env closure) quoted-args) env ram))))
    (etypecase expr
      ((or closure self-evaluating-symbol) (values expr env ram))
      (symbol
       (multiple-value-bind (v found-p)
           (lookup-find expr env)
         (values
          (if found-p
              v
              (lookup expr (ram-defs ram)))
          env ram)))
      (atom (unless (typep expr `(atom ,p))
              (error "~S is out of range [0, ~S)." expr p))
       (values expr env ram))

      (list
       (destructuring-bind (head &rest rest) expr
         (etypecase head
           (closure (apply-closure head env rest))
           ((eql api.ram:define)
            (destructuring-bind (var rhs) rest
              (let ((val (eval-expr rhs env)))
                (values var env (extend-ram-defs ram var val)))))
           ((eql api.ram:defmacro)
            (destructuring-bind (var params body) rest
              (let* ((rhs `(api:lambda ,params ,body))
                     (val (eval-expr rhs env)))
                (values var env (extend-ram-macros ram var val)))))
           ((eql api:let)
            (destructuring-bind (bindings body-expr) rest
              (let ((new-env env))
                (loop for (var val) in bindings
                      ;; Evaluate VAL in NEW-ENV
                      for evaled = (eval-expr val new-env)
                      do (setq new-env (extend new-env var evaled)))
                (eval-expr body-expr new-env))))
           ((eql api:letrec)
            (destructuring-bind (bindings body-expr) rest
              (let ((new-env env))
                (loop for (var val) in bindings
                      ;; Evaluate VAL in NEW-ENV
                      for evaled = (eval-expr val new-env)
                      do (setq new-env (extend-rec new-env var evaled)))
                (eval-expr body-expr new-env))))
           ((eql api:lambda)
            (destructuring-bind (args body-expr) rest
              ;; Since there are currently no side-effects, supporting multiple
              ;; expressions in the body would be pointless. Therefore, for now,
              ;; only one expression is permitted rather than allow the
              ;; confusing possibility of wastefully including some ignored
              ;; first expressions in the body.
              (let* ((env-var (gensym "ENV"))
                     (ram-var (gensym "RAM"))
                     (source `(lambda (,ram-var ,env-var ,@args)
                                (inner-eval-expr-for-p ,p
                                                 ;; Close your eyes and believe.
                                                 `(api:let (,,@(mapcar (lambda (arg) `(list ',arg ,arg)) args))
                                                    ,',body-expr)
                                                 ,env-var
                                                 ,ram-var))))
                (values (make-closure :env env :function (compile nil source)) env ram))))
           ((eql api:if)
            (destructuring-bind (condition a b) rest
              (let ((result (if (eval-expr condition env)
                                (eval-expr a env)
                                (eval-expr b env))))
                (values result env ram))))
           #+(or) ;; Disabled initially, since variadic functions aren't simple in other implementations.
           ((eql api:list)
            (values (hlist* (mapcar (lambda (x) (eval-expr x env)) rest))
                    env
                    ram))
           ((eql api:current-env) (values env env ram))
           ((eql api.ram:current-ram) (values ram env ram))
           ((eql api:eval)
            (let ((ev-expr (macro-expand-for-p p (eval-expr (car rest) env) ram))
                  (ev-env (if (null (cdr rest)) (empty-env) (eval-expr (car (cdr rest)) env))))
              (eval-expr ev-expr ev-env)))
           ((eql api:begin)
            (if (null (cdr rest))
                (eval-expr (car rest) env)
                (multiple-value-bind (ignored-val new-env new-ram)
                    (eval-expr (car rest) env)
                  ;; specifically eval the rest with the new ram,
                  ;; but NOT the new env
                  (inner-eval-expr-for-p p `(api:begin ,@(cdr rest)) env new-ram))))
           (built-in-unary
            (destructuring-bind (arg) rest
              (let ((result (ecase head
                              (api:atom
                               (typecase (eval-expr arg env)
                                 (atom api:t)
                                 (t api:nil)))
                              (api:car (let ((v (eval-expr arg env)))
                                         (if (typep v 'string)
                                             (if (equal "" v)
                                                 nil
                                                 (char v 0))
                                             (car v))))
                              (api:cdr (let ((v (eval-expr arg env)))
                                         (if (typep v 'string)
                                             (if (equal "" v)
                                                 ""
                                                 (subseq v 1))
                                             (cdr v))))
                              (api:emit (let ((v (eval-expr arg env)))
                                          (push v *emitted*)
                                          (emit-out t v)
                                          v))
                              (api:quote (quote-expr arg))
                              (api:macroexpand (quote-expr (macro-expand-for-p p (eval-expr arg env) ram))))))
                (values result env ram))))
           (built-in-binary
            (destructuring-bind (a b) rest
              (let* ((evaled-a (eval-expr a env))
                     (evaled-b (eval-expr b env))
                     (result (ecase head
                               (api:+ (mod (+ evaled-a evaled-b) p))
                               (api:- (mod (- evaled-a evaled-b) p))
                               (api:* (mod (* evaled-a evaled-b) p))
                               (api:/ (assert (not (zerop evaled-b)) (evaled-b) "Cannot divide ~S by 0." evaled-a)
                                (mod (* evaled-a (inverse evaled-b p)) p))
                               (api:= (if (= evaled-a evaled-b) api:t api:nil))
                               (api:eq (if (equal evaled-a evaled-b) api:t api:nil))
                               (api:strcons (if (and (typep evaled-a 'character)
                                                     (typep evaled-b 'string))
                                                (concatenate 'string (string evaled-a) evaled-b)
                                                (error "Wrong type arguments for STRCONS: ~S" (list evaled-a evaled-b))))
                               (api:cons (hcons evaled-a evaled-b))
                               )))
                (values result env ram))))
           (t
            ;; (fn . args)
            ;; First evaluate FN, then substitue result in new expression to evaluate.
            (let ((evaled (eval-expr head env)))
              (etypecase evaled
                (closure (apply-closure evaled rest env)))))))))))

;; TODO: Make the cons store an explicit argument here and of CONS.
;; Returns a value EQUAL to EXPR, but with all CONS subexpressions
;; canonicalized via HCONS.
(defun quote-expr (expr)
  (typecase expr
    (cons (hcons (quote-expr (car expr)) (quote-expr (cdr expr))))
    (t expr)))

(defun* (make-evaluator -> function) ((p (integer 0)))
  (lambda (expr env ram)
    (let ((r (macro-expand-for-p p expr ram)))
      (eval-expr-for-p p r env ram))))

(defun* (lookup -> (values expr boolean)) ((var symbol) (env env))
  (multiple-value-bind (result found-p)
      (lookup-find var env)
      (if found-p
          result
          (error "Unbound var: ~S" var))))

;; Looks up VAR in ENV and returns three values:
;; 1. The bound value, if any.
;; 2. A boolean indicating whether a binding was found.
;; 3. The immediately enclosing environment, if a binding was found.
(defun* (lookup-find -> (values expr boolean)) ((var symbol) (env env))
  (if (endp env)
      (values nil nil nil)
      (destructuring-bind (key . val)
          (car env)
        (etypecase key
          (list
           ;; If KEY is a list, then (CAR ENV) is a recursive env.
           (multiple-value-bind (result found-p in-env)
               (lookup-find var (car env))
             (cond
               (found-p
                (typecase result
                  (closure
                   ;; Extend the closure with the environment in which it was found.
                   (values (extend-closure result in-env) found-p))
                  (t (values result found-p env))))
               (t
                 (lookup-find var (cdr env))))))
          (symbol (if (eql var key)
                      (values val t env)
                      (lookup-find var (cdr env))))))))

(defun* (empty-env -> env) ())
(defun* (empty-ram -> ram) () (make-ram :defs (empty-env) :macros (empty-env)))

(defun* extend-ram-defs ((ram ram) (var expr) (val expr))
  (make-ram :defs (extend (ram-defs ram) var val) :macros (ram-macros ram)))

(defun* extend-ram-macros ((ram ram) (var expr) (val expr))
  (make-ram :defs (ram-defs ram) :macros (extend (ram-macros ram) var val)))

(defun* extend ((env env) (var expr) (val expr))
  (check-type var symbol)
  ;; Use HCONS so equivalent returned environments are EQ.
  (hcons (hcons var val) env))

(defun* extend-rec ((env env) (var expr) (val expr))
  (check-type var symbol)
  (destructuring-bind (&optional binding-or-env . rest)
      env
    (destructuring-bind (&optional var-or-binding . val-or-more-bindings)
        binding-or-env
      (declare (ignore val-or-more-bindings))
      (etypecase var-or-binding
        ((or symbol nil) (hcons (hlist (hcons var val)) env))
        (cons (hcons (hcons (hcons var val) binding-or-env) rest))))))

(test eval-expr-for-p
  (let* ((p 1009)
         (evaluator (make-evaluator 1009))
         (empty-env (empty-env))
         (ram (empty-ram)))
    (flet ((evaluate (expr env)
             (funcall evaluator expr env ram)))
      (is (eql 1 (evaluate 1 empty-env)))
      (signals error (evaluate (1+ p) empty-env))
      (signals error (evaluate p empty-env))

      (signals error (evaluate 'a empty-env))

      (let* ((env (extend empty-env 'a 9))
             (env2 (extend env 'a 10))
             (env3 (extend empty-env 'b 8)))
        (is (eql 9 (evaluate 'a env)))
        (is (eql 10 (evaluate 'a env2)))
        (is (eql 8 (evaluate 'b env3)))
        (signals error (evaluate 'b env))
        (signals error (evaluate 'b env2))
        (signals error (evaluate 'a env3)))

      (is (eql api:t (evaluate '(api:atom 8) empty-env)))
      (is (eql api:t (evaluate '(api:atom 'a) empty-env)))
      (is (eql nil (evaluate '(api:atom '(1 2 3)) empty-env)))

      (signals error (evaluate 'x empty-env))

      (is (eql 123 (evaluate '(api:let ((x 123))
                               x)
                             empty-env)))

      (is (eql 987 (evaluate '(api:let ((x 123)
                                        (x 987))
                               x)
                             empty-env)))

      ;; Bindings are sequential, not parallel within a single LET* expression.
      (is (eql 1 (evaluate '(api:let ((a 1)
                                       (b 2))
                             (api:let ((b a)
                                        (a b))
                               a))
                           empty-env)))

      (is (eql 5 (evaluate '(api:+ 3 2) empty-env)))
      (is (eql 91 (evaluate '(api:+ 500 600) empty-env)))

      (is (eql 1 (evaluate '(api:- 3 2) empty-env)))
      (is (eql 909 (evaluate '(api:- 500 600) empty-env)))

      (is (eql 6 (evaluate '(api:* 3 2) empty-env)))
      (is (eql 306 (evaluate '(api:* 17 18) empty-env)))

      (is (eql 2 (evaluate '(api:/ 6 3) empty-env)))
      (is (eql 18 (evaluate '(api:/ 306 17) empty-env)))
      (signals error (evaluate '(api:/ 99 0) empty-env))

      (is (eql api:t (evaluate '(api:= 5 (api:+ 3 2)) empty-env)))
      (is (eql nil (evaluate '(api:= 6 (api:+ 3 2)) empty-env)))

      (is (eql api:t (evaluate '(api:eq 2 (api:+ 1 1)) empty-env)))
      (is (eql nil (evaluate '(api:eq 3 (api:+ 1 1)) empty-env)))

      (is (eql api:t (evaluate '(api:eq (api:cons 1 2) (api:cons 1 2)) empty-env)))
      (is (eql nil (evaluate '(api:eq (api:cons 1 2) (api:cons 2 3)) empty-env)))
      (signals error (evaluate '(api:= (api:cons 1 2) (api:cons 1 2)) empty-env))

      (is (eql 9 (evaluate '(api:car (api:cons 9 8)) empty-env)))
      (is (eql 8 (evaluate '(api:cdr (api:cons 9 8)) empty-env)))
      (is (eql nil (evaluate '(api:cdr nil) empty-env)))
      (signals error (evaluate '(api:cdr 99) empty-env))

      (is (eql 'a (evaluate '(api:car '(a 8)) empty-env)))
      (is (eql 'x (evaluate '(api:quote x) empty-env)))
      (is (eql 'x (evaluate ''x empty-env)))

      ;; This test will fail without QUOTE-EXPR.
      (is (eql api:t (evaluate '(api:eq (api:cons 'a 'b) '(a . b)) empty-env)))

      ;; Keep and uncomment when LIST is supported.
      ;; (is (equal '(1 2 3) (evaluate '(api:list 1 2 3) empty-env)))
      ;; (is (eql t (evaluate '(api:eq (api:cons 1 (api:cons 2 (api:cons 3 api:nil))) (api:list 1 2 3)) empty-env)))
      ;; (is (eql t (evaluate '(api:eq (api:cons 1 (api:cons 2 (api:cons 3 api:nil))) (api:list 1 (api:+ 1 1) 3)) empty-env)))

      (is (eql 1 (evaluate '(api:if api:t 1 2) empty-env)))
      (is (eql 2 (evaluate '(api:if api:nil 1 2) empty-env)))
      (is (eql 1 (evaluate '(api:if 9 1 2) empty-env)))
      (is (eql 1 (evaluate '(api:if (api:eq 'a 'a) 1 2) empty-env)))
      (is (eql 2 (evaluate '(api:if (api:eq 'a 'b) 1 2) empty-env)))
      (is (eql 1 (evaluate '(api:if (api:eq 3 (api:+ 1 2)) 1 2) empty-env)))

      (is (typep (evaluate '(api:lambda (x) (* x x)) empty-env) 'closure))
      (is (eql 81 (evaluate '(api:let ((f (api:lambda (x) (api:* x x)))) (f 9)) empty-env)))
      (is (eql 81 (evaluate '((api:lambda (x) (api:* x x)) 9) empty-env)))
      (is (eql 9 (evaluate '(api:let ((make-adder (api:lambda (x)
                                                    (api:lambda (y) (api:+ x y))))
                                      (adder (make-adder 1)))
                             (adder 8))
                           empty-env)))
      (is (eql 8 (evaluate '(api:letrec ((pow (api:lambda (base)
                                                 (api:lambda (exp)
                                                   (api:if (api:= exp 0)
                                                           1
                                                           (api:* base ((pow base) (api:- exp 1))))))))
                             ((pow 2) 3))
                           empty-env)))
      (is (eql 8 (evaluate '(api:letrec ((pow (api:lambda (base exp)
                                                 (api:if (api:= exp 0)
                                                         1
                                                         (api:* base (pow base (api:- exp 1)))))))
                             (pow 2 3))
                           empty-env)))
      (is (eq (hlist (hcons 'b 9) (hcons 'a 8)) (evaluate '(api:let ((a 8) (b 9)) (api:current-env)) empty-env)))

      (let ((lib-env (evaluate '(api:letrec ((pow (api:lambda (base exp)
                                                     (api:if (api:= exp 0)
                                                             1
                                                             (api:* base (pow base (api:- exp 1)))))))
                                 (api:current-env))
                               empty-env)))
        (is (eql 8 (evaluate '(pow 2 3) lib-env))))

      ;; Regression test to ensure function arguments are evaluated only once.
      (is (eq (hlist 1) (evaluate '(api:letrec ((f (api:lambda (x) x)))
                                    (f '(1)))
                                  nil)))

      ;; Regression: ensure that letrec* does not forget old bindings.
      (is (eq (hcons 1 1) (evaluate '(api:let ((disj (api:lambda (g1 g2) (api:lambda (x) (api:cons (g1 x) (g2 x))))))
                                      (api:letrec ((foo (disj (api:lambda (x) x) (api:lambda (x) x))))
                                       (foo 1)))
                                    nil)))

      (signals error (evaluate '(api:letrec ((a (api:lambda (x) (b x))) (b (api:lambda (x) (api:* x x)))) (a 9)) empty-env))
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defparameter *default-p* #x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001 
  "Order of BLS12-381's scalar field. (Default *for now*.)")

(defun evaluate (expr env ram)
  "Convenience function to evaluate without specifying field order."
  (eval-expr-for-p *default-p* expr env ram))

;; Using Extended Euclidean Algorithm
;; https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
;; Inverse of N in a prime-field P.
(defun inverse (a n)
  (let* ((s 0)
         (r n)
         (new-s 1)
         (new-r a))
    (loop while (not (= 0 new-r))
          for quotient = (floor r new-r)
          do (psetf s new-s
                    new-s (cl:- s (cl:* quotient new-s))
                    r new-r
                    new-r (cl:- r (cl:* quotient new-r))))
    ;; If r > 1 then ELT is not invertible.
    (assert (<= 1 r))
    (if (< s 0) (+ s n) s)))

(test inverse
  (is (equal 1 (inverse 1 13)))
  (is (equal 7 (inverse 2 13)))
  (is (equal 2 (inverse 7 13)))
  (is (equal 9 (inverse 3 13)))
  (is (equal 3 (inverse 9 13)))
  (is (equal 10 (inverse 4 13)))
  (is (equal 4 (inverse 10 13)))
  (is (equal 8 (inverse 5 13)))
  (is (equal 5 (inverse 8 13)))
  (is (equal 11 (inverse 6 13)))
  (is (equal 6 (inverse 11 13)))
  (is (equal 12 (inverse 12 13)))

  (let ((p 89))
    (loop for i from 1 below p
          for inv = (inverse i p)
          do (is (= i (inverse inv p)))
          do (is (= 1 (mod (* i inv) p))))))

(test type-regression
  (is (not (null (typep '(((((VAR DUMMY)))) Z) 'expr)))))
