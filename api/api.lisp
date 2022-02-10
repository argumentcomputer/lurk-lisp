(in-package lurk.api.impl)
(def-suite* api-impl-suite :in lurk:master-suite)

(defconstant api:t 'api:t)

(defstruct closure env function (macrop nil))

(defun* (extend-closure -> closure) ((c closure) (rec-env env))
  (let ((extended (hcons rec-env (closure-env c))))
    (make-closure :env extended :function (closure-function c) :macrop (closure-macrop c))))

;; The following types loosely specify the language which EXPR allows.

;; A FIELD-ELEMENT is a natural number below P, where P must be prime. If P is
;; unsupplied, then no upper bound is placed on the value.
(deftype field-element (&optional p) (if (or (not p) (eql p '*))
                                         `(integer 0)
                                         `(integer 0 ,(- p 1))))

;; An ATOM is a FIELD-ELEMENT, SYMBOL, FUNCTION, or NIL.
(deftype atom (&optional p) `(or (field-element ,p) symbol nil function closure))

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

(deftype built-in-unary () '(member api:atom api:car api:cdr api:quote))
(deftype built-in-binary () '(member api:+ api:- api:/ api:* api:= api:eq api:cons))
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

(defun* (eval-expr-for-p -> (values expr env)) ((p integer) (expr expr) (env env))
  (labels ((eval-expr (expr env)
           (eval-expr-for-p p expr env))
           (apply-closure (closure args env)
             (if (closure-macrop closure)
                 (let* ((quoted-args (mapcar (lambda (unevaled) `(quote ,unevaled)) args))
                        (macro-result (apply (closure-function closure) (closure-env closure) quoted-args))
                        (result (eval-expr macro-result env)))
                   (values result env))
                 (let* ((evaled-args (mapcar (lambda (x) (eval-expr x env)) args))
                        (quoted-args (mapcar (lambda (evaled) `(quote ,evaled)) evaled-args)))
                   (values (apply (closure-function closure) (closure-env closure) quoted-args) env)))))
    (etypecase expr
      ((or closure self-evaluating-symbol) (values expr env))
      (symbol
       (values (lookup expr env) env))
      (atom (unless (typep expr `(atom ,p))
              (error "~S is out of range [0, ~S)." expr p))
       (values expr env))

      (list
       (destructuring-bind (head &rest rest) expr
         (etypecase head
           (closure (apply-closure head env rest))
           ((eql api:let*)
            (destructuring-bind (bindings &optional body-expr) rest
              (let ((new-env env))
                (loop for (var val) in bindings
                      ;; Evaluate VAL in NEW-ENV
                      for evaled = (eval-expr val new-env)
                      do (setq new-env (extend new-env var evaled)))
                (eval-expr body-expr new-env))))
           ((eql api:letrec*)
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
                     (source `(lambda (,env-var ,@args)
                                (eval-expr-for-p ,p
                                                 ;; Close your eyes and believe.
                                                 `(api:let* (,,@(mapcar (lambda (arg) `(list ',arg ,arg)) args))
                                                    ,',body-expr)
                                                 ,env-var))))
                (values (make-closure :env env :function (compile nil source)) env))))
           ((eql api:lambda-macro)
            (destructuring-bind (args body-expr) rest
              ;; TODO(namin): avoid dup with case above
              (let* ((env-var (gensym "ENV"))
                     (source `(lambda (,env-var ,@args)
                                (eval-expr-for-p ,p
                                                 ;; Close your eyes and believe.
                                                 `(api:let* (,,@(mapcar (lambda (arg) `(list ',arg ,arg)) args))
                                                    ,',body-expr)
                                                 ,env-var))))
                (values (make-closure :env env :function (compile nil source) :macrop t) env))))
           ((eql api:if)
            (destructuring-bind (condition a b) rest
              (let ((result (if (eval-expr condition env)
                                (eval-expr a env)
                                (eval-expr b env))))
                (values result env))))
           #+(or) ;; Disabled initially, since variadic functions aren't simple in other implementations.
           ((eql api:list)
            (values (hlist* (mapcar (lambda (x) (eval-expr x env)) rest))
                    env))
           ((eql api:current-env) (values env env))
           (built-in-unary
            (destructuring-bind (arg) rest
              (let ((result (ecase head
                              (api:atom
                               (typecase (eval-expr arg env)
                                 (atom (values api:t env))
                                 (t (values api:nil env))))
                              (api:car (car (eval-expr arg env)))
                              (api:cdr (cdr (eval-expr arg env)))
                              (api:quote (quote-expr arg)))))
                (values result env))))
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
                               (api:eq (if (eq evaled-a evaled-b) api:t api:nil))
                               (api:cons (hcons evaled-a evaled-b)))))
                (values result env))))
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
  (lambda (expr env)
    (eval-expr-for-p p expr env)))

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
         (empty-env (empty-env)))
    (flet ((evaluate (expr env)
             (funcall evaluator expr env)))
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

      (is (eql 123 (evaluate '(api:let* ((x 123))
                               x)
                             empty-env)))

      (is (eql 987 (evaluate '(api:let* ((x 123)
                                        (x 987))
                               x)
                             empty-env)))

      ;; Bindings are sequential, not parallel within a single LET* expression.
      (is (eql 1 (evaluate '(api:let* ((a 1)
                                       (b 2))
                             (api:let* ((b a)
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
      (is (eql 81 (evaluate '(api:let* ((f (api:lambda (x) (api:* x x)))) (f 9)) empty-env)))
      (is (eql 81 (evaluate '((api:lambda (x) (api:* x x)) 9) empty-env)))
      (is (eql 9 (evaluate '(api:let* ((make-adder (api:lambda (x)
                                                    (api:lambda (y) (api:+ x y))))
                                      (adder (make-adder 1)))
                             (adder 8))
                           empty-env)))
      (is (eql 8 (evaluate '(api:letrec* ((pow (api:lambda (base)
                                                 (api:lambda (exp)
                                                   (api:if (api:= exp 0)
                                                           1
                                                           (api:* base ((pow base) (api:- exp 1))))))))
                             ((pow 2) 3))
                           empty-env)))
      (is (eql 8 (evaluate '(api:letrec* ((pow (api:lambda (base exp)
                                                 (api:if (api:= exp 0)
                                                         1
                                                         (api:* base (pow base (api:- exp 1)))))))
                             (pow 2 3))
                           empty-env)))
      (is (eq (hlist (hcons 'b 9) (hcons 'a 8)) (evaluate '(api:let* ((a 8) (b 9)) (api:current-env)) empty-env)))

      (let ((lib-env (evaluate '(api:letrec* ((pow (api:lambda (base exp)
                                                     (api:if (api:= exp 0)
                                                             1
                                                             (api:* base (pow base (api:- exp 1)))))))
                                 (api:current-env))
                               empty-env)))
        (is (eql 8 (evaluate '(pow 2 3) lib-env))))

      ;; Regression test to ensure function arguments are evaluated only once.
      (is (eq (hlist 1) (evaluate '(api:letrec* ((f (api:lambda (x) x)))
                                    (f '(1)))
                                  nil)))

      ;; Regression: ensure that letrec* does not forget old bindings.
      (is (eq (hcons 1 1) (evaluate '(api:let* ((disj (api:lambda (g1 g2) (api:lambda (x) (api:cons (g1 x) (g2 x))))))
                                      (api:letrec* ((foo (disj (api:lambda (x) x) (api:lambda (x) x))))
                                       (foo 1)))
                                    nil)))

      (signals error (evaluate '(api:letrec* ((a (api:lambda (x) (b x))) (b (api:lambda (x) (api:* x x)))) (a 9)) empty-env))
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defparameter *default-p* #x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001 
  "Order of BLS12-381's scalar field. (Default *for now*.)")

(defun eval (expr env)
  "Convenience function to evaluate without specifying field order."
  (eval-expr-for-p *default-p* expr env))

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
