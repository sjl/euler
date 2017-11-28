(in-package :euler)

;;;; Operator Protocol
(defgeneric operatorp (object)
  (:method ((object t)) nil))

(defun operandp (object)
  (not (operatorp object)))

(defgeneric operator-arity (operator))
(defgeneric operator-function (operator))
(defgeneric operator-weight (operator))

(defmacro define-operator (symbol function arity weight)
  `(progn
     (defmethod operatorp ((symbol (eql ',symbol)))
       t)
     (defmethod operator-arity ((symbol (eql ',symbol)))
       ,arity)
     (defmethod operator-function ((symbol (eql ',symbol)))
       ,function)
     (defmethod operator-weight ((symbol (eql ',symbol)))
       ,weight)))


;;;; Binary
(define-operator = '= 2 0)
(define-operator < '> 2 0)
(define-operator > '< 2 0)
(define-operator <= '>= 2 0)
(define-operator >= '<= 2 0)
(define-operator - '- 2 1)
(define-operator + '+ 2 1)
(define-operator * '* 2 2)
(define-operator / '/ 2 2)
(define-operator % 'rem 2 2)
(define-operator ^ 'expt 2 3)


;;;; Unary
(define-operator sqrt 'sqrt 1 10)
(define-operator sin 'sin 1 10)
(define-operator cos 'cos 1 10)
(define-operator tan 'tan 1 10)
(define-operator abs 'abs 1 10)


;;;; Parsing
(defun shunting-yard (tokens &aux operators output)
  (labels ((precedence>= (a b)
             (unless (null a)
               (>= (operator-weight a)
                   (operator-weight b))))
           (pop-unary-operator ()
             (push (list (operator-function (pop operators))
                         (pop output))
                   output))
           (pop-binary-operator ()
             (push (list (operator-function (pop operators))
                         (pop output)
                         (pop output))
                   output))
           (pop-operator ()
             (ecase (operator-arity (first operators))
               (1 (pop-unary-operator))
               (2 (pop-binary-operator))))
           (lisp-expression-p (expr)
             (or (vectorp expr)
                 (and (consp expr)
                      (eq (first expr) 'lisp))))
           (lisp-expression (expr)
             (etypecase expr
               (vector (coerce expr 'list))
               (cons (second expr)))))
    (do* ((tokens (reverse tokens) (rest tokens))
          (prev nil token)
          (token (first tokens) (first tokens)))
        ((null tokens))
      (cond
        ((lisp-expression-p token)
         (push (lisp-expression token) output))
        ((consp token)
         (push (shunting-yard token) output))
        ((operatorp token)
         (iterate (while (precedence>= (first operators)
                                       token))
                  (pop-operator)
                  (finally (push token operators))))
        ((and prev (not (operatorp prev))) ; implicit multiplication
         (setf token prev)
         (push '* tokens)
         (push prev tokens))
        (t (push token output))))
    (iterate (while operators)
             (pop-operator)))
  (first output))


;;;; API
(defmacro math (&rest expr)
  (shunting-yard expr))

