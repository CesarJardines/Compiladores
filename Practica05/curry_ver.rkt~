#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza

Some helper functions done in the session 11/6/2020.
|#

;; Language LF definition
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (list (l))
   (string (s))
   (type (t)))
  (Expr (e body)
    x
    c
    l
    s
    pr
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (lambda ([x* t*] ...) body* ... body)
    (let ([x* t* e*] ...) body* ... body)
    (letrec ([x* t* e*] ...) body* ... body)
    (or e* ...)
    (and e* ...)
    (not e)
    (list e* e)
    (e0 e1 ...)))

;; Predicate for the variables
(define (variable? x)
  (symbol? x))

;; Predicate for the types
(define (type? x)
  (or (equal? x 'Bool) (equal? x 'Int) (equal? x 'Char) (equal? x 'List) (equal? x 'String) (equal? x 'Lambda)))

;; Predicate for the constants
(define (constant? x)
  (or (boolean? x) (number? x) (char? x)))

;; Predicate for primitives
(define (primitive? x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/) (equal? x 'and) (equal? x 'or) (equal? x 'not)
      (equal? x 'length) (equal? x 'car) (equal? x 'cdr)))

;; All this for the parser

(define-parser parser-LF LF)

#| Excercise 1|#

(define-language L1
  (extends LF)
  (Expr (e body)
        (- (not e)
           (or e* ...)
           (and e* ...))))

(define-parser parser-L1 L1)

(define-pass remove-and-or-not : LF (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(not ,[e]) `(if ,e #f #t)]
        [(or) #f]
        [(or ,[e] ,[e*] ...) ; e -> e1 ... e2 está en e*
         (let f ([e e] [binding* e*])
           (if (null? binding*)
               e
               `(if ,e #t ,(f (car binding*) (cdr binding*)))))]
        [(and) #t]
        [(and ,[e] ,[e*] ...)
         (let f ([e e] [binding* e*])
           (if (null? binding*)
               e
               `(if ,e ,(f (car binding*) (cdr binding*)) #f)))]))

(define-language L7
  (extends LF)
  (Expr (e body)
        (- (let ([x* t* e*] ...) body* ... body)
           (letrec ([x* t* e*] ...) body* ... body))
        (+ (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body))))

#| Some examples for exercise 1|#

;; Hint: Consider the arguments of the let and letrec expressions as lists (bindings). c:

(curry-let (parse-LF `(let ([x Int 4] [y Int 6]) (+ x y))))
;; Desired response: (language:L7 '(let ((x Int 4)) (let ((y Int 6)) (+ x y))))
(curry-let (parse-LF `(let ([x Int 4] [y Int 6] [z Int x]) (+ x y z))))
;; Desired response: (language:L7 '(let ((x Int 4)) (let ((y Int 6)) (let ((z Int x)) (+ x y z)))))
(curry-let (parse-LF `(letrec ([f Lambda (lambda [x Int] (+ x 1))] [g Lambda (lambda [x Int] (* x 1))]) (f (g 3)))))
;; Desired response: (language:L7 '(letrec ((f Lambda (lambda (x Int) (+ x 1)))) (letrec ((g Lambda (lambda (x Int) (* x 1)))) (f (g 3)))))
(curry-let (parse-LF `(+ (let ([x Int 4] [y Int 6]) (+ x y)) 1)))
;; Desired response: (language:L7 '(+ (let ((x Int 4)) (let ((y Int 6)) (+ x y))) 1))

#| Some examples for exercise 2 |#

;; Hint: Don't waste time, you can assume the entry language is L7. c; Give a good explanation why this assumption satisfies the specification.

(identify-assigments (curry-let (parse-LF `(+ (let ([foo Lambda (lambda [x Int] (+ x 1))] [faa Lambda (lambda [y Int] (+ y 1))]) (foo (faa 1))) 1))))
;; Desired response (language:L7 '(+ (letrec ((foo Lambda (lambda (x Int) (+ x 1)))) (letrec ((faa Lambda (lambda (y Int) (+ y 1)))) (foo (faa 1)))) 1))
(identify-assigments (curry-let (parse-LF `(+ (let ([foo Lambda (lambda [x Int] (+ x 1))] [y Int 4]) (+ (foo 1) y)) 1))))
;; Desired response (language:L7 '(+ (letrec ((foo Lambda (lambda (x Int) (+ x 1)))) (let ((y Int 4)) (+ (foo 1) y))) 1))

#| Some examples for exercise 3 |#

;; Hint: First, create a pass that adds "foo" as the name of the letfun constructor, for every lambda.
;; More precisely, every time we achieve a lambda, we simply add "foo".
;; Second, create a pass that creates a new "foo" every time appears a letfun. An idea of this goal can be the excercise of the last practice.
;; Third, create a pass that receives a expression of L7, uses the two last passes, and returns the desired expression.

(un-anonymous (parse-L7 '(lambda ([x Bool]) (if x 1 2))))
;; Desired response (language:L8 '(letfun ((foo Lambda (lambda ((x Bool)) (if x 1 2)))) foo))
(un-anonymous (parse-L7 '(lambda ([y Int]) (lambda ([x Bool]) (if x 1 y)))))
;; Desired response (language:L8 '(letfun ((foo Lambda (lambda ((y Int)) (letfun ((foo0 Lambda (lambda ((x Bool)) (if x 1 y)))) foo0)))) foo))
