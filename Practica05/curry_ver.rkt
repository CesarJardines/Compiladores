#lang nanopass

(require nanopass/base)
(require racket/set)

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
    t
    pr
    (begin e* ... e)
    (primapp pr e* ...)
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

(define-parser parse-LF LF)

#| Excercise 1|#

(define-language L1
  (extends LF)
  (Expr (e body)
        (- (not e)
           (or e* ...)
           (and e* ...))))


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
        ;Como se removió * ... body en LF también se remobió aqui ya que se extiende
        (- (let ([x* t* e*] ...) body* ... body)
           (letrec ([x* t* e*] ...) body* ... body))
        (+ (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body))))

(define-language L8
  (extends L7)
  (Expr (e body)
        (+ (letfun ([x t e]) body))))


(define-parser parser-L1 L1)
(define-parser parser-L7 L7)
(define-parser parser-L8 L8)

;Ejercicio 1
(define-pass curry-let : LF (ir) -> L7 ()
  (definitions
    (define (curry var type e body op)
      (cond
        [(equal? (length var) 0) body]
        [else `(,op ([,(car var) ,(car type) ,(car e)]) ,(curry (cdr var) (cdr type) (cdr e) body op))])))
  (Expr : Expr (ir) -> Expr ()
        [(let ([,x* ,t* ,[e*]] ...) ,[body])
         (let f ([x* x*] [t* t*] [e* e*])
           (if (not(null? x*)) `(let ([,(car x*) ,(car t*) ,(car e*)]) ,(f (cdr x*) (cdr t*) (cdr e*)))body))]
        [(letrec ([,x* ,t* ,[e*]] ...) ,[body])
         (let f ([x* x*] [t* t*] [e* e*])
           (if (not(null? x*)) `(letrec ([,(car x*) ,(car t*) ,(car e*)]) ,(f (cdr x*) (cdr t*) (cdr e*)))body))]))



;Ejercicio 2
(define-pass identify-assigment : L7(ir) -> L7()
  (Expr : Expr (ir) -> Expr()
    [(let ([,x ,t ,[e]]) ,[body]) (if (eq? t 'Lambda) `(letrec ([,x ,t ,e]) ,body) ir)]))


;Ejercicio 3
;Aquí se va ir guardando la cuenta 
(define count 0)

(define-pass un-anonymous : L7(ir) -> L8()
  (definitions
    (define new-name
      (lambda()
      (begin
        (define str (string-append "foo" (number->string count)))
        (set! count (+ count 1))
        (string->symbol str)))))
  (Expr : Expr (ir) -> Expr()
    [(lambda ([,x ,t]) ,[body])
     (let ([name (new-name)])
       `(letfun ([,name Lambda (lambda ([,x ,t]) ,body)]) ,name))]))

;Ejercicio 4
#|
(define-pass verify-arity : L8(ir) -> L8()
  (definitions
    (define (arit? o) (memq o '(+ - / *)))
    (define (list? l) (memq l '(car cdr length))))
  (Expr : Expr (ir) -> Expr()
    [(primapp ,pr ,[e*]...) (cond
                            [(and (arit? pr) (equal? (length e*) 2)) ir]
                            [(and (list? pr) (equal? (length e*) 1)) ir]
                            [else (error "Arity missmatch")])])) |#
#|
(define-pass verify-arity : L8 (ir) -> L8 ()
  (definitions
    (define (arit? o) (memq o '(+ - / *)))
    (define (list? o) (memq o '(car cdr length)))
    )
  (Expr : Expr (ir) -> Expr ()
        [(primapp ,pr ,[e*] ...)
         (cond
           [(and (arit? pr) (equal? (length e*))2) ir]
           [(and (list? pr) (equal? (length e*))2) ir]
           [else (error "Arity mismatch")])])) |#



;Funcion auxiliar para verify-vars
(define (rmvl lst x)
  (cond
    [(member x lst) (rmvl (remove x lst) x)]
    [else lst]))

(define (pass expr)
      (nanopass-case (L8 Expr) expr
                     [,x `(,x)]
                     [(begin ,e* ...,e) (append (pass e) (foldr append '() (map pass e*)))]
                     [(if ,e0 ,e1 ,e2) (append (append (pass e0) (pass e1)) (pass e2))]
                     [(lambda ([,x* ,t*] ...) ,body) (remv* (pass body) x*)]
                     [(let ([,x ,t ,e*]) ,body) (rmvl (append (pass body) (pass e*))  `,x)]
                     [(letrec ([,x ,t ,e*]) ,body) (rmvl (append (pass body) (pass e*) `,x))]
                     [(letfun ([,x ,t ,e*]) ,body) (rmvl (append (pass body) (pass e*) `,x))]
                     [(primapp ,pr ,e* ...) (foldr append '() (map pass e*))]
                     [(list ,e* ,e) (append (verify-vars e) (append-map verify-vars e*))]
                     [else expr]))

(define-pass verify-vars : L8 (ir) -> L8 ()
  (Expr : Expr (ir) -> Expr ()
        [,x (let([lst (pass ir)])(if (empty? lst)
                                           `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]
        [(begin ,e* ...,e) (let([lst (pass ir)])(if (empty? lst)
                                                          `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]
        [(primapp ,pr ,[e*] ...) (let([lst (pass ir)])(if (empty? lst)
                                                                `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]
        [(if ,e0 ,e1 ,e2) (let([lst (pass ir)])(if (empty? lst)
                                                         `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]
        [(lambda ([,x* ,t*] ...) ,body) (let([lst (pass ir)])(if (empty? lst)
                                                                       `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]
        [(let ([,x* ,t* ,e*]) ,body) (let([lst (pass ir)])(if (empty? lst)
                                                                    `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]
        [(letrec ([,x* ,t* ,e*]) ,body) (let([lst (pass ir)])(if (empty? lst)
                                                                       `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]
        [(list ,e* ,e) (append (verify-vars e) (append-map verify-vars e*))]
        [(letfun ([,x ,t ,e]) ,body) (let([lst (pass ir)])(if (empty? lst)
                                                                    `,ir(error (string-append "Free variable x" (symbol->string (car lst))))))]))



#| Some examples for exercise 1|#

;; Hint: Consider the arguments of the let and letrec expressions as lists (bindings). c:

;(curry-let (parse-LF `(let ([x Int 4] [y Int 6]) (+ x y))))
;; Desired response: (language:L7 '(let ((x Int 4)) (let ((y Int 6)) (+ x y))));
;(curry-let (parse-LF `(let ([x Int 4] [y Int 6] [z Int x]) (+ x y z))))
;; Desired response: (language:L7 '(let ((x Int 4)) (let ((y Int 6)) (let ((z Int x)) (+ x y z)))))
;(curry-let (parse-LF `(letrec ([f Lambda (lambda [x Int] (+ x 1))] [g Lambda (lambda [x Int] (* x 1))]) (f (g 3)))))
;; Desired response: (language:L7 '(letrec ((f Lambda (lambda (x Int) (+ x 1)))) (letrec ((g Lambda (lambda (x Int) (* x 1)))) (f (g 3)))))
;(curry-let (parse-LF `(+ (let ([x Int 4] [y Int 6]) (+ x y)) 1)))
;; Desired response: (language:L7 '(+ (let ((x Int 4)) (let ((y Int 6)) (+ x y))) 1))

#| Some examples for exercise 2 |#

;; Hint: Don't waste time, you can assume the entry language is L7. c; Give a good explanation why this assumption satisfies the specification.

;(identify-assigment (curry-let (parse-LF `(+ (let ([foo Lambda (lambda [x Int] (+ x 1))] [faa Lambda (lambda [y Int] (+ y 1))]) (foo (faa 1))) 1))))
;; Desired response (language:L7 '(+ (letrec ((foo Lambda (lambda (x Int) (+ x 1)))) (letrec ((faa Lambda (lambda (y Int) (+ y 1)))) (foo (faa 1)))) 1))
;(identify-assigments (curry-let (parse-LF `(+ (let ([foo Lambda (lambda [x Int] (+ x 1))] [y Int 4]) (+ (foo 1) y)) 1))))
;; Desired response (language:L7 '(+ (letrec ((foo Lambda (lambda (x Int) (+ x 1)))) (let ((y Int 4)) (+ (foo 1) y))) 1))

#| Some examples for exercise 3 |#

;; Hint: First, create a pass that adds "foo" as the name of the letfun constructor, for every lambda.
;; More precisely, every time we achieve a lambda, we simply add "foo".
;; Second, create a pass that creates a new "foo" every time appears a letfun. An idea of this goal can be the excercise of the last practice.
;; Third, create a pass that receives a expression of L7, uses the two last passes, and returns the desired expression.

;(un-anonymous (parser-L7 '(lambda ([x Bool]) (if x 1 2))))
;; Desired response (language:L8 '(letfun ((foo Lambda (lambda ((x Bool)) (if x 1 2)))) foo))
;(un-anonymous (parser-L7 '(lambda ([y Int]) (lambda ([x Bool]) (if x 1 y)))))
;; Desired response (language:L8 '(letfun ((foo Lambda (lambda ((y Int)) (letfun ((foo0 Lambda (lambda ((x Bool)) (if x 1 y)))) foo0)))) foo))


#| Some examples for exercise 4 |#
;(verify-arity (parser-L8 '(+ 2 3)))
;(verify-arity (parser-L8 '(+ 2 )))
;(verify-arity (parser-L8 '( car 2 3)))
;(verify-arity (parse-L8 '(primapp * (quot 3))))


#| Some examples for exercise 5 |#
;(verify-vars (parser-L8 '(x)))
;(verify-vars (parser-L8 '(+ 2 x)))
(verify-vars (parser-L8 '(let ((x Bool #t)) (x)))) ; no errror