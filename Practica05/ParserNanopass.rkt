#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza

Our first approach with nanopass.

César Eduardo Jardines Mendoza, 314071549
Jerónimo Almeida Rodríguez, 418003815

|#

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         nanopass/base)

; Calling the script of "Practica 2 - Parte 2"
(require "Parser.rkt")


; Function that returns the string representation of a ASA
(define (expr->string e)
  (match e
    [(var-exp e) (symbol->string e)]
    [(num-exp e) (number->string e)]
    [(bool-exp e) (format "~a" e)]
    [(int-exp) "Int"]
    [(boole-exp) "Bool"]
    [(par-exp e) (string-append "(" (expr->string e) ")")]
    [(key-exp e) (string-append "{" (expr->string e) "}")]
    [(key-exp e) (string-append "[" (expr->string e) "]")]
    [(begin-exp e) (string-append "(begin " (expr->string e) " )")]
    [(prim-exp '- e1 e2) (string-append "- " (expr->string e1) " " (expr->string e2))]
    [(prim-exp '+ e1 e2) (string-append "+ " (expr->string e1) " " (expr->string e2))]
    [(prim-exp '/ e1 e2) (string-append "/ " (expr->string e1) " " (expr->string e2))]
    [(prim-exp '* e1 e2) (string-append "* " (expr->string e1) " " (expr->string e2))]
    [(prim-exp 'and p q) (string-append "and " (expr->string p) " " (expr->string q))]
    [(prim-exp 'or p q) (string-append "or " (expr->string p) " " (expr->string q))]
    [(typeof-exp v e) (string-append "(" (expr->string v) " " (expr->string e) ")")]
    [(let-exp e b) (string-append "(let " (expr->string e) " " (expr->string b) " )")]
    [(app-exp e1 e2) (string-append "( " (expr->string e1) " " (expr->string e2) " )")]
    [(app-t-exp e1 e2) (string-append "( " (expr->string e1) " " (expr->string e2) " )")]
    [(assign-exp e1 e2) (string-append "( " (expr->string e1) " " (expr->string e2) " )")]
    [(if-then-exp1 g e1) (string-append "(if " (expr->string g) " " (expr->string e1) " )")]
    [(if-then-exp g t e) (string-append "(if " (expr->string g) " " (expr->string t) " " (expr->string e) ")")]
    [(fun-f-exp lv a b) (string-append "(funF " (expr->string lv) (expr->string a) " " (expr->string b) " )" )]
    [(fun-exp (typeof-exp args t) b) (string-append "(fun " (expr->string args) " " (expr->string t) " " (expr->string b) " )")]))



; The definition of our language
(define-language LF
  (terminals
   (variable (x))
   (string (s))
   (list (l))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
    x
    pr
    c
    t
    s
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (fun ((x* t*) ...) t body* ... body)
    (let ((x* t* e*) ...) body* ... body)
    (funF x ((x* t*) ...) t body* ... body)
    (e0 e1 ...)
    (list e* e)
    (pr e* ... e)))

;Some predicates
(define (variable? x) (symbol? x))

(define (type? t)
 (or (equal? t 'Int) (equal? t 'Bool) (string? t)))

(define (constant? x)
  (or (number? x) (boolean? x) (char? x)))

(define (primitive? x) (memq x '(+ - * / and or list)))

(define-parser parse-LF LF)

; A function that make explicit the ocurrences of the begin
(define-pass make-explicit : LF (ir) -> LF ()
  (Expr : Expr (ir) -> Expr ()
    [,c `',c]
    [(fun ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(fun ([,x* ,t*] ...) t (begin ,body* ... ,body))]
    [(let ([,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
     `(let ([,x* ,t* ,e*] ...) (begin ,body* ... ,body))]
    [(funF ,x ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(funF x ([,x* ,t*] ...) t (begin ,body* ... ,body))]))


; The parser of LF
(define-parser parser-LF LF)
;lenguaje para el ejercicio 5
(define-language LNI (extends LF)
  (Expr (e body)
        (- (if e0 e1))
        ))
(define-parser parser-LNI LNI)


;Se comenta para que no mande error
;(define-parser parser-LF LF)
;Lenguaje para el ejercicio 6
;(define-language LNI2 (extends LF)
 ; (Expr (e body)
  ;      (- (if e0 e1))
   ;     ))
;(define-parser parser-LN2 LN2)


 (define var-lexer
   (lexer
    [(:: (:+ (char-range #\a #\z)(:+(char-range #\A #\Z))))
     ;=>
     (cons `(VAR1 , (string->symbol lexeme)) (var-lexer input-port))]
    [(::(:: (:+ (char-range #\1 #\9))(:*(char-range #\0 #\9))))
     ;=>
     (cons `(NUM1 ,(string->number lexeme))(var-lexer input-port))]
    [(eof)
     `()]))

; Define a function that takes a list [x1,...,xn-1,xn] and
; returns the list [x1,...,xn-1]
(define (init l)
  (error "Oopsie Woopsie! Uwu We made a fucky wucky!! A wittle fucko boingo! The code monkeys at our headquarters are working VEWY HAWD to fix this!"))

; Define a function that takes a list of tokens [t1,...,tn] and if the last token is
; a number n, returns [t1,...,n+1]; otherwise, returns [t1,...,tn,0]
(define (newName lToks)
  (error "Oopsie Woopsie! Uwu We made a fucky wucky!! A wittle fucko boingo! The code monkeys at our headquarters are working VEWY HAWD to fix this!"))

; Define a function that takes a function that given a list of tokens,
; flats the list of tokens into a string
(define (constructVar lToks)
  (error "Oopsie Woopsie! Uwu We made a fucky wucky!! A wittle fucko boingo! The code monkeys at our headquarters are working VEWY HAWD to fix this!"))

; Define a function that takes a symbol that represents a name of a variable
; and returns a fresh name of variable as a string
(define (newVar x)
  (error "Oopsie Woopsie! Uwu We made a fucky wucky!! A wittle fucko boingo! The code monkeys at our headquarters are working VEWY HAWD to fix this!"))

;Definir un preproceso del compilador que renombre las variables
;usadas, de tal forma que queden unificados los nombres de estas y no
;existan variables repetidas.
(define (rename-var var-lexer)
  (if (empty? var-lexer)
      0
      (newVar(constructVar(newName (init var-lexer))))))

;; Proceso del compilador encargado de eliminar la expresion if sin el caso para else.
(define-pass remove-one-armed-if : LF(ir) -> LF ()
  (Expr : Expr (ir) -> Expr ()
        [(if ,[e0] ,[e1]) '(if ,e0 ,e1 (void))]
        [(if ,[e0] ,[e1] ,[e2]) '(if ,e0 ,e1 ,e2)]))

;; Procesp del compilador encargado de eliminar las cadenas como elementos
;; terminales del lenguaje.
;; Tomamos un pequeño apoyo en https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_6.html para entender y
;; aplicar algunos funcionanmientos de las expresiones
(define-pass remove-string : LF(ir) -> LF ()
  (definitions
    (define (make-list  l1 l2)
      (cond
        [(null? l2) l1]
        [else (make-list (append l1 (list (car l2))) (cdr l2))])))
  (Expr : Expr (ir) -> Expr ()
        [(unquote s) (let ([l2 (string->list s)])
                       `,(make-list '(list) l2))]))




; Concrete expression;
; (33 + 2)
;(expr->string (par-exp (prim-exp '+ (num-exp 33) (num-exp 2)))) ;;;;;;TOOOOODOOOOOO
; Answer: "(+ 33 2)"

; Concrete expression
; 3 - (3 / 6)
;(expr->string (prim-exp '- (num-exp 3) 
    ;(par-exp (prim-exp '/ (num-exp 3) (num-exp 6))))) ;;;;;;;;;TOOOOOOOODOOOOOOO
; Answer "(- 3 (/ 3 6))"

; Concrete expression:
; if(#t and #f)then{2}else{3}
;(expr->string (if-then-exp (prim-exp 'and (bool-exp #t) (bool-exp #f)) ;;;;;;;;;;;TOOOOOOOODOOOOOOO
    ;(num-exp 2) (num-exp 3)))
; Answer: "(if (and #t #f) 2 3)"

; Concrete expression:
; fun ([x:Int]:Int) => x
;(expr->string (fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x) ;;;;;;;;;;TOOOOODOOOOOOOO
    ;(int-exp))) (int-exp)) (var-exp 'x)))
; Answer: "(fun ((x Int)) Int x)"

; Concrete expression:
; fun ([x:Int][y:Int]:Int) => x*y
;(expr->string (fun-exp
 ;(typeof-exp (brack-exp (app-t-exp 
    ;(typeof-exp (var-exp 'x) 
        ;(int-exp)) (typeof-exp (var-exp 'y) (int-exp)))) (int-exp))
 ;(prim-exp '* (var-exp 'x) (var-exp 'y))))
; Answer: (fun ((x Int) (y Int)) Int (* x y))"

; Concrete expression:
; funF (sumita ([x:Int][y:Int]):Int) => x+y
;(fun-f-exp
 ;(typeof-f-exp (var-exp 'sumita) (brack-exp 
    ;(app-t-exp (typeof-exp (var-exp 'x) (int-exp)) 
    ;(typeof-exp (var-exp 'y) (int-exp)))) (int-exp))
 ;(prim-exp '+ (var-exp 'x) (var-exp 'y)))
; Answer: "(funF sumita ((x Int) (y Int)) Int (+ x y))"