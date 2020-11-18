#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza
Lexer y parser

César Eduardo Jardines Mendoza, 314071549
Jerónimo Almeida Rodríguez, 418003815

|#

(require "Compiler.rkt" ;; Here goes your lexer file. In my case I called "Compiler.rkt".
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/lex)

(provide (all-defined-out))

;; An abstraction of the grammar minHS.
(define-struct var-exp (i) #:transparent)   ; For the variables.
(define-struct num-exp (n) #:transparent)   ; For the numbers.
(define-struct bool-exp (b) #:transparent)  ; For the booleans.
(define-struct and-exp (p q) #:transparent)
(define-struct or-exp (p q) #:transparent)  

; Note: There is a difference: bool is for values and boole is for type IMPORTANT

(define-struct typeof-exp (v e) #:transparent)        
(define-struct typeof-f-exp (f t e) #:transparent)    
(define-struct int-exp () #:transparent)              
(define-struct boole-exp () #:transparent)            

(define-struct prim-exp (op e1 e2) #:transparent)     ; For the arithmetic operations.
(define-struct assign-exp (e1 e2) #:transparent)      ; For the assignment expr.
(define-struct if-then-exp (g e1 e2) #:transparent)   ; For the if conditionals.
(define-struct let-exp (e b) #:transparent)       
(define-struct fun-exp (lv e) #:transparent)          
(define-struct fun-f-exp (lv t e) #:transparent)      
(define-struct func-exp (e1 e2) #:transparent)        
(define-struct app-exp (e1 e2) #:transparent)         
(define-struct app-t-exp (e1 e2) #:transparent)       

(define-struct par-exp (exp) #:transparent)           
(define-struct key-exp (exp) #:transparent)           
(define-struct brack-exp (exp) #:transparent)

;Se agrega define para begin
(define-struct begin-exp (exp) #:transparent)
(define-struct if-then-exp1 (g e1)#:transparent)

(define minHS-parser
  (parser
   (start exp)      ; start clause. The exp is the initial symbol where the parser begins the analysis. 
   (end EOF)        ; end clause. The parser ends when it reads the given symbol. In our case, EOF.
   (error void)     ; error clause. Here can be some errors presented in the anlysis.
   (tokens a b)     ; tokens clause. Here goes our tokens. In our case, we defined the tokens in the lexer script.
   (precs (nonassoc LK RK LS RS LP RP IF THEN ELSE FUNF FUN FUNC LET IN END NUM BOOL VAR INT BOOLE)
          (left ACA)
          (left APP)
          (left ASSIGN)
          (left TYPEOF)          
          (left * /)
          (left - + AND OR))   ; precs clause. Here we can give some precedence of our language operators.
   (grammar                    ; grammar clause. Here goes the grammar of minHS.
    (exp
     ((BOOL) (bool-exp $1))
     ((NUM) (num-exp $1)) ;; ((Token) (constructor $1 $2 ... $n)) [1,2,3,...,n]
     ((VAR) (var-exp $1));
     ((exp + exp) (make-prim-exp + $1 $3)) ; ((e1 e2 e3 .... en) (constructor $1 $2 $3 ... $n))
     ((exp - exp) (make-prim-exp - $1 $3))
     ((exp * exp) (make-prim-exp * $1 $3))
     ((exp / exp) (make-prim-exp / $1 $3))
     ((exp AND exp) (and-exp $1 $3))
     ((exp OR exp) (or-exp $1 $3));

     ;Se agrega begin e if con then 
     ((BEGIN exp) (begin-exp $2))
     ((IF LP exp RP THEN LK exp RK) (if-then-exp1 $3 $7))

     
     ((IF LP exp RP THEN LK exp RK ELSE LK exp RK) (if-then-exp $3 $7 $11))
     ((FUN LP funn RP ACA exp) (fun-exp $3 $6))
     ((FUNF LP exp LP TO RP TYPEOF type RP ACA exp) (fun-f-exp (typeof-exp $3 $5) $8 $11));;
     ;;((FUNC type type) (func-exp $2 $3)) SE ELIMINA POR SER INSTRUCCIÓN DE LA PRÁCTICAS
     ((exp APP exp) (app-exp $1 $3))
     ((LET LP TE RP IN exp END) (let-exp (make-brack-exp $3) $6))
     ((LP exp RP) (make-par-exp $2))
     ((LS exp RS) (make-brack-exp $2))
     ((LK exp RK) (make-key-exp $2))
     ((exp ASSIGN exp) (assign-exp $1 $3))
     ((exp TYPEOF type) (typeof-exp $1 $3)))
    
    (type
     ((INT) (int-exp))
     ((BOOLE) (boole-exp))
     ((FUNC type type) (func-exp $2 $3))
     ((LP type RP) (par-exp $2)))
    
    (funn 
     ((exp TYPEOF type) (typeof-exp $1 $3))
     ((LS funn RS TYPEOF type) (typeof-exp $2 $5))
     ((funn funn) (app-t-exp $1 $2)))
    
    (TO 
     ((LS exp TYPEOF type RS) (make-brack-exp (typeof-exp $2 $4)))
     ((LS exp TYPEOF type RS TO) (app-t-exp (typeof-exp $2 $4) $6)))
    
    (TE 
     ((LS exp ASSIGN exp RS) (assign-exp $2 $4))
     ((LS exp ASSIGN exp RS TE) (app-t-exp (assign-exp $2 $4) $6)))
    )))

; A function that stores our lexer into a lambda function without arguments.
(define (lex-this lexer input) (lambda () (lexer input)))

;A lot of examples.
#|
(display "Example 1: 3 - (3 / 6)\n")
|#
(let ((input (open-input-string "3 - (3 / 6)")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(prim-exp #<procedure:-> (num-exp 3) (par-exp (prim-exp #<procedure:/> (num-exp 3) (num-exp 6))))
|#
#|
(display "\nExample 2: if(#t)then{2}else{3}\n")
(let ((input (open-input-string "if(#t)then{2}else{3}")))
  (minHS-parser (lex-this minHS-lexer input)))
|#
#|
Desired response:
;; (if-exp (bool-exp #t) (num-exp 2) (num-exp 3))
|#
#|
(display "\nExample 3: fun ([x:Int]:Int) => x\n")
(let ((input (open-input-string "fun ([x:Int]:Int) => x")))
  (minHS-parser (lex-this minHS-lexer input)))
|#
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x) (int-exp))) (int-exp)) (var-exp 'x))
|#
#|
(display "\nExample 4: fun ([f:Func Int Int]:Int) => f app 1\n")
(let ((input (open-input-string "fun ([f:Func Int Int]:Int) => f app 1")))
  (minHS-parser (lex-this minHS-lexer input)))
|#
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (int-exp) (int-exp)))) (int-exp)) (app-exp (var-exp 'f) (num-exp 1)))
|#
#|
(display "\nExample 5: fun ([f:Func (Func Int Bool) Int]:Bool) => #t\n")
(let ((input (open-input-string "fun ([f:Func (Func Int Bool) Int]:Bool) => #t")))
  (minHS-parser (lex-this minHS-lexer input)))
|#
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (par-exp (func-exp (int-exp) (boole-exp))) (int-exp)))) (boole-exp)) (bool-exp #t))
|#
#|
(display "\nExample 6: funF (sumita ([x:Int][y:Int]):Int) => x+y\n")
(let ((input (open-input-string "funF (sumita ([x:Int][y:Int]):Int) => x+y")))
  (minHS-parser (lex-this minHS-lexer input)))
|#
#|
Desired response:
(fun-f-exp (typeof-exp (var-exp 'sumita) (brack-exp (app-t-exp (typeof-exp (var-exp 'x) (int-exp)) (typeof-exp (var-exp 'y) (int-exp))))) (int-exp) (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#
#|
(display "\nExample 7: let ([x:Int = 1][y:Int = 2]) in x+y end\n")
(let ((input (open-input-string "let ([x:Int = 2][y:Int = 2]) in x+y end")))
  (minHS-parser (lex-this minHS-lexer input)))
|#
#|
Desired response:
(let-exp (brack-exp (app-t-exp (assign-exp (typeof-exp (var-exp 'x) (int-exp)) (num-exp 1)) (assign-exp (typeof-exp (var-exp 'y) (int-exp)) (num-exp 2)))) (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#

#|
(display "\nExample 8: ((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4\n")
(let ((input (open-input-string "((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4")))
|#
 ; (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(app-exp(par-exp(app-exp(par-exp(fun-f-exp(typeof-exp (var-exp 'sumita) (brack-exp (app-t-exp (typeof-exp (var-exp 'x) (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))))(int-exp)(prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y))))(num-exp 2)))(num-exp 4))
|#
