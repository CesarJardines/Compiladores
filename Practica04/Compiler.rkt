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

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out))

(define-tokens a (NUM
                  VAR
                  BOOL)) ;Tokens que son contenedores --- data Tokens = NUM Int | VAR String | BOOL Bool
(define-empty-tokens b (+ - * / AND OR 
                        BOOLE INT FUNC  
                        LP RP LS RS LK RK 
                        IF THEN ELSE 
                        LET IN END ASSIGN 
                        FUN TYPEOF ACA
                        FUNF 
                        APP  
                        EOF
                        BEGIN
                        ))
; Esta definicion pertenece a parsers-tools/lex-ptv-v200
; pero al importar esta biblioteca, causa problemas de sintaxis con los
; operadores aritmeticos
(define-lex-trans (epsilon stx)
  (syntax-case stx ()
    [(_) #'""]))

(define minHS-lexer
  (lexer
   ;[RegularExpression
   ;=>
   ;Token]
   ; prim 
   [(:: #\+)
    ;=>
    (token-+)]
   
   [(:: #\-)
    ;=>
    (token--)]
   
   [(:: #\*)
    ;=>
    (token-*)]
   
   [(:: #\/)
    ;=>
    (token-/)]
   
   [(:: #\a #\n #\d)
    ;=>
    (token-AND)]
   
   [(:: #\o #\r)
    ;=>
    (token-OR)]
   
   [(:: #\B #\o #\o #\l)
    ;=>
    (token-BOOLE)]
   
   [(:: #\I #\n #\t)
    ;=>
    (token-INT)]
   
   ;[(:: #\F #\u #\n #\c)
    ;=>
    ;(token-FUNC)]
   
   [(:: #\# #\t)
    ;=>
    (token-BOOL #t)]
   
   [(:: #\# #\f)
    ;=>
    (token-BOOL #f)]
   
   [(:: #\()
    ;=>
    (token-LP)]
   
   [(:: #\))
    ;=>
    (token-RP)]
   
   [(:: #\[)
    ;=>
    (token-LS)]
   
   [(:: #\])
    ;=>
    (token-RS)]
   
   [(:: #\{)
    ;=>
    (token-LK)]
   
   [(:: #\})
    ;=>
    (token-RK)]

   ;Se añade begin
   [(:: #\b #\e #\g #\i #\n)
    ;=>
    (token-BEGIN)]
   
   [(:: #\i #\f)
    ;=>
    (token-IF)]
   
   [(:: #\t #\h #\e #\n)
    ;=>
    (token-THEN)]
              
   [(:: #\e #\l #\s #\e)
    ;=>
    (token-ELSE)]
   
   [(:: #\l #\e #\t)
    ;=>
    (token-LET)]
   
   [(:: #\i #\n)
    ;=>
    (token-IN)]
   
   [(:: #\e #\n #\d)
    ;=>
    (token-END)]
   
   [(:: #\=)
    ;=>
    (token-ASSIGN)]
   
   [(:: #\f #\u #\n #\F)
    ;=>
    (token-FUNF)]
   
   [(:: #\f #\u #\n)
    ;=>
    (token-FUN)]
   
   [(:: #\:)
    ;=>
    (token-TYPEOF)]
   
   [(:: #\= #\>)
    ;=>
    (token-ACA)]
   
   [(:: #\a #\p #\p)
    ;=>
    (token-APP)]
   
   [(eof)
    (token-EOF)]
   [(:: (:or #\- (epsilon)) 
        (:: (:* (char-range #\0 #\9))
            (char-range #\0 #\9)))
    ; =>
    (token-NUM (string->number lexeme))]   
   [(:: (:+ (char-range #\a #\z)) ; a char
        (:or (:* (char-range #\a #\z)) ; more chars
             (char-range #\0 #\9) ; u num
             (:* (:: (char-range #\a #\z) (char-range #\0 #\9))) ;; chars and nums
             (epsilon))) ;; nothing
    ; =>
    (token-VAR (string->symbol lexeme))]

    [(:: (:+ (char-range #\a #\z)) ; a char
        (:or 
             (:* (:: (:* (char-range #\a #\z)) (:* (char-range #\0 #\9)))))) ;; chars and nums
             ;;(epsilon))) ;; nothing
    ; =>
    (token-VAR (string->symbol lexeme))]
   
   [whitespace ;Caso expecial
    ; =>
    (minHS-lexer input-port)] ;borramos todos los posibles espacios en blanco, tabuladores, etc
   ))

; Empecemos a definir la gramatica de minHS
; data minHS = NUM Int | ... | ADD minHS minHS | ...
(define-struct let-exp (var e1 e2) #:transparent) ; let(e1,x.e2) --- let x = e1 in e2 end
(define-struct bin-exp (op e1 e2) #:transparent) ; opb(e,e)
(define-struct un-exp (op e1) #:transparent) ; opu(e)
(define-struct par-exp (exp) #:transparent) ; (e)
(define-struct num-exp (n) #:transparent)
(define-struct var-exp (i) #:transparent)
; e :: = num | x | bool | opu(e) | opb(e,e) | fun [(x:T)]* e | ...

; Experimentos bonitos y romanticos
#|
(let ((input (open-input-string "holaaaa9a9a")))
  (minHS-lexer input))
(let ((input (open-input-string "ifi")))
  (minHS-lexer input))
(let ((input (open-input-string "then")))
  (minHS-lexer input))
(let ((input (open-input-string "else")))
  (minHS-lexer input))
(let ((input (open-input-string "if")))
  (minHS-lexer input))
(let ((input (open-input-string "-666")))
  (minHS-lexer input))
(let ((input (open-input-string "6")))
  (minHS-lexer input))
(let ((input (open-input-string "let")))
  (minHS-lexer input))
(let ((input (open-input-string "in")))
  (minHS-lexer input))
(let ((input (open-input-string "(")))
  (minHS-lexer input))
(let ((input (open-input-string "[")))
  (minHS-lexer input))
(let ((input (open-input-string "{")))
  (minHS-lexer input))
|#

#|
(let ((input (open-input-file "EjemplitoChido.mhs")))
  (begin
    (print (minHS-lexer input))
    (close-input-port input)))
|#


; Proximamente un parser
