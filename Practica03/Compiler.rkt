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
(provide (all-defined-out))
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens a (NUM VAR BOOLE )) ;Tokens que son contenedores --- data Tokens = NUM Int | VAR String | BOOL Bool
(define-empty-tokens b (+ - * / = and or :
                          APP EOF LET ASSIGN IN
                          LP RP LC RC LB RB
                          FUN FUNF IF THEN ELSE
                          ACA ATA
                          INT BOOL FUNC LIST)) ;Tokens que no almacenan datos

; "fun (x:T) : T => x"
; [FUN,LP,VAR "x",TYPEOF,TYPE "T",RP,TYPEOF,TYPE "T",ACA,VAR "x",ATA,EOF]
; TYPE (TYPE (TYPE ...))
; T ::= INT | BOOL | T -> T

; Esta definicion pertenece a parsers-tools/lex-ptv-v200
; pero al importar esta biblioteca, causa problemas de sintaxis con los
; operadores aritmeticos
(define-lex-trans (epsilon stx)
  (syntax-case stx ()
    [(_) #'""]))

; Nuestro hermosisimo lexer
(define minHS-lexer
  (lexer
   ;[RegularExpression
   ;=>
   ;Token]

   [#\0
    ;=>
    (token-VAR 'z)]

   ; :: s*
   [(:: #\l #\e #\t) ; Expresion regular let
    ; =>
    (token-LET)] ;token resultante

   ;[(:: #\a (:: #\s (:: #\s (:: #\i (:: #\g #\n)))))
    ; =>
    ;(cons '(token-ASSIGN) (minHS-lexer input-port))]

   [(:: #\a (:: #\s (:: #\s (:: #\i (:: #\g #\n)))))
    ; =>
    (token-ASSIGN)]

   [(:: #\l #\i #\s #\t)
   (token-LIST)]

   [(:: #\i #\n)
    ; =>
    (token-IN)]

   [(:: #\a #\p #\p)
    (token-APP)]

   [(:: #\# #\f)
    ;=>
    (token-BOOLE #f)]

   [(:: #\# #\t)
    ;=>
    (token-BOOLE #t )]

   [(:: #\I #\n #\t)
    ;=>
    (token-INT)]

   [(:: (:or #\B #\b) #\o #\o #\l)
    ;=>
    (token-BOOL)]

   [(:: (:or #\F #\f) #\u #\n #\c)
    ;=>
    (token-FUNC)]

   [(:: #\F #\u #\n #\c)
    (token-FUN)]

  [(:: (:or #\F #\f) #\u #\n #\f)
    ; =>
    (token-FUNF)]





   [(::  (:or #\- (epsilon)) (:: (:* (char-range #\0 #\9)) (:: (:or (:: #\. (char-range #\0 #\9)) (:: (char-range #\0 #\9)) #\.) (:* (char-range #\0 #\9)))))
    ; =>
    (token-NUM (string->number lexeme))]

   [#\+
    ; =>
    (token-+)]

   [#\-
    ; =>
    (token--)]

   [#\*
    ; =>
    (token-*)]

   [#\(
    ;=>
    (token-LP)
    ]

   [#\)
    ;=>
    (token-RP)
    ]

   [#\/
    ;=>
    (token-/)]

   [#\=
    ;=>
    (token-=)]

   [#\[
    ;=>
    (token-LC)]

   [#\]
    ;=>
    (token-RC)]

    [#\{
     ;=>
     (token-LB)]

    [#\}
     ;=>
     (token-RB)]

   [(:: #\o #\r)
    ;=>
    (token-or)]

   [(:: #\a #\n #\d)
    ;=>
    (token-and)]

     [(:: #\i #\f)
    ;=>
    (token-IF)]

    [(:: #\t #\h #\e #\n)
    ;=>
    (token-THEN)]

   [(:: #\e #\l #\s #\e)
    ;=>
    (token-ELSE)]

   [(:: #\= #\>)
    ;=>
    (token-ACA)]

   [(:: #\:)
    (token-:)]

   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) ; ([a..z]|[A..Z])^+
    ; =>
    (token-VAR (string->symbol lexeme))]

   ;<var>::= <car><digit>
   [(::(:or (char-range #\a #\z) (char-range #\A #\Z))(char-range #\0 #\9))
    ;=>
    (token-VAR (string->symbol lexeme))]

   ;<var>::= <car><var>
   [(::  (:+ (char-range #\a #\z))  (::(:+(::(char-range #\a #\z) (::(char-range #\0 #\9))))) )
    ;=>
    (token-VAR (string->symbol lexeme))]

   ;<var>::= <car><digit><var>
   [(::  (:+ (::(char-range #\a #\z) (::(char-range #\0 #\9))))  (::(:+(::(char-range #\a #\z) (::(char-range #\0 #\9))))))
    ;=>
    (token-VAR (string->symbol lexeme))]

   [whitespace ;Caso expecial
    ; =>
    (minHS-lexer input-port)] ;borramos todos los posibles espacios en blanco, tabuladores, etc

   [(eof) ;Token que indica que se termino de lexear la cadena
    (token-EOF)]))

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

;El objetivo de estos ejemplos se limita a mostrar que el programa detecta la aparición del primer
; token que encuentra.
(let ((input (open-input-string "3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "{3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "Funf 3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "(3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "if  3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "0.3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "[ 3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "]3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "-0.33 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "- 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "then 3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "* - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "assign - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "app - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "False 3 - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "Bool - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "Num - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "INT - 3.3 + 6")))
  (minHS-lexer input))

(let ((input (open-input-string "func - 3.3 + 6")))
  (minHS-lexer input))





;(let ((input (open-input-file "EjemplitoChido.mhs")))
 ;(begin
  ; (print (minHS-lexer input))
   ;(close-input-port input)))

; Proximamente un parser
