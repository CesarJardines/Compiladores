#lang nanopass

;;César Eduardo Jardines Mendoza
;;314071549

;;Espero te guste mi práctica, la hice con mucho amor y tiempo
;;No pude acabar el ejercicio 3 por lo que no lo anexo

;;Ejercicio 1
(define (hamming l1 l2)
         (if (and (empty? l1)(empty? l2))
             0
             (if (equal? (first l1) (first l2))
                 (hamming (rest l1) (rest l2))
                 (+ 1 (hamming (rest l1) (rest l2))))))

;;Funición auciliar para el ejercicio 2a y 2b
(define (bis n)
  (if(= (modulo n 4) 0)
     (if (= (modulo n 100)0)
         (if (= (modulo n 400) 0)
             366
             365) 
          366) 
     365)
  )
;;Función auxiliar para ejercicio 2a 
(define (fun n m)
  (if (<= n m)
      (if (= (bis n) 366)
          (+ 366 (fun (+ 1 n) m))
          (+ 365 (fun (+ 1 n) m)))
      0))
;;Ejercicio 2a
(define (viernes n m)
  (if (= n m)
      (floor(/(fun n m)7))
      (ceiling(/(fun n m)7)))) 

;;Función auxiliar para ejercicio 2b
(define (foo m n)
  (cond
    [(= (modulo m 12) 1) 31]
    [(= (modulo m 12) 2) (if (= (bis n) 366) 29 28)]
    [(= (modulo m 12) 3) 31]
    [(= (modulo m 12) 4) 30]
    [(= (modulo m 12) 5) 31]
    [(= (modulo m 12) 6) 30]
    [(= (modulo m 12) 7) 31]
    [(= (modulo m 12) 8) 31]
    [(= (modulo m 12) 9) 30]
    [(= (modulo m 12) 10) 31]
    [(= (modulo m 12) 11) 30]
    [(= (modulo m 12) 0) 31]
    ))

;;Ejecicio 2b
;;Es d el día de la semana y m el mes con el que empezó 1901, en este caso empezó el año siendo martes(2) del mes enero(1)
;; (domingo 1901 2000 2 1) 
(define (domingo a1 a2 d m)
  (if (>= a2 a1)
      (if (= (modulo (+ d (foo m a1 )) 7) 0) 
          ;;then
          (if(= m 12)
             (+ 1 (domingo (+ 1 a1) a2 (modulo (+ d (foo m a1 )) 7) 1))
             (+ 1 (domingo a1 a2 (modulo (+ d (foo m a1 )) 7) (+ 1 m))))  
          ;;else
          (if (= m 12)
              (domingo(+ 1 a1) a2 (modulo (+ d (foo m a1 )) 7) 1)
              (domingo a1 a2 (modulo (+ d (foo m a1 )) 7) (+ 1 m)))) 
      0))

;;Función auxiliar para ejercicio 4
(define (l-prime n)
  (if (= n 3)
      '(2 3)
      (let ([lst (l-prime (sub1 n))])
            (cond
              [(prime? n lst) (append lst (list n))]
              [else lst]))))

;;Función auxiliar para ejercicio 4
(define (prime? n l)
    (cond
      [(empty? l) #t]
      [(zero? (modulo n (car l))) #f]
      [else (prime? n (cdr l))]))

;; Struct de ejercicio 4a
(struct node (x left right) #:transparent)
(struct leaf (x) #:transparent)

;;Ejercicio 4a
(define (div-tree n)
  (Ntree n))

;;Funcion auxiliar para ejercicio 4a
(define (derechaSig n)
  (let ([lst (l-prime n)])
    (if(member n lst)
       (leaf n)
       (Ntree n))))

;;Función auxiliar para ejercicio 4a
(define (Dhojas l n)
  (if (zero? (modulo n (car l)))
      (car l)
      (Dhojas (cdr l) n)))

;;Función auxiliar para ejercicio 4a
(define (Ntree n)
  (let* ([lst (l-prime n)]
         [hoja (Dhojas lst n)]) (node n (leaf hoja) (derechaSig (quotient n hoja)))))

;;Función auxiliar para ejercicio 4b
(define (factor n)
  (define (*factor divisor n)
    (if (> (* divisor divisor) n)
        (list n)
        (if (= (modulo n divisor) 0)
            (cons divisor (*factor divisor (/ n divisor)))
            (*factor (+ divisor 1) n))))
  (*factor 2 n))

;;Función auxiliar para ejercicio 4b
(define (multiplica l n a)
  (if (null? l) ;; vacia
      (list a)
      (if (equal? n (list-ref l 0)) ;; iguales
          (multiplica (list-tail l 1) n (* n a))
          (append (list a) (multiplica (list-tail l 1) (list-ref l 0) (list-ref l 0))))))

;;Ejercicio 4b
(define (prime-fac n)
  (multiplica (factor n) 2 1))

