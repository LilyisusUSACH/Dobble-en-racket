#lang racket

; TDA CARTA
; Representacion
; Carta = CartaVacia | lista de N simbolos

; Constructor
(define cartaVacia null)
(define cartaInicial list)
; Pertenencia
(define cartaVacia? null?)

; Selectores

; Modificadores
(define addSymbol (lambda (C x)
                  (cons C x)))

; TDA CARTAS
; representacion
; Cartas = CartasVacia | Lista de N carta

; Constructor
(define cartasVacias null)
; Pertenencia
(define cartasVacias? null?)
;selectores
(define getnCard (lambda (Cs n)
                   (list-ref Cs n)))
(define largo length)                
;modificadores
(define addCard (lambda (C Cs)
                  (cons C Cs)))

(define card (lambda (carta i n)
               (if (not (= i (+ 2 n)))
                        (card (addSymbol i carta) (+ 1 i) n)
                        carta)))

(define cartaN (lambda (carta k j n)
                 (if (not(= k (+ 1 n)))
                     (cartaN (addSymbol (+(* n j)(+ k 1)) carta) (+ k 1) j n)
                     carta)))

(define cartas-n (lambda (tope j cartas)
                   (if (not(= (+ 1 tope) j))
                       (cartas-n tope (+ j 1) (addCard (cartaN (addSymbol 1 cartaVacia) 1 j tope) cartas))
                       cartas
                   )))

(define cartaNN_int (lambda (carta i j k n)
                      (if (not(= k (+ 1 n)))
                          (cartaNN_int (addSymbol (+ n 2 (* n (- k 1)) (remainder (- (+(* (- i 1) (- k 1)) j) 1) n)) carta)
                                  i j (+ 1 k) n)
                          carta)))

(define cartasNN (lambda(cartas i j n)
                   (if (not(= j (+ 1 n)))
                       (cartasNN (addCard (cartaNN_int(addSymbol (+ 1 i) cartaVacia) i j 1 n) cartas) i (+ j 1) n)
                        cartas)))


(define cartas-nn (lambda (tope i cartas)
                    (if (not (= i (+ 1 tope)))
                        (cartas-nn tope (+ 1 i)(cartasNN cartas i 1 tope))
                        cartas)))

(define setCard (lambda (n)
                  (cartas-nn n 1
                       (cartas-n n 1
                            (addCard (card cartaVacia 1 n) cartasVacias )))))

; FUNCION QUE VERIFICA SI N ES PRIMO
(define is_primo (lambda (i n)
                                          (if (= i (- n 1))
                                              #f
                                              (if (= (remainder n i) 0)
                                                  #t
                                                  (is_primo (+ 1 i) n)))))
(define es_primo? (lambda (n)
                   (if (or (= n 1) (= n 2) (= n 3))
                       #t
                       (not(is_primo 2 n))
                       )
                   ))
                         

; ARMANDO LA FUNCION PRINCIPAL

(define cardSet (lambda (simbolos n)
                  (if (null? simbolos)
                      (setCard n)
                      (linkear simbolos (setCard n)))))

(define linkear(lambda (simbolos cartas)
                 (if (not ( = (length simbolos) (length cartas)))
                     null
                     (asociar simbolos cartas cartasVacias))))

(define eachCard (lambda (simbolos carta newCarta)
                   (if (null? carta)
                       newCarta
                       (eachCard simbolos (cdr carta) (addSymbol (list-ref simbolos (- (car carta) 1)) newCarta)))))
                   
(define asociar (lambda (simbolos cartas newCards)
                  (if (null? cartas)
                      newCards
                      (asociar simbolos (cdr cartas) (addCard (eachCard simbolos (car cartas) cartaVacia) newCards)))))

; Falta terminar de implementar esto
(define cardSet->string(lambda (Cs)
                         (map (lambda (L) (string-join
                                           (addCard "\n" (addCard "C" (map (lambda (C) (number->string C)) L)))
                                                   ))Cs)))

; FUNCION Q "ALEATORIZA" LAS CARTAS
(define rand (lambda (L)
               (define randomize(lambda (L newL i)
                 (if (not (null? L))
                     (if (or (=(remainder i 2)0)) ;probe ocupando el randomFn pero no
                         (randomize (cdr L) (addSymbol (car L) newL) (+ i 1))
                         (randomize (cdr L) (addSymbol (car L) (reverse newL)) (+ i 1))
                         )
                     newL
                 )))
               (randomize L cartasVacias 0)))

; Funcion que corte las cartas en n elementos
(define cut (lambda (L n)
             (if (or (null? L) (> n (largo L)))
                 null
                 (cortar L n)
                 )))

(define cortar (lambda (L n)
                 (if (= n (largo L))
                     L
                     (if (= n (largo (cdr L)))
                         (cdr L)
                         (cortar (cdr L)  n)))))
                     
(define newCardSet (lambda (simbolos n cantCard)
                     (cut (rand (cardSet simbolos n)) cantCard)))