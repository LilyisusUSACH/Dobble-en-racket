#lang racket

(require "cards.rkt")
(require "card.rkt")
(require "aux.rkt")


; TDA CardSet

; Constructor

(define setCard (lambda (n)
                  (define card (lambda (carta i n)
                                 (if (not (= i (+ 2 n)))
                                     (card (addSymbol i carta) (+ 1 i) n)
                                     carta)))
                  (define cartas-n (lambda (tope j cartas)
                                     (define cartaN (lambda (carta k j n)
                                                      (if (not(= k (+ 1 n)))
                                                          (cartaN (addSymbol (+(* n j)(+ k 1)) carta) (+ k 1) j n)
                                                          carta)))
                                     (if (not(= (+ 1 tope) j))
                                         (cartas-n tope (+ j 1) (addCard (cartaN (addSymbol 1 cartaVacia) 1 j tope) cartas))
                                         cartas
                                         )))
                  (define cartas-nn (lambda (tope i cartas)
                                      (define cartasNN (lambda(cartas i j n)
                                                         (define cartaNN_int (lambda (carta i j k n)
                                                                               (if (not(= k (+ 1 n)))
                                                                                   (cartaNN_int (addSymbol (+ n 2 (* n (- k 1)) (remainder (- (+(* (- i 1) (- k 1)) j) 1) n)) carta)
                                                                                                i j (+ 1 k) n)
                                                                                   carta)))
                                                         (if (not(= j (+ 1 n)))
                                                             (cartasNN (addCard (cartaNN_int(addSymbol (+ 1 i) cartaVacia) i j 1 n) cartas) i (+ j 1) n)
                                                             cartas)))
                    
                                      (if (not (= i (+ 1 tope)))
                                          (cartas-nn tope (+ 1 i)(cartasNN cartas i 1 tope))
                                          cartas)))
                  (cartas-nn n 1
                             (cartas-n n 1
                                       (addCard (card cartaVacia 1 n) cartasVacias)))))

(define newCardSet (lambda (simbolos n cantCard)
                     (define cardSet (lambda (simbolos n)
                                       (if (null? simbolos)
                                           (setCard n)
                                           (linkear simbolos (setCard n)))
                                       ))
                     
                     (if (not (= n 0))
                         (cons (cut (rand (cardSet simbolos (- n 1))) cantCard) simbolos)
                         null)))

(define linkear(lambda (simbolos cartas)
                 (define asociar (lambda (simbolos cartas newCards)
                                   (define eachCard (lambda (simbolos carta newCarta)
                                                      (if (null? carta)
                                                          newCarta
                                                          (eachCard simbolos (cdr carta) (addSymbol (list-ref simbolos (- (car carta) 1)) newCarta)))))
                                   (if (null? cartas)
                                       newCards
                                       (asociar simbolos (cdr cartas) (addCard (eachCard simbolos (car cartas) cartaVacia) newCards)))))
                 
                 (if (not (>= (length simbolos) (length cartas)))
                     null
                     (asociar simbolos cartas cartasVacias))))
; Selectores
(define getCards car)
(define getSymbols cdr)
(define nthCard (lambda (n cardSet)
           (getnCard (getCards cardSet) n)))

; FUNCION Q "ALEATORIZA" LAS CARTAS
(define rand (lambda (L)
               (define randomize(lambda (L newL i)
                                  (if (not (null? L))
                                      (if (or (=(remainder i 2)0))
                                          (randomize (cdr L) (addSymbol (car L) newL) (+ i 1))
                                          (randomize (cdr L) (addSymbol (car L) (reverse newL)) (+ i 1))
                                          )
                                      newL
                                      )))
               (randomize L cartasVacias 0)))

; Funcion que corte las cartas en n elementos
(define cut (lambda (L n)
              (define cortar (lambda (L n)
                               (if (= n (largo L))
                                   L
                                   (if (= n (largo (cdr L)))
                                       (cdr L)
                                       (cortar (cdr L)  n)))))
              
              (if (or (null? L) (> n (largo L)))
                  null
                  (if (<= 0 n)
                      (cortar L n)
                      L)
                  )))
;Pertenencia

(define dobble? (lambda (setcartas)
                  (define coinciden (lambda (setcartas completesetcartas bool)
                                      (if (not (null? setcartas))
                                          (coinciden (cdr setcartas)
                                                     completesetcartas (and (ormap (lambda (x)
                                                                                     (set=? (car setcartas) x)) completesetcartas) bool))
                                          bool
                                          )))

                  (if (null? (car setcartas))
                      #f
                      (coinciden (car setcartas) (car (newCardSet (cdr setcartas) (largo (car (car setcartas))) -1)) #t)
                      )))

; Otros

(define findTotalCards (lambda (carta)
                         (define n (- (largo carta) 1))
                         (+(* n n) n 1)))

(define requiredElements (lambda (carta)
                           (define n (largo carta))
                           (+(* n n) n 1)))

(define cardSet->string(lambda (setcartas)
                         (define string-eachcard
                           (lambda (setcartas i string)
                             (if (not (null? setcartas))
                                 (string-eachcard (cdr setcartas) (+ 1 i)
                                                  (string-append "C" (~a i) ": "
                                                                 (string-join (map (lambda(x) (string-append (~a x) " -")) (car setcartas))) "\n" string))
                                 string
                                 )))
                         (string-eachcard (car setcartas) 0 "")
                         ))

(define missingCards (lambda (cartas)
                       (define esta-en (lambda (cartas carta)
                                         (if (null? cartas)
                                             #f
                                             (or (set=? (car cartas) carta) (esta-en (cdr cartas) carta))
                                             )))
                       
                       (if (= (largo (car cartas)) (findTotalCards (car (car cartas))))
                           null
                           (filter (lambda (x)
                                     (if (esta-en (car cartas) x)
                                         #f
                                         #t
                                         ))
                                   (car (newCardSet (cdr cartas) (largo (car (car cartas))) -1)))
                           )))

(define error '(((1 2)) "A" "B" "C"))
