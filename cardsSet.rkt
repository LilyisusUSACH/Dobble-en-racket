#lang racket

(require "cards.rkt")
(require "card.rkt")
(require "auxi.rkt")
(provide setCard)
(provide emptyCardsSet)
(provide cardsSet)
(provide dobble?)
(provide getCards)
(provide getSymbols)
(provide numCards)
(provide nthCard)
(provide requiredElements)
(provide findTotalCards)
(provide cardsSet->string)
(provide missingCards)
(provide addCard)
(provide addCardToFinal)
(provide addCardToTop)

; TDA CardSet
; emptyCardsSet | Cartas x list (de simbolos)

; Constructor

;Nombre Función: emptyCardsSet
;Descripción:  crea una version vacia del cardsSet
;Dom: - no recibe nada
;Rec: CardsSet

(define emptyCardsSet (cons cartasVacias null))

;Nombre Función: setCard
;Descripción: Crea segun un algoritmo internamente un "Cards"
;             Que despues servira para ser un set de dobble
;Dom: Z+
;Rec: Cards
;Tipo Recursión: Cola

(define setCard(lambda (n)
                 (define 1card (lambda (carta i n)
                                (if (not (= i (+ 2 n)))
                                    (1card (addSymbol i carta) (+ 1 i) n)
                                    carta)))
                 (define cartas-n (lambda (tope j cartas)
                                    (define cartaN (lambda (carta k j n)
                                                     (if (not(= k (+ 1 n)))
                                                         (cartaN (addSymbol (+(* n j)(+ k 1)) carta) (+ k 1) j n)
                                                         carta)))
                                    (if (not(= (+ 1 tope) j))
                                        (cartas-n tope (+ j 1) (addCardToCards (cartaN (addSymbol 1 cartaVacia) 1 j tope) cartas))
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
                                                            (cartasNN (addCardToCards (cartaNN_int(addSymbol (+ 1 i) cartaVacia) i j 1 n) cartas) i (+ j 1) n)
                                                            cartas)))
                    
                                     (if (not (= i (+ 1 tope)))
                                         (cartas-nn tope (+ 1 i)(cartasNN cartas i 1 tope))
                                         cartas)))
                 (cartas-nn n 1
                            (cartas-n n 1
                                      (addCardToCards (1card cartaVacia 1 n) cartasVacias)))))


;Nombre Función: setCard
;Descripción: Define los limites del cardsSet para despues llamar
;              A la funcion que lo crea, ademas lo recorta y 
;Dom: Lista X Z+ X Z+ + {0} X funcion
;Rec: cardsSet

(define cardsSet (lambda (simbolos n cantCard fnrandom)
                     (define cardSet (lambda (simbolos n)
                                       (if (null? simbolos)
                                           (setCard n)
                                           (linkear simbolos (setCard n)))
                                       ))      
                     (if (not (= n 0))
                         (cons (cut (rand (cardSet simbolos (- n 1)) fnrandom 1) cantCard) simbolos)
                         null)))


;Nombre Función: Linkear
;Descripción: Si hay simbolos, asociada cada simbolo a un numero dentro de las cartas
;Dom: list de simbolos, cards
;Rec: cards
;Tipo Recursión: Cola

(define linkear(lambda (simbolos cartas)
                 (define asociar (lambda (simbolos cartas newCards)
                                   (define eachCard (lambda (simbolos carta newCarta)
                                                      (if (null? carta)
                                                          newCarta
                                                          (eachCard simbolos (cdr carta) (addSymbol (list-ref simbolos (- (car carta) 1)) newCarta)))))
                                   (if (null? cartas)
                                       newCards
                                       (asociar simbolos (cdr cartas) (addCardToCards (eachCard simbolos (car cartas) cartaVacia) newCards)))))

                 (define rellenarSimbolos (lambda (simbolos n-simbolos)          
                                            (simbolos-hasta-n n-simbolos simbolos)))
                 
                 (if (not (>= (length simbolos) (length cartas) ))
                     (asociar (rellenarSimbolos simbolos (- (length cartas) (length simbolos))) cartas cartasVacias) ; MUCHO CUIDADO CON ESTO
                     (asociar simbolos cartas cartasVacias))))
; Pertenencia

; errores cuando hay simbolos que autocompletar | sets no creados, si no que insertados
; si hay tiempo arreglar

;Nombre Función: Dobble?
;Descripción: comprueba si es un dobble valido
;Dom: cardsSet
;Rec: boolean, #t o #f

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
                      (coinciden (car setcartas) (car (cardsSet (cdr setcartas) (largo (car (car setcartas))) -1 randomFn)) #t)
                      )))
                                             

; Selectores

;Nombre Función: getCards
;Descripción: devuelve las cards de un cardsSet
;Dom: cardsSet
;Rec: cards

(define getCards car)

;Nombre Función: getSymbols
;Descripción: devuelve una lista de elementos
;Dom: cardsSet
;Rec: list

(define getSymbols cdr)

;Nombre Función: numCards
;Descripción: Calcular el total de cartas que tiene un cardsSet
;Dom: cardsSet
;Rec: Z+
(define numCards (lambda (setcartas)
                   (largo (getCards setcartas))))

;Nombre Función: nthCard
;Descripción: devuelve la carta que esta en una posicion n de un cardsSet
;Dom: cardsSet
;Rec: card

(define nthCard (lambda (n cardSet)
                  (getnCard (getCards cardSet) n)))

; Otros

;Nombre Función: findTotalCards
;Descripción: Calcular el total de cartas que deberia tener un cardsSet completo
;             a partir de una carta
;Dom: card
;Rec: Z+

(define findTotalCards (lambda (carta)
                         (define n (- (largo carta) 1))
                         (+(* n n) n 1)))

;Nombre Función: findTotalCards
;Descripción: Calcular el total de elementos que deberia tener un cardsSet completo
;             a partir de una carta
;Dom: card
;Rec: Z+

(define requiredElements (lambda (carta)
                           (define n (- (largo carta) 1))
                           (+(* n n) n 1)))

;Nombre Función: cardSet->string
;Descripción: Devuelve un formato en string de un cardsSet
;             Para posteriormente ser usado con un display
;Dom: cardsSet
;Rec: String
;Tipo Recursión: Cola

(define cardsSet->string(lambda (setcartas)
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

;Nombre Función: missingCards
;Descripción: Devuelve las cartas faltantes para que un cardsSet
;             Sea un cardsSet completo
;Dom: cardsSet
;Rec: cardsSet

(define missingCards (lambda (cartas)
                       (if (not(null? (getCards cartas)))
                           (if (= (largo (car cartas)) (findTotalCards (car (car cartas))))
                               null
                               (cons (filter (lambda (x)
                                               (if (esta-en (car cartas) x)
                                                   #f
                                                   #t
                                                   ))
                                             (car (cardsSet (cdr cartas) (largo (car (car cartas))) -1 randomFn))) (getSymbols cartas))
                               )
                           null
                           )))

(define error '(((1 2)) "A" "B" "C"))

; Punto 18

;Nombre Función: addCard
;Descripción: añade una carta a un set, verificando que cumpla con ser dobble
;Dom: cardsSet X card
;Rec: cardsSet
;Tipo Recursión: Cola

(define addCard (lambda (setcartas carta)
                  (define setConCarta (lambda (setcartas carta)
                                        (if (esta-en (getCards setcartas) carta)
                                            setcartas
                                           (cons (addCardToCards carta (getCards setcartas)) (set-union carta (reverse (getSymbols setcartas))))))) ;Hacerlo sin union

                  (define resultado (setConCarta setcartas carta))
                  
                  (if (dobble? resultado)
                      resultado
                      setcartas)
                  ))

;Nombre Función: addCardToTop
;Descripción: añade una card en el top del cardsSet sin verificar
; ya que se usara con cartas que estan verificadas, ya que fueron
; Sacadas directamente del set creado
;Dom: cardsSet X card
;Rec: cardsSet

(define addCardToTop (lambda (setcartas carta)
                         (cons (addCardToCards carta (getCards setcartas) ) (getSymbols setcartas))))

;Nombre Función: addCardToFinal
;Descripción: añade una card al final de un cardsSet igual que la anterior
;Dom: cardsSet X card
;Rec: cardsSet

(define addCardToFinal (lambda (setcartas carta)
                         (cons (addCardFinalToCards carta (getCards setcartas)) (getSymbols setcartas))))