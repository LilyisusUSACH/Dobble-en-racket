#lang racket

(provide gameAreaVacia)
(provide addCardToArea)
(provide firstArea)
(provide restoArea)
(provide isSymbolInArea?)
(provide isSymbolInAreaCards?)

; TDA area de juego ?
; lista de cartas

; null| Card x Area

; contructor
(define gameAreaVacia null)

; Modificador
(define addCardToArea (lambda (C Area)
                        (cons C Area)))

; Selector

(define firstArea car)
(define restoArea cdr)

(define isSymbolInArea? (lambda (simbolo area)
                          (if (null? area)
                              #f
                              (or (not (equal? #f (member simbolo (firstArea area)))) (isSymbolInArea? simbolo (restoArea area)))
                          )))

(define isSymbolInAreaCards? (lambda (simbolo area)
                               (andmap (lambda (x) (not (equal? #f (member simbolo x)))) area)))