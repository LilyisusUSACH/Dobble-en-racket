#lang racket

(provide cartaVacia)
(provide cartaInicial)
(provide cartaVacia?)
(provide addSymbol)

; TDA CARTA

; Representacion
; Carta = null | lista de N simbolos

; Constructor

;Nombre Función: cartaVacia
;Descripción: Crea un card vacio
;Dom: - no recibe nada
;Rec: card

(define cartaVacia null)

;Nombre Función: cartaInicial
;Descripción: Crea una carta con elementos entregados
;Dom: Cualquier elemento o simbolo
;Rec: card

(define cartaInicial list)

;Nombre Función: addSymbol
;Descripción: añade un simbolo o elemento a un card
;Dom: card X elemento/simbolo
;Rec: card

(define addSymbol (lambda (C x)
                  (cons C x)))

; Pertenencia

;Nombre Función: cartaVacia?
;Descripción: verifica si es una card vacia
;Dom: card
;Rec: boolean -> #t o #f

(define cartaVacia? null?)

; Selectores

; Modificadores

