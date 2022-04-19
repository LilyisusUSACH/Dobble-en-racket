#lang racket

(provide gameAreaVacia)
(provide addCardToArea)
(provide firstArea)
(provide restoArea)
(provide isSymbolInArea?)
(provide isSymbolInAreaCards?)

; TDA area
; lista de cartas

; null| Card x Area

; contructor


;Nombre Función: gameAreaVacia
;Descripción: Crea un area vacia
;Dom: - no recibe nada
;Rec: area

(define gameAreaVacia null)

;Nombre Función: addCardToArea
;Descripción: añade una card a un area
;Dom: card X area
;Rec: area

(define addCardToArea (lambda (C Area)
                        (cons C Area)))

; Selector

;Nombre Función: firstArea
;Descripción: devuelve la primera card de un area
;Dom: area
;Rec: card

(define firstArea car)

;Nombre Función: restoArea
;Descripción: devuelve todas las card de un area excepto la primera
;Dom: area
;Rec: area

(define restoArea cdr)

;Nombre Función: isSymbolInArea?
;Descripción: verifica si un elemento o simbolo
; esta en alguna de las card del area
;Dom: Simbolo/elemento (puede ser cualquier cosa) X area
;Rec: boolean -> #t o #f
;Tipo Recursión: Cola

(define isSymbolInArea? (lambda (simbolo area)
                          (if (null? area)
                              #f
                              (or (not (equal? #f (member simbolo (firstArea area)))) (isSymbolInArea? simbolo (restoArea area)))
                          )))

;Nombre Función: isSymbolInAreaCards?
;Descripción: verifica si un elemento o simbolo
; esta en TODAS de las card del area
;Dom:  Simbolo/elemento (puede ser cualquier cosa) X area
;Rec: boolean -> #t o #f

(define isSymbolInAreaCards? (lambda (simbolo area)
                               (andmap (lambda (x) (not (equal? #f (member simbolo x)))) area)))