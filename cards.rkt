#lang racket

(provide cartasVacias)
(provide cartasVacias?)
(provide getfirstCard)
(provide getnCard)
(provide largo)
(provide addCardToCards)
(provide addCardFinalToCards)

; TDA Cards
; representacion
; CartasVacia | Lista con card
;     null    | card X cards

; Constructor

;Nombre Función: cartasVacias
;Descripción: Crea un cards Vacio
;Dom: - no recibe nada
;Rec: cards

(define cartasVacias null)

;Nombre Función: addCardToCards
;Descripción: Añade una card a cards
;Dom: card X cards
;Rec: cards

(define addCardToCards (lambda (C Cs)
                  (cons C Cs)))

;Nombre Función: addCardFinalToCards
;Descripción: añade una card al final de cards
;Dom: card X cards
;Rec: cards
;Tipo Recursión: natural

(define addCardFinalToCards (lambda (C Cs)
                             (if (null? Cs)
                                 (cons C null)
                                 (cons (car Cs) (addCardFinalToCards C (cdr Cs))))))

; Pertenencia

;Nombre Función: cartasVacias?
;Descripción: Consulta si es un cards vacio
;Dom: cards
;Rec: boolean -> #t o #f

(define cartasVacias? null?)

;selectores

;Nombre Función: getfirstCard
;Descripción: obtiene la primera card de cards
;Dom: cards
;Rec: card

(define getfirstCard car)

;Nombre Función: getAnotherCards
;Descripción: Obtiene todas las cards menos la primera
;Dom: cards
;Rec: cards

(define getAnotherCards cdr)

;Nombre Función: getnCard
;Descripción: obtiene la card en la posicion n de cards
;Dom: cards X Z+ + {0}
;Rec: card

(define getnCard (lambda (Cs n)
                   (list-ref Cs n)))

;Nombre Función: largo
;Descripción: Obtiene la cantidad de cartas de un cards
;Dom: cards
;Rec: Z+ + {0}

(define largo length)                

                       