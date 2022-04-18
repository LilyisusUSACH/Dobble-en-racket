#lang racket

(provide cartasVacias)
(provide cartasVacias?)
(provide getfirstCard)
(provide getnCard)
(provide largo)
(provide addCardToCards)
(provide addCardFinalToCards)

; TDA CARTAS
; representacion
; Cartas = CartasVacia | Lista con cartas
;              null    | carta X Cartas

; Constructor
(define cartasVacias null)

(define addCardToCards (lambda (C Cs)
                  (cons C Cs)))

(define addCardFinalToCards (lambda (C Cs)
                             (if (null? Cs)
                                 (cons C null)
                                 (cons (car Cs) (addCardFinalToCards C (cdr Cs))))))

; Pertenencia
(define cartasVacias? null?)

;selectores
(define getfirstCard caar)
(define getAnotherCards cadr)

(define getnCard (lambda (Cs n)
                   (list-ref Cs n)))

(define largo length)                
;modificadores

                       