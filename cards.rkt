#lang racket

(provide cartasVacias)
(provide getfirstCard)
(provide getnCard)
(provide largo)
(provide addCard)

; TDA CARTAS
; representacion
; Cartas = CartasVacia | Lista de N carta

; Constructor
(define cartasVacias null)
; Pertenencia
(define cartasVacias? null?)

;selectores
(define getfirstCard caar)
(define getnCard (lambda (Cs n)
                   (list-ref Cs n)))

(define largo length)                
;modificadores
(define addCard (lambda (C Cs)
                  (cons C Cs)))