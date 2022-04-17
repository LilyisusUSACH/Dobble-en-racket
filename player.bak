#lang racket

(require "cards.rkt")
(provide newPlayer)
(provide getPlayerName)
(provide getPlayerCards)

; TDA player

; null| Nombre X Cartas
; constructor
(define newPlayer (lambda (nombre)
                    (cons nombre cartasVacias)))
; selectores
(define getPlayerName car)
(define getPlayerCards cdr)
