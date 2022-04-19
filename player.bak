#lang racket

(require "cards.rkt")
(provide newPlayer)
(provide getPlayerName)
(provide getPlayerScore)
(provide getPlayerCards)
(provide addPlayerCard)
(provide setPlayerScore)

; TDA player

; null| Nombre X puntaje X Cartas
; constructor
(define newPlayer (lambda (nombre)
                    (list nombre 0 cartasVacias)))
; selectores
(define getPlayerName car)
(define getPlayerScore cadr)
(define getPlayerCards caddr)

; Modificadores
(define setPlayerScore (lambda (player newScore)
                         (list (getPlayerName player) newScore (getPlayerCards player))))

(define setPlayerCards (lambda (player cards)
                         (cons (getPlayerName player) (getPlayerScore player) cards)))

(define addPlayerCard (lambda (player card)
                         (list (getPlayerName player) (getPlayerScore player) (addCardToCards card (getPlayerCards player)))))