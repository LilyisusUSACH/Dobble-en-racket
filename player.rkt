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

; Constructor

;Nombre Función: newPlayer
;Descripción: crea un player segun un nombre
;Dom: String
;Rec: player

(define newPlayer (lambda (nombre)
                    (list nombre 0 cartasVacias)))
; Selectores

;Nombre Función: getPlayerName
;Descripción: Obtiene el nombre de un player
;Dom: player
;Rec: String

(define getPlayerName car)

;Nombre Función: getPlayerScore
;Descripción: Obtiene el score de un player
;Dom: player
;Rec: Z+ + {0}

(define getPlayerScore cadr)

;Nombre Función: getPlayerCards
;Descripción: Obtiene las cards de un player
;Dom: player
;Rec: cards

(define getPlayerCards caddr)

; Modificadores

;Nombre Función: setPlayerScore
;Descripción: Modifica el score de un player
;Dom: player X Z+
;Rec: player

(define setPlayerScore (lambda (player newScore)
                         (list (getPlayerName player) newScore (getPlayerCards player))))

;Nombre Función: setPlayerCards
;Descripción: Modifica las cards de un player
;Dom: player X cards
;Rec: player

(define setPlayerCards (lambda (player cards)
                         (list (getPlayerName player) (getPlayerScore player) cards)))

;Nombre Función: addPlayerCard
;Descripción: Añade una card al cards de un player
;Dom: player X card
;Rec: player

(define addPlayerCard (lambda (player card)
                         (list (getPlayerName player) (getPlayerScore player) (addCardToCards card (getPlayerCards player)))))