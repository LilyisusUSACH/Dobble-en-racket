#lang racket

(require "player.rkt")
(provide playersVacio)
(provide firstPlayer)
(provide anotherPlayers)
(provide addPlayer)
(provide isPlayer?)

; TDA PLAYERS
; null| Player X Players

; Constructor
(define playersVacio null)

; Selectores
(define firstPlayer car)
(define anotherPlayers cdr)
; Modificadores

(define addPlayer (lambda (nombre pls)
                    (cons (newPlayer nombre) pls)))

; otros
(define isPlayer? (lambda (nombre pls) 
                    (if (null? pls)
                        #f
                        (if (equal? (getPlayerName (firstPlayer pls)) nombre)
                            #t
                            (isPlayer? nombre (anotherPlayers pls))
                            ))
                    ))
                     