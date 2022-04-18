#lang racket

(require "player.rkt")
(provide playersVacio)
(provide firstPlayer)
(provide anotherPlayers)
(provide addPlayer)
(provide isPlayer?)
(provide cantidadPlayers)
(provide getnPlayer)
(provide addCardToPlayer)
(provide addCardsToPlayer)
(provide addPlayerToFinal)
; TDA PLAYERS
; null| Player X Players

; Constructor
(define playersVacio null)

; Selectores
(define firstPlayer car)
(define anotherPlayers cdr)
(define cantidadPlayers length)
(define getnPlayer (lambda (pls n)
                     (list-ref pls n)))
; Modificadores

(define addPlayer (lambda (nombre pls)
                    (cons (newPlayer nombre) pls)
                    ))

(define addPlayerToFinal append)
  
(define addCardToPlayer (lambda (nombre carta pls resultado)
                          (if (null? pls)
                              resultado
                              (if (equal? (getPlayerName (firstPlayer pls)) nombre)
                                  (addCardToPlayer nombre carta (anotherPlayers pls)
                                                   (addPlayerToFinal resultado (list (setPlayerScore (addPlayerCard (firstPlayer pls) carta) (+ 1 (getPlayerScore (firstPlayer pls) ))))))
                                  (addCardToPlayer nombre carta (anotherPlayers pls) (addPlayerToFinal resultado (list (firstPlayer pls)) ))
                                  ))))

(define addCardsToPlayer (lambda (nombre cartas pls)
                           (if (null? cartas)
                               pls
                               (addCardsToPlayer nombre (cdr cartas) (addCardToPlayer nombre (car cartas) pls null)))))
                                
; otros
(define isPlayer? (lambda (nombre pls) 
                    (if (null? pls)
                        #f
                        (if (equal? (getPlayerName (firstPlayer pls)) nombre)
                            #t
                            (isPlayer? nombre (anotherPlayers pls))
                            ))
                    ))
(define p1 (addPlayer "fidea"  (addPlayer "fideo" (addPlayer "juan" (addPlayer "jose" playersVacio)))))