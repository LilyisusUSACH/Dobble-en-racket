#lang racket

(require "card.rkt")
(require "cards.rkt")
(require "cardsSet.rkt")
(require "player.rkt")
(require "players.rkt")
(require "area.rkt")

; Para los turnos hace un modulo del n° jugadores

; Modos de juego:

; Uno donde se va comparando con la carta superior
; Se va guardando en los mazos de los jugadores (abajo), cada jugador
; Empieza con una, el resto en el medio
; Gana el que tiene mas cartas
; Score = cartas del jugador

; Uno donde cada jugador tiene igual cantidad de
; Cartas, una al centro y gana el que se queda sin cartas
; Score = Total de cartas entregadas - cartas restantes

; Una para cada uno, se compara con las del resto, si coincide se le entrega
; Cuando se le entrega queda en el top
; Gana el que tiene menos cartas al terminar el juego


; TDA Game
; rec : n°players X cardsSet X modo (FUNCION) X funcion random
; Area de juego , cardSet , turno , max Jugadores, jugadores registrados (con sus cartas), estado juego, modo , randomfn

         
; Constructor
(define gameVacio (list gameAreaVacia emptyCardsSet 0 0 null "No Iniciado" null null))

; Estado si esta "Terminado" no se puede seguir jugando.

(define game(lambda (n_players setcartas modo randomfn)
              (if (and (positive? n_players) (dobble? setcartas) (procedure? modo)(procedure? randomfn))
                  (list gameAreaVacia setcartas 0 n_players playersVacio "No iniciado" modo randomfn)
                  gameVacio
                  )))
(define game_to_modify list)


; Selectores
(define getArea car)
(define getSetCartas cadr)
(define getTurn caddr)
(define getMaxPlayers cadddr)
(define getPlayers (lambda (juego)
                     (car (cddddr juego))))
(define getStatus (lambda (juego)
                    (cadr (cddddr juego))))
(define getMode (lambda (juego)
                  (caddr (cddddr juego))))
(define getRandom (lambda (juego)
                    (cadddr (cddddr juego))))

; Modificadores
(define register (lambda (nombre juego)
                   (if (or (isPlayer? nombre (getPlayers juego)) (>= (cantidadPlayers (getPlayers juego)) (getMaxPlayers juego) ))
                       juego
                       (game_to_modify  (getArea juego) (getSetCartas juego) (getTurn juego) (getMaxPlayers juego)
                                        (addPlayer nombre (getPlayers juego)) (getStatus juego) (getMode juego) (getRandom juego)))))

(define updateArea (lambda (area juego)
                     (game_to_modify  area (getSetCartas juego) (getTurn juego) (getMaxPlayers juego)
                                      (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego))
                     ))

(define updateTurn (lambda (turn juego)
                     (if (positive? turn)
                         (game_to_modify  (getArea juego) (getSetCartas juego) turn (getMaxPlayers juego)
                                          (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego))
                         null
                         )))

(define updateSet (lambda (setCartas juego)
                    (if (not (null? setCartas))
                        (game_to_modify  (getArea juego) setCartas (getTurn juego) (getMaxPlayers juego)
                                         (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego))
                        null
                        )))

(define updateAreaSet (lambda (newArea newSetCartas juego)
                        (if (not (null? newSetCartas))
                            (game_to_modify newArea newSetCartas (getTurn juego) (getMaxPlayers juego)
                                            (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego))
                            null
                            )))

; Otros
(define whoseTurnIsIt? (lambda (juego)
                         (if (not (= 0 (cantidadPlayers (getPlayers juego))))
                             (if (and (= 0 (getTurn juego)) )
                                 (getPlayerName (getnPlayer (getPlayers juego) 0))
                                 (getPlayerName (getnPlayer (getPlayers juego) (remainder (getTurn juego) (cantidadPlayers (getPlayers juego)))))
                                 )
                             null)))

; Tengo el set,saco las 2 primeras cartas, eligo un item, paso.
;(define stackMode (lambda (juego funcion)
;                    (if (= funcion null)

(define pass (lambda (juego)
               (if (equal? (getArea juego) gameAreaVacia)
                   juego
                   (pass  (updateAreaSet (restoArea (getArea juego)) (addCard (getSetCartas juego) (firstArea (firstArea juego))) juego))
                   )))
                   
                    
                                                  