#lang racket

(require "cardsSet.rkt")
(require "cards.rkt")
(require "cardsSet.rkt")
(require "player.rkt")
(require "players.rkt")
(require "area.rkt")
(require "auxi.rkt")
(provide (all-defined-out)) ; haria el provide uno x uno pero no hay tiempoooo

; TDA Game
; rec : n°players X cardsSet X modo (FUNCION) X funcion random
; Area de juego , cardSet , turno , max Jugadores, jugadores registrados (con sus cartas), estado juego, modo , randomfn

         
; Constructor

;Nombre Función: gameVacio
;Descripción: Crea un game vacio
;Dom: No recibe nada
;Rec: game

(define gameVacio (list gameAreaVacia emptyCardsSet 0 0 playersVacio "No Iniciado" null null))

; Estado si esta "Terminado" no se puede seguir jugando.

;Nombre Función: game
;Descripción: Crea un game a partir de los datos entregados
;Dom: Z+ X cardsSet X procedure (funcion) X funcion
;Rec: game

(define game(lambda (n_players setcartas modo randomfn)
              (if (and (positive? n_players) (dobble? setcartas) (procedure? modo) (procedure? randomfn))
                  (list gameAreaVacia setcartas 0 n_players playersVacio "No iniciado" modo randomfn)
                  gameVacio
                  )))

;Nombre Función: game_to_modify
;Descripción: Solo se usa para modificar elementos del game
;Dom: game
;Rec: game

(define game_to_modify list)


; Selectores

;Nombre Función: getArea
;Descripción: Obtiene el area de un game
;Dom: game
;Rec: area

(define getArea car)

;Nombre Función: getSetCartas
;Descripción: Obtiene el cardsSet de un game
;Dom: game
;Rec: cardsSet

(define getSetCartas cadr)

;Nombre Función: getTurn
;Descripción: Obtiene el turno de un game
;Dom: game
;Rec: Z+ + {0}

(define getTurn caddr)

;Nombre Función: getMaxPlayers
;Descripción: Obtiene el maximo de jugadores de un game
;Dom: game
;Rec: Z+ + {0}

(define getMaxPlayers cadddr)

;Nombre Función: getPlayers
;Descripción: Obtiene los players de un game
;Dom: game
;Rec: players

(define getPlayers (lambda (juego)
                     (car (cddddr juego))))

;Nombre Función: getStatus
;Descripción: Obtiene el estado de un game
;Dom: game
;Rec: String

(define getStatus (lambda (juego)
                    (cadr (cddddr juego))))

;Nombre Función: status
;Descripción: Obtiene el estado de un game
;Dom: game
;Rec: String

(define status getStatus)


;Nombre Función: getMode
;Descripción: Obtiene una funcion modo de un game
;Dom: game
;Rec: procedure (funcion)

(define getMode (lambda (juego)
                  (caddr (cddddr juego))))

;Nombre Función: getRandom
;Descripción: Obtiene la funcion random de un game
;Dom: game
;Rec: procedure (funcion)

(define getRandom (lambda (juego)
                    (cadddr (cddddr juego))))

; Modificadores

;Nombre Función: register
;Descripción: Funcion que registra un nuevo usuario en el juego
;Dom: String X game
;Rec: game

(define register (lambda (nombre juego)
                   (if (or (isPlayer? nombre (getPlayers juego)) (>= (cantidadPlayers (getPlayers juego)) (getMaxPlayers juego) ))
                       juego
                       (game_to_modify  (getArea juego) (getSetCartas juego) (getTurn juego) (getMaxPlayers juego)
                                        (addPlayer nombre (getPlayers juego)) (getStatus juego) (getMode juego) (getRandom juego)))))

;Nombre Función: updateArea
;Descripción: Funcion que actualiza el area
;Dom: area X game
;Rec: game

(define updateArea (lambda (area juego)
                     (game_to_modify  area (getSetCartas juego) (getTurn juego) (getMaxPlayers juego)
                                      (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego))
                     ))

;Nombre Función: updateTurn
;Descripción: Funcion que actualiza el turno
;Dom: turno X game
;Rec: game

(define updateTurn (lambda (turn juego)
                     (if (positive? turn)
                         (game_to_modify  (getArea juego) (getSetCartas juego) turn (getMaxPlayers juego)
                                          (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego))
                         null
                         )))

;Nombre Función: updateSet
;Descripción: Funcion que actualiza el cardsSet
;Dom: cardsSet X game
;Rec: game

(define updateSet (lambda (setCartas juego)
                    (if (not (null? setCartas))
                        (game_to_modify  (getArea juego) setCartas (getTurn juego) (getMaxPlayers juego)
                                         (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego))
                        null
                        )))

;Nombre Función: updateStatus
;Descripción: Funcion que actualiza el estado
;Dom: status X game
;Rec: game

(define updateStatus (lambda (newStatus juego)
                    (if (not (null? newStatus))
                        (game_to_modify  (getArea juego) (getSetCartas juego) (getTurn juego) (getMaxPlayers juego)
                                         (getPlayers juego) newStatus  (getMode juego) (getRandom juego))
                        null
                        )))

;Nombre Función: updatePlayers
;Descripción: Funcion que actualiza los players
;Dom: players X game
;Rec: game

(define updatePlayers (lambda (newPls juego)
                        (game_to_modify  (getArea juego) (getSetCartas juego) (getTurn juego) (getMaxPlayers juego)
                                         newPls (getStatus juego)  (getMode juego) (getRandom juego))))
; Otros

;Nombre Función: whoseTurnIsIt?
;Descripción: Funcion que dependiendo del turno, dice a que player le corresponde
;Dom: game
;Rec: String

(define whoseTurnIsIt? (lambda (juego)
                         (if (not (= 0 (cantidadPlayers (getPlayers juego))))
                             (if (and (= 0 (getTurn juego)) )
                                 (getPlayerName (getnPlayer (getPlayers juego) 0))
                                 (getPlayerName (getnPlayer (getPlayers juego) (remainder (getTurn juego) (cantidadPlayers (getPlayers juego)))))
                                 )
                             null)))

; FUNCIONES QUE NO VAN ACA, FALTA COMPLETAR
(define corte (lambda (x j)
                (cut (rand (getCards (getSetCartas j)) randomFn (getTurn j) )x)))

(define repartirCartas (lambda (players cantidad juego newPlayers)
                         (if (not (null? newPlayers))
                             (repartirCartas (addCardsToPlayer (getPlayerName (firstPlayer newPlayers)) (corte cantidad juego) players) cantidad
                                             (updateSet (set-subtract (getCards (getSetCartas juego)) (corte cantidad juego) )juego) (anotherPlayers newPlayers))
                             players)))

; Modos de juego

;Nombre Función: stackMode
;Descripción: Funcion que define las reglas del modo de juego stackMode
;Dom: game X funcion
;Rec: game | llama  a otra funcion

(define stackMode (lambda (juego funcion)
                    (define corte (cut (rand (getCards (getSetCartas juego)) randomFn (getTurn juego) ) 2))
                    (if (null? funcion)
                        (if (and (or (cartasVacias? corte) (cartasVacias? (getCards (getSetCartas juego)))) (cartasVacias? (getArea juego)))
                            (updateStatus "Finalizado" juego) ; ademas elegir ganador y eso, despues vere
                            (if (cartasVacias? (getArea juego))
                                (updateStatus "Jugando" (updateSet (cons (set-subtract (getCards (getSetCartas juego)) corte)
                                                 (getSymbols (getSetCartas juego))) (updateArea corte juego)))
                                juego
                            ))
                        (funcion juego 0)
                        )))

;Nombre Función: empyHandsStackMode
;Descripción: Funcion que define las reglas del modo de juego empyHandsStackMode
;Dom: game X funcion
;Rec: game | llama  a otra funcion

(define empyHandsStackMode (lambda (juego funcion)     
                             (if (and (null? funcion) (equal? (getStatus juego) "No iniciado"))
                                 (floor (/ (numCards (getSetCartas juego)) (+ 1 (cantidadPlayers(getPlayers juego))) ))
                                 null
                                 )))

;Nombre Función: pass
;Descripción: Funcion que realiza la accion pass en el juego
;Dom: game X Z+
;Rec: game
;Tipo Recursión: Cola

(define pass (lambda (juego condi)
               (if (= 0 condi)
                   (if (equal? (getArea juego) gameAreaVacia)
                       (updateTurn (+ 1 (getTurn juego)) juego)
                       (pass (game_to_modify  (restoArea (getArea juego)) (addCardToFinal (getSetCartas juego) (firstArea (firstArea juego)))
                                              (getTurn juego) (getMaxPlayers juego) (getPlayers juego) (getStatus juego) (getMode juego) (getRandom juego)) condi)
                       )
                   (if (= 1 condi)
                       condi
                       null)
                   )))                   


;Nombre Función: spotIt
;Descripción: Funcion currificada que recibe un simbolo
; y despues espera a ser llamada en otra funcion con la funcion interna
; evaluada en el simbolo recibido y realiza las acciones correspondientes al
; modo de juego
;Dom: Elemento (el que sea) X game X Z+
;Rec: game
;Tipo Recursión: Cola

(define spotIt (lambda (simbolo)
                 (define funcion2 (lambda (simbolo juego condi)
                                    (if (= 0 condi)
                                        (if (isSymbolInAreaCards? simbolo (getArea juego))
                                            (pass (updateArea null (updatePlayers (addCardsToPlayer (whoseTurnIsIt? juego) (getArea juego) (getPlayers juego)) juego)) condi)
                                            (pass juego condi))
                                        (if (= 1 condi)
                                            condi
                                            null)
                                        )))
                 (curry funcion2 simbolo)))

;Nombre Función: play
;Descripción: funcion que envia a el modo de juego evaluado en el juego y
; con una accion
;Dom: game X funcion
;Rec: game -> que sera devuelto por la funcion interna

(define play (lambda (juego action)
               ((getMode juego) juego action)
  ))