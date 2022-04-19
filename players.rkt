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

;Nombre Función: playersVacio
;Descripción: Funcion que crea un players Vacio
;Dom: - no recibe nada
;Rec: players

(define playersVacio null)

;Nombre Función: addPlayer
;Descripción: Añade un player a players, solo con el nombre
;Dom: String X players
;Rec: players

(define addPlayer (lambda (nombre pls)
                    (cons (newPlayer nombre) pls)
                    ))

;Nombre Función: addPlayerToFinal
;Descripción: añade un player al final de un players
;Dom: - no recibe nada
;Rec: players

(define addPlayerToFinal append)

; Selectores

;Nombre Función: firstPlayer
;Descripción: retorna el primer player de un player
;Dom: players
;Rec: player

(define firstPlayer car)

;Nombre Función: anotherPlayers
;Descripción: retorna un players con todos los players menos el primero
;Dom: players
;Rec: players

(define anotherPlayers cdr)

;Nombre Función: cantidadPlayers
;Descripción: retorna la cantidad de player en un players
;Dom: players
;Rec: Z+ + {0}

(define cantidadPlayers length)

;Nombre Función: getnPlayer
;Descripción: devuelve el player en la posicion n de players
;Dom: players X Z+ + {0}
;Rec: player

(define getnPlayer (lambda (pls n)
                     (list-ref pls n)))

;Nombre Función: isPlayer?
;Descripción: funcion que revisa si un player esta en players
; en base a su nombre
;Dom: String X players
;Rec: boolean -> #t o #f
;Tipo Recursión: Cola

(define isPlayer? (lambda (nombre pls) 
                    (if (null? pls)
                        #f
                        (if (equal? (getPlayerName (firstPlayer pls)) nombre)
                            #t
                            (isPlayer? nombre (anotherPlayers pls))
                            ))
                     ))
; Modificadores

;Nombre Función: addCardToPlayer
;Descripción: Funcion que le añade una card a un player segun su nombre
;Dom: String X card X players X players
;Rec: players
;Tipo Recursión: Cola
  
(define addCardToPlayer (lambda (nombre carta pls resultado)
                          (if (null? pls)
                              resultado
                              (if (equal? (getPlayerName (firstPlayer pls)) nombre)
                                  (addCardToPlayer nombre carta (anotherPlayers pls)
                                                   (addPlayerToFinal resultado (list (setPlayerScore (addPlayerCard (firstPlayer pls) carta) (+ 1 (getPlayerScore (firstPlayer pls) ))))))
                                  (addCardToPlayer nombre carta (anotherPlayers pls) (addPlayerToFinal resultado (list (firstPlayer pls)) ))
                                  ))))

;Nombre Función: addCardsToPlayer
;Descripción: Funcion que le añade cards a un player segun su nombre
;Dom: String X cards X players
;Rec: Z+ + {0} (o cardinales)
;Tipo Recursión: Cola

(define addCardsToPlayer (lambda (nombre cartas pls)
                           (if (null? cartas)
                               pls
                               (addCardsToPlayer nombre (cdr cartas) (addCardToPlayer nombre (car cartas) pls null)))))