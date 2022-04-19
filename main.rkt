#lang racket

; main
(require "cardsSet.rkt")
(require "game.rkt")
(require "cardsSet.rkt")
(require "cards.rkt")
(require "cardsSet.rkt")
(require "player.rkt")
(require "players.rkt")
(require "area.rkt")
(require "auxi.rkt")

; EJEMPLOS DE USO

; TDA cardsSet
(define Cs1 (cardsSet '("D" "E" "F") 2 -1 randomFn))
(define Cs2 (cardsSet '("A" "B" "C" "D" "E" "F" "G") 6 -1 randomFn))
(define Cs3 (cardsSet '( "A" "B" "C") 3 1 randomFn))
(define dobbleOriginal (cardsSet null 8 55 randomFn))

(define db1 (dobble? Cs1))
(define db2 (dobble? Cs2))
(define db3 (dobble? Cs3))
(define dbOriginal (dobble? dobbleOriginal))

(define nC1 (numCards Cs1))
(define nC2 (numCards Cs2))
(define nC3 (numCards Cs3))
(define nCOriginal (numCards dobbleOriginal))

(define ntC1 (nthCard 2 Cs1))
(define ntC2 (nthCard 6 Cs2))
(define ntC3 (nthCard 0 Cs3))
(define ntCOriginal (nthCard 54 dobbleOriginal))

(define ftc1 (findTotalCards (getfirstCard(getCards Cs1))))
(define ftc2 (findTotalCards (getfirstCard(getCards Cs2))))
(define ftc3 (findTotalCards (getfirstCard(getCards Cs3))))
(define ftcOriginal (findTotalCards (getfirstCard(getCards dobbleOriginal))))

(define RE1 (requiredElements (getfirstCard(getCards Cs1))))
(define RE2 (requiredElements (getfirstCard(getCards Cs2))))
(define RE3 (requiredElements (getfirstCard(getCards Cs3))))
(define REOriginal (requiredElements (getfirstCard(getCards dobbleOriginal))))

(define MC1 (missingCards Cs1))
(define MC2 (missingCards Cs2))
(define MC3 (missingCards Cs3))
(define MCOriginal (missingCards dobbleOriginal))

; RECORDAR USAR CON DISPLAY
(define CtoS1 (cardsSet->string Cs1))
(define CtoS2 (cardsSet->string Cs2))
(define CtoS3 (cardsSet->string Cs3))
(define CtoSOriginal (cardsSet->string dobbleOriginal))

; TDA GAME

; Solo esta el stackMode Implementado
; estos ejemeplos son tanto de stackMode como de game
(define game1(game 2 (cardsSet (list "A" "B" "C") 2 -1 randomFn) stackMode randomFn))
(define game2(game 4 (cardsSet (list "A" "B" "C" "D" "E" "F" "G") 3 -1 randomFn) stackMode randomFn))
(define game3(game 5 dobbleOriginal stackMode randomFn))
  
(define game11 (register "israel" (register "juan" game1)))
; añadir mismo nombre
(define game11E (register "juan" game11))
; añadir mas del max
(define game11EE (register "joaquin" game11E))
  
(define game21 (register "figueroa" (register "lulu" game2)))
(define game31 (register "jiji" (register "Esteban" (register "Paredes" game3))))

(define wti11 (whoseTurnIsIt? game11))
(define wti12 (whoseTurnIsIt? game21))
(define wti13 (whoseTurnIsIt? game31))

(define play1 (play game11 null))
(define play2 (play game21 null))
(define play3 (play game31 null))

(define play11 (play play1 pass))
(define play21 (play play2 (spotIt "A")))
(define play31 (play play3 null))

(define play12 (play play11 null))
(define play22 (play play21 (spotIt "A")))
(define play32 (play play31 (spotIt 10)))

(define play13 (play play12 (spotIt "A")))
(define play14 (play play13 null)) ; Lo da por finalizado

(define st1 (status game1))
(define st11 (status play11))
(define st12 (status play14))

;(define game131 (play game12 (spotIt "A")))

; TDA game addCard -
(define addC1(addCard emptyCardsSet '("A" "B")))
(define addC2(addCard addC1 '("B" "F")))
(define addCE(addCard addC1 '("C" "F")))
(define addC3 (addCard (addCard (addCard (addCard emptyCardsSet '("A" "B")) '("B" "C")) '("C" "A")) '("C" "D")))
