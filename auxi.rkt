#lang racket

(require "card.rkt")
(require "cards.rkt")

(provide es_primo?)
(provide esta-en)
(provide simbolos-hasta-n)
(provide rand)
(provide cut)

; AUXILIAR PARA CARDSET

(define es_primo? (lambda (n)
                    (define is_primo (lambda (i n)
                                       (if (= i (- n 1))
                                           #f
                                           (if (= (remainder n i) 0)
                                               #t
                                               (is_primo (+ 1 i) n)))))
                    
                    (if (or (= n 1) (= n 2) (= n 3))
                        #t
                        (not(is_primo 2 n))
                        )
                    ))

(define esta-en (lambda (cartas carta)
                  (if (null? cartas)
                      #f
                      (or (set=? (car cartas) carta) (esta-en (cdr cartas) carta))
                      )))

(define simbolos-hasta-n(lambda (n resultado)
                          (if (= n 0)
                              resultado
                              (simbolos-hasta-n (- n 1) (append resultado (list n))))))

(define rand (lambda (L)
               (define randomize(lambda (L newL i)
                                  (if (not (null? L))
                                      (if (or (=(remainder i 2)0))
                                          (randomize (cdr L) (addSymbol (car L) newL) (+ i 1))
                                          (randomize (cdr L) (addSymbol (car L) (reverse newL)) (+ i 1))
                                          )
                                      newL
                                      )))
               (randomize L cartasVacias 0)))

(define cut (lambda (L n)
              (define cortar (lambda (L n)
                               (if (= n (largo L))
                                   L
                                   (if (= n (largo (cdr L)))
                                       (cdr L)
                                       (cortar (cdr L)  n)))))
              
              (if (or (null? L) (> n (largo L)))
                  null
                  (if (<= 0 n)
                      (cortar L n)
                      L)
                  )))