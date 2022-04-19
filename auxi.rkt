#lang racket

(require "card.rkt")
(require "cards.rkt")

(provide es_primo?)
(provide esta-en)
(provide simbolos-hasta-n)
(provide rand)
(provide cut)
(provide randomFn)

; AUXILIARES

;Nombre Función: es_primo?
;Descripción: Calcular si un numero es primo
;Dom: Z+ - {1}
;Rec: boolean -> #t o #f
;Tipo Recursión: Cola

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

;Nombre Función: esta-en
;Descripción: Comprueba si una card esta dentro de un cards
;Dom: cards X card
;Rec: boolean -> #t o #f
;Tipo Recursión: Cola

(define esta-en (lambda (cartas carta)
                  (if (null? cartas)
                      #f
                      (or (set=? (car cartas) carta) (esta-en (cdr cartas) carta))
                      )))

;Nombre Función: simbolos-hasta-n
;Descripción: Crea una lista de numeros que van desde 1 hasta n
;Dom: Z+ X list
;Rec: list
;Tipo Recursión: Cola

(define simbolos-hasta-n(lambda (n resultado)
                          (if (= n 0)
                              resultado
                              (simbolos-hasta-n (- n 1) (append resultado (list n))))))

;Nombre Función: rand
;Descripción: Funcion que recibe una lista, una funcion que aleatoriza numeros, un numero inicial
;para esa funcion y va aleatorizando una lista, donde va aumentando el numero de uno
;en uno, y si el resultado de la funcion en ese numero es par, añade el numero a la lista,
;si no,  da vuelta la lista y lo añade al principio de la lista, pero ahora la lista esta invertida
;Dom: List o cards X funcion X Z+
;Rec: List o cards
;Tipo Recursión: Cola

(define rand (lambda (L fnrandom j)
               (define randomize(lambda (L newL i fn)
                                  (if (not (null? L))
                                      (if (or (=(remainder (fn i) 2)0))
                                          (randomize (cdr L) (addSymbol (car L) newL) (+ i 1) fn)
                                          (randomize (cdr L) (addSymbol (car L) (reverse newL)) (+ i 1) fn)
                                          )
                                      newL
                                      )))
               (randomize L cartasVacias j fnrandom)))

;Nombre Función: cut
;Descripción: Funcion que recibe una lista y un numero y corta la lista en en ese numero
;Dom: list o cards X Z+
;Rec: list o cards
;Tipo Recursión: Cola

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

; Extra
(define m 2147483647)
(define a 1103515245)
(define c 12345)

;Nombre Función: randomFn
;Descripción: Funcion que recibe un numero y simula una aleatorizacion, solo simula debido a que
;parte importante de este paradigma impide que para una misma entrada hayan 2 salidas
;diferentes
;Dom: Z+ + {0}
;Rec: Z+ + {0}

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)
