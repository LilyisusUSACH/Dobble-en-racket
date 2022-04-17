#lang racket

(provide es_primo?)
(provide esta-en)

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
