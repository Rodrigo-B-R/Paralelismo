#lang racket
; crea una lista con ndatos aleatorios
(define (lista-rands ndatos max-val)
  (build-list ndatos
    (lambda (_)
      (random max-val))))

; divide una lista en partes de tamaño x
(define (spliter lista x)
  (cond
    [(= x (length lista))
     (list lista)]

    [else
     (cons (take lista x)
           (spliter (drop lista x) x))]
  )
)

; genera lista con 12 valores random, hasta 20 c/u
(define lista1 (lista-rands 12 20))
lista1

(take lista1 3) ; los primeros 3
(drop lista1 3) ; los restantes

(spliter lista1 3)

(define lista2 (lista-rands 17 20))
lista2

(define lista3 (lista-rands 15 20))
lista3

(define nhilos 4) ; cantidad de grupos


; calc chunk-size
(define (calc-chunk-size lista nhilos)
  (cond
    [(= (modulo (length lista) nhilos) 0)
     (quotient (length lista) nhilos)]

    [else
     (+ 1 (quotient (length lista) nhilos))]
  )
)

(define particiones '((1 2 3) (4 5 6) (7 8 9)))

(define myFutures 
    (map (lambda (particion)
        (future (lambda () (add1-lista particion) ))
        )
        particiones
            )
                )

; chunk-size para lista2?
(define chunk2 (calc-chunk-size lista2 nhilos))

; chunk-size para lista3?
(define chunk3 (calc-chunk-size lista3 nhilos))