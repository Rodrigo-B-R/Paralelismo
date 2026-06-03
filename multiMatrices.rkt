#lang racket



; Crea una fila aleatoria, con n vals
(define (random-row n)
  (build-list n (lambda (_) (random 100)))
)

; Matrix aleatoria, con rows y cols
(define (random-matrix rows cols)
  (build-list rows  (lambda (_)   (random-row cols)))
)

;----------------------------------------------------
; la transpuesta funciona asi: 
; apply 'desempaqueta' las filas de m, para que map las reciba
; (map list '(1 2 3) '(4 5 6))


; transpuesta de una matrix (para extraer columnas) 
(define (transpuesta m)   (apply map list m) )


; producto punto:
; (map * '(1 2 3) '(4 5 6) )
; map multiplica y apply suma el resultado
(define (dot row col)
     (apply + (map * row col))
)

;--------------------------------------------------------
; multiplica matrix, secuencial
; producto punto de cada fila de A con cada col de B
(define (multi-secuencial A  B )
     (define BT (transpuesta B) )

     (map (lambda (filaA) 
               (map (lambda (colBT) (dot filaA colBT) ) BT))
          A
     )
)


;------------------------------------------------------
; en paralelo
; por cada fila de A... crea un future (sin numero de hilos fijo) 
; cada future aplica un map de su fila, con cada col de BT
(define (multi-paralelo A B)
  (define BT (transpuesta B))

  (define futures
    (map (lambda (row)
           (future (lambda ()
                   (map (lambda (col) (dot row col))  BT))  )
         )
         A
    )
  )
  ;(length futures)  ; es 1 por row
  (map touch futures)
)




(define (multi-paralelo-chunk A B nT)
  (define BT (transpuesta B))

  (define chunk-size (ceiling (/ (length A) nT)))
  (define chunky-A (particiona A chunk-size) )
  
  (define futures
    (map (lambda (chunk) 
           (future(lambda ()
              (map (lambda (row)
                     (map (lambda (col) (dot row col)) BT)
                   )
                   chunk
              )
           ))
         )
        chunky-A
    )
  )
  (map touch futures)
)



(define (particiona lista size )
  (define (helper lista accu)
    (cond
      [(<= (length lista) size ) (append accu (list lista)) ]
      [else
         (define newAccu (append accu (list (take lista size) ) ) )
         (helper (drop lista size) newAccu)
      ]
    )
  )
  (helper lista '())
)

; funcion que mide cuanto toma una funcion
; devuelve dos cosas: resultado y tiempo
(define (medidor funcion)
  ; limpiar antes, para que sea menos
  ; probable limpiar mientras medimos
  (collect-garbage) 
  
  (define start (current-inexact-milliseconds))
  
         (define result (funcion) )

  (define end (current-inexact-milliseconds))
  
  (values result  (- end start) ) ; 2 values de regreso!
)

;----------------------------------------------------

;(random-row 10)
;(random-matrix 5 5)

(define N 5)
(displayln "Matrix size: ")
N

(define A (random-matrix N N))

; Llamada asi solo da el resultado 
;(multi-secuencial A  A )
;(multi-paralelo A A)

; Asi podemos sacar tambien el tiempo
(define-values  [_resultado-secuencial tiempo-secuencial]
         (medidor (lambda () (multi-secuencial A A)) )
)
;resultado-secuencial
(displayln "tiempo sec:")
tiempo-secuencial


(define-values  [_resultado-paralelo tiempo-paralelo]
         (medidor (lambda () (multi-paralelo A A) ) )
)
;resultado-paralelo
(displayln "tiempo paralelo:")
tiempo-paralelo


(define n_threads 2)
(define-values  [_resultado-chunk tiempo-chunk]
         (medidor (lambda () (multi-paralelo-chunk A A n_threads) ) )
)
;resultado-chunk
(displayln (string-append "tiempo paralelo con chunks (" (number->string n_threads) " hilos):"))
(displayln tiempo-chunk)


(displayln (string-append "Cores disponibles: " (number->string (processor-count))))