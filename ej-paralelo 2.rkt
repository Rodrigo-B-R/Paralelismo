#lang racket

(require racket/future)     ; para paralelo
(require future-visualizer) ; para ver los hilos

; crea lista con valores aleatorios
(define (lista-rands ndatos max-val)
  (build-list ndatos (lambda (_) (random max-val)) )
)


; crea lista con sublistas de tamaño size
(define (spliter lista size )
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


(define (spliter-faster lista size)
  (define (helper lista accu)
    (cond
      [(<= (length lista) size)
       (reverse (cons lista accu))]
      [else
       (helper (drop lista size)
               (cons (take lista size) accu))]
    )
  )
  (helper lista '())
)




; 2400000 valores aleatorios
(define fullDatos (lista-rands 2400000 20) )

; processor-count para averiguar cuantos cores hay 
(define num-cores (processor-count))
num-cores

; chunk-size segun el numero de hilos
(define chunk-size (ceiling (/ (length fullDatos) num-cores)) )

; void evita que se imprima el resultado
(time (void (spliter fullDatos chunk-size)))
(time (void (spliter-faster fullDatos chunk-size)))


;---------------------------------------------------

; incrementa 1 a toda una lista
(define (add1-list lista)
   (map add1 lista )
)

; version secuencial
;(add1-list fullDatos)


; obtener secciones de datos, una por core
(define chunks (spliter-faster fullDatos chunk-size ) )


(define chunksResultados
 (visualize-futures
   (let
    ([futures  (map (lambda (chunk)
          (future (lambda () (add1-list chunk) ) )
       )chunks)  ])
    (map touch futures)
   ) 
 )
)
; nota que chunksResultados esta dividido en chunks
; debe unirse
(apply append chunksResultados) ; juntar resultados

; para obtener el tiempo debes considerar el proceso completo: 
; cargar datos, crear los chunks, crear los futures
; y reunir el resultado

; util: averiguar cuanto toma cada parte




