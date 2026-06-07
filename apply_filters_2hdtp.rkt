#lang racket

(require racket/future)     
(require 2htdp/image) 



;definir chunks tamaño: width  *  (height / num procesadores)
(define img (bitmap/file "images/new-york-large.jpg"))
(define pixels (image->color-list img))


;pixels

(define num-cores (processor-count))
(define nT num-cores)

(define width (image-width img))
(define chunk-size (ceiling (/ (length pixels) nT)))


;dividir lista en listas del mismo tamaño
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

;mide velocidad de una funcion
(define (medidor funcion)
  
  (collect-garbage) 
  
  (define start (current-inexact-milliseconds))
  
         (define result (funcion) )

  (define end (current-inexact-milliseconds))
  
  (values result  (- end start) ) 
)

;aplica una funcion a cada chunk en paralelo
(define (apply-parallel funcion chunks)
  (define futures (map (lambda (chunk) (future (lambda () (funcion chunk)))) chunks))
  (map touch futures))

;convierte chunks a imagen completa
(define (chunks->image chunks img-width)
  (define all-pixels (apply append chunks))
  (define total-height (/ (length all-pixels) img-width))
  (color-list->bitmap all-pixels img-width total-height))

(define (clamp x)
  (min 255 (max 0 (inexact->exact (round x)))))

(define (invert chunk)
  (map (lambda (c)
         (color (- 255 (color-red c))
                (- 255 (color-green c))
                (- 255 (color-blue c))
                (color-alpha c)))
       chunk))

(define (grayscale chunk)
  (map (lambda (c)
         (let ([gray (quotient (+ (color-red c) (color-green c) (color-blue c)) 3)])
           (color gray gray gray (color-alpha c))))
       chunk))

(define (sepia chunk)
  (map (lambda (c)
         (let* ([r (color-red c)]
                [g (color-green c)]
                [b (color-blue c)])
           (color (clamp (+ (* r 0.4) (* g 0.7) (* b 0.2)))
                  (clamp (+ (* r 0.3) (* g 0.6) (* b 0.16)))
                  (clamp (+ (* r 0.27) (* g 0.53) (* b 0.13)))
                  (color-alpha c))))
       chunk))


;llamar funciones
(define-values (chunks tiempo-dividir)
  (medidor (lambda () (spliter-faster pixels chunk-size))))

(displayln (format "Cores:        ~a  |  Dividir: ~a ms" nT tiempo-dividir))

;invert
(define-values (_chunks-inv-seq tiempo-inv-seq)
  (medidor (lambda () (invert pixels))))
(define-values (chunks-inv-par tiempo-inv-par)
  (medidor (lambda () (apply-parallel invert chunks))))

(displayln (format "\n--- invert ---"))
(displayln (format "Secuencial:   ~a ms" tiempo-inv-seq))
(displayln (format "Paralelo:     ~a ms (+ ~a ms dividir)" tiempo-inv-par tiempo-dividir))
(displayln (format "Speedup:      ~ax" (/ tiempo-inv-seq (+ tiempo-dividir tiempo-inv-par))))

;grayscale
(define-values (_chunks-gray-seq tiempo-gray-seq)
  (medidor (lambda () (grayscale pixels))))
(define-values (chunks-gray-par tiempo-gray-par)
  (medidor (lambda () (apply-parallel grayscale chunks))))

(displayln (format "\n--- grayscale ---"))
(displayln (format "Secuencial:   ~a ms" tiempo-gray-seq))
(displayln (format "Paralelo:     ~a ms (+ ~a ms dividir)" tiempo-gray-par tiempo-dividir))
(displayln (format "Speedup:      ~ax" (/ tiempo-gray-seq (+ tiempo-dividir tiempo-gray-par))))

;sepia
(define-values (_chunks-sep-seq tiempo-sep-seq)
  (medidor (lambda () (sepia pixels))))
(define-values (chunks-sep-par tiempo-sep-par)
  (medidor (lambda () (apply-parallel sepia chunks))))

(displayln (format "\n--- sepia ---"))
(displayln (format "Secuencial:   ~a ms" tiempo-sep-seq))
(displayln (format "Paralelo:     ~a ms (+ ~a ms dividir)" tiempo-sep-par tiempo-dividir))
(displayln (format "Speedup:      ~ax" (/ tiempo-sep-seq (+ tiempo-dividir tiempo-sep-par))))

(chunks->image chunks-inv-par width)
(chunks->image chunks-gray-par width)
(chunks->image chunks-sep-par width)







;(define chunk-size (ceiling (/ (length fullDatos) num-cores)) )






