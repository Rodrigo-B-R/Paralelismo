#lang racket

(require racket/future)
(require racket/draw)

(define bmp (make-object bitmap% "images/new-york-large.jpg"))
(define w (send bmp get-width))
(define h (send bmp get-height))

(define pixel-bytes (make-bytes (* w h 4)))
(send bmp get-argb-pixels 0 0 w h pixel-bytes)

(define pixels (bytes->list pixel-bytes))

(define num-cores (processor-count))


;dividir lista en listas del mismo tamaño
(define (spliter-faster lista size)
  (define (helper lista accu)
    (cond
      [(<= (length lista) size)
       (reverse (cons lista accu))]
      [else
       (helper (drop lista size)
               (cons (take lista size) accu))]))
  (helper lista '()))

;mide velocidad de una funcion
(define (medidor funcion)
  (collect-garbage)
  (define start (current-inexact-milliseconds))
  (define result (funcion))
  (define end (current-inexact-milliseconds))
  (values result (- end start)))

;aplica una funcion en paralelo sobre chunks ya divididos
(define (apply-parallel funcion chunks)
  (define futures (map (lambda (chunk) (future (lambda () (funcion chunk)))) chunks))
  (map touch futures))

;convierte chunks a imagen usando racket/draw
(define (chunks->image chunks w h)
  (define all-pixels (apply append chunks))
  (define pb (list->bytes all-pixels))
  (define bmp2 (make-bitmap w h))
  (send bmp2 set-argb-pixels 0 0 w h pb)
  bmp2)

(define (invert lista)
  (cond [(empty? lista) '()]
        [else (let ([a (first lista)]
                    [r (- 255 (second lista))]
                    [g (- 255 (third lista))]
                    [b (- 255 (fourth lista))])
                (append (list a r g b) (invert (drop lista 4))))]))

(define (grayscale lista)
  (cond [(empty? lista) '()]
        [else (let* ([a    (first lista)]
                     [r    (second lista)]
                     [g    (third lista)]
                     [b    (fourth lista)]
                     [gray (quotient (+ r g b) 3)])
                (append (list a gray gray gray)
                        (grayscale (drop lista 4))))]))

(define (sepia lista)
  (define (clamp x)
    (min 255 (max 0 (inexact->exact (round x)))))
  (cond [(empty? lista) '()]
        [else (let* ([a   (first lista)]
                     [r   (second lista)]
                     [g   (third lista)]
                     [b   (fourth lista)]
                     [n_r (clamp (+ (* r 0.4) (* g 0.7) (* b 0.2)))]
                     [n_g (clamp (+ (* r 0.3) (* g 0.6) (* b 0.16)))]
                     [n_b (clamp (+ (* r 0.27) (* g 0.53) (* b 0.13)))])
                (append (list a n_r n_g n_b)
                        (sepia (drop lista 4))))]))


;llamar funciones
(define nT num-cores)
(define chunk-size (ceiling (/ (length pixels) nT)))
(define-values (chunks tiempo-dividir)
  (medidor (lambda () (spliter-faster pixels chunk-size))))

(displayln (format "Cores:        ~a  |  Dividir: ~a ms" nT tiempo-dividir))

;invert
(define-values (_inv-seq tiempo-inv-seq)
  (medidor (lambda () (invert pixels))))
  
(define-values (chunks-inv-par tiempo-inv-par)
  (medidor (lambda () (apply-parallel invert chunks))))

(displayln (format "\n--- invert ---"))
(displayln (format "Secuencial:   ~a ms" tiempo-inv-seq))
(displayln (format "Paralelo:     ~a ms" tiempo-inv-par))
(displayln (format "Speedup:      ~ax" (/ tiempo-inv-seq tiempo-inv-par)))

;sepia
(define-values (_sep-seq tiempo-sep-seq)
  (medidor (lambda () (sepia pixels))))
(define-values (chunks-sep-par tiempo-sep-par)
  (medidor (lambda () (apply-parallel sepia chunks))))

(displayln (format "\n--- sepia ---"))
(displayln (format "Secuencial:   ~a ms" tiempo-sep-seq))
(displayln (format "Paralelo:     ~a ms" tiempo-sep-par))
(displayln (format "Speedup:      ~ax" (/ tiempo-sep-seq tiempo-sep-par)))

;grayscale
(define-values (_gray-seq tiempo-gray-seq)
  (medidor (lambda () (grayscale pixels))))
(define-values (chunks-gray-par tiempo-gray-par)
  (medidor (lambda () (apply-parallel grayscale chunks))))

(displayln (format "\n--- grayscale ---"))
(displayln (format "Secuencial:   ~a ms" tiempo-gray-seq))
(displayln (format "Paralelo:     ~a ms" tiempo-gray-par))
(displayln (format "Speedup:      ~ax" (/ tiempo-gray-seq tiempo-gray-par)))

(chunks->image chunks-inv-par w h)
(chunks->image chunks-sep-par w h)
(chunks->image chunks-gray-par w h)
