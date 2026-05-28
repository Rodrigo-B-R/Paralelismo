#lang racket
(require 2htdp/image) 


(define start-seq (current-inexact-milliseconds))
; lee una imagen y crea mapa de bits
(define img (bitmap/file "images/new-york-large.jpg"))
img


; obtener la altura
(define w (image-width  img) )
(define h (image-height img) )

; convierte mapa de bits a lista de colores
; unidimensional en RGBA (son objetos)
(define pixels (time (image->color-list img)) ) 

;(define pixels (time (image->color-list img)) ) ; task

(define end-seq (current-inexact-milliseconds))
(displayln (format "\ntiempo: ~a ms\n" (- end-seq start-seq) ) )


;el primer pixel
(define p1 (car pixels) )
p1





;(define img (bitmap/file "new-york-large.jpg")) ; imagen grande

; escalado 
;(scale 0.25 img)