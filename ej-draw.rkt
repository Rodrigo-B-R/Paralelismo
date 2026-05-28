#lang racket




(require racket/draw)

(define start-seq (current-inexact-milliseconds))
; abre image, regresa argb
(define bmp (make-object bitmap% "images/new-york-large.jpg"))
bmp

; ancho y altura 
(define w (send bmp get-width))
(define h (send bmp get-height))


; prepara pixeles
(define pixel-bytes (make-bytes (* w h 4)))  

; extrae pixeles
(send bmp get-argb-pixels 0 0 w h pixel-bytes)  

; lista plana en ARGB
(define pixels (bytes->list pixel-bytes)   )

(define end-seq (current-inexact-milliseconds))
(displayln (format "\ntiempo: ~a ms\n" (- end-seq start-seq) ) )

;(take pixels 16)  ; primeros 4 pixeles


; (define (invert lista)
;     ; aplica el filtro
;     ; A (255-R) (255-G) (255-B)
;     ; alpha queda igual, es 255. rgb participa en la resta
; )

; (define pix-inveted (invert pixels) ) 


; ; para ver la imagen nueva:
; ; reconvierte de pixeles a bytes con (list->bytes pix-inveted)
; ; crea un bmp nuevo con make-bitma, medidas w h
; ; set-argb-pixels para agregar los pixeles nuevos al bmp


; (define pixel-bytes2 (list->bytes pix-inveted))

; (define bmp2 (make-bitmap w h))
; (send bmp2 set-argb-pixels 0 0 w h pixel-bytes2)
; bmp2


; ; Ahora:
; ; Aplica el proceso en paralelo
; ; Averigua cuanto tarda 

