#lang racket
(require racket/gui/base)

;; --------------------------
;; Crear el frame principal
;; --------------------------
(define frame (new frame% [label "Sprite Viewer"] [width 500] [height 300]))

;; --------------------------
;; Cargar sprites 0.png a 8.png
;; --------------------------
(define number-sprites
  (for/list ([i (in-range 9)])
    (make-object bitmap% (string-append "sprites/" (number->string i) ".png"))))

;; Cargar mine y flag
(define mine-sprite (make-object bitmap% "sprites/mine.png"))
(define flag-sprite (make-object bitmap% "sprites/flag.png"))

;; Combinar todos los sprites en una lista
(define all-sprites (append number-sprites (list mine-sprite flag-sprite)))

;; --------------------------
;; Variables para manejo de sprites clickeables
;; --------------------------
(define sprite-indexes (make-vector (length all-sprites) 0)) ; índice actual de cada sprite

;; Posiciones de los sprites en el canvas (x, y)
(define sprite-positions
  (for/list ([i (in-range (length all-sprites))])
    (cons (+ 10 (* 50 i)) 10))) ; x = 10 + 50*i, y = 10

;; --------------------------
;; Crear el canvas con clase personalizada para manejar eventos
;; --------------------------
(define sprite-canvas%
  (class canvas%
    (inherit refresh get-dc) ; Inherit get-dc method
    
    (define/override (on-event event)
      (when (send event button-down? 'left)
        (define click-x (send event get-x))
        (define click-y (send event get-y))
        ;; Revisar cada sprite
        (for ([i (in-range (length all-sprites))])
          (define pos (list-ref sprite-positions i))
          (when (and (>= click-x (car pos))
                     (< click-x (+ (car pos) 40)) ; ancho sprite aproximado
                     (>= click-y (cdr pos))
                     (< click-y (+ (cdr pos) 40))) ; alto sprite aproximado
            ;; Cambiar al siguiente sprite
            (vector-set! sprite-indexes i
                         (modulo (+ 1 (vector-ref sprite-indexes i))
                                 (length all-sprites)))
            (refresh)))))
    
    (define/override (on-paint)
      (define dc (get-dc))
      (for ([i (in-range (length all-sprites))])
        (define img (list-ref all-sprites (vector-ref sprite-indexes i)))
        (define pos (list-ref sprite-positions i))
        (send dc draw-bitmap img (car pos) (cdr pos))))
    
    (super-new)))

;; --------------------------
;; Crear el canvas
;; --------------------------
(define canvas (new sprite-canvas% [parent frame]))

;; --------------------------
;; Mostrar el frame
;; --------------------------
(send frame show #t)