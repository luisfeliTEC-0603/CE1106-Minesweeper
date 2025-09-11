#lang racket
(require racket/gui/base)

;; Create the main frame
(define frame (new frame% [label "Sprite Viewer"] [width 500] [height 300]))

;; Load sprites 0.png to 8.png
(define number-sprites
  (for/list ([i (in-range 9)])
    (make-object bitmap% (string-append "sprites/" (number->string i) ".png"))))

;; Load mine and flag
(define mine-sprite (make-object bitmap% "sprites/mine.png"))
(define flag-sprite (make-object bitmap% "sprites/flag.png"))

;; Combine all sprites for easy iteration
(define all-sprites (append number-sprites (list mine-sprite flag-sprite)))

;; Create a canvas
(define canvas
  (new canvas%
       [parent frame]
       [paint-callback
        (lambda (canvas dc)
          (define x 10)
          (define y 10)
          ;; Draw each sprite horizontally
          (for-each
           (lambda (img)
             (send dc draw-bitmap img x y)
             (set! x (+ x 50))) ; spacing between images
           all-sprites))]))

;; Show the frame
(send frame show #t)
