#lang racket

; Minesweeper game logic functions

; Game configuration
(define grid-size-hor 9)
(define grid-size-vrt 9)
(define mine-count 10)

; Game state variables
(define mine-field-values null)
(define clear-field-count 0)

; Generate all possible coordinates
(define (all-coordinates rows cols)
  (for*/list ([i (in-range rows)]
              [j (in-range cols)])
    (cons i j)))

; Check if coordinates are within bounds
(define (cord-in-bounds row col rows cols)
  (and (>= row 0) (>= col 0) (< row rows) (< col cols)))

; Get adjacent fields coordinates
(define (adjacent-fields row col rows cols)
  (filter
   (lambda (x) (cord-in-bounds (car x) (cdr x) rows cols))
   (list (cons (- row 1) (- col 1))
         (cons (- row 1) col)
         (cons (- row 1) (+ col 1))
         (cons row (- col 1))
         (cons row (+ col 1))
         (cons (+ row 1) (- col 1))
         (cons (+ row 1) col)
         (cons (+ row 1) (+ col 1)))))

; Generate the mine field with random mine placement
(define (generate-mine-field rows cols)
  (define mine-positions (take (shuffle (all-coordinates rows cols)) mine-count))
  (for/list ([row (in-range rows)])
    (for/list ([col (in-range cols)])
      ; Set value to "*" for mines or to the count of adjacent mines
      (if (member (cons row col) mine-positions)
          "💣"
          (length (set-intersect (adjacent-fields row col rows cols) mine-positions))))))

; Get value at specific coordinates
(define (mine-field-value row col) 
  (list-ref (list-ref mine-field-values row) col))

; Accessor and mutator functions for game state
(define (get-clear-field-count) clear-field-count)
(define (set-clear-field-count! value) (set! clear-field-count value))
(define (reset-clear-field-count!) (set! clear-field-count 0))
(define (increment-clear-field-count!) (set! clear-field-count (+ clear-field-count 1)))

(define (get-mine-field-values) mine-field-values)
(define (set-mine-field-values! values) (set! mine-field-values values))

; Provide functions to be used by the GUI module
(provide
 grid-size-hor
 grid-size-vrt
 mine-count
 
 ; Game state accessors
 get-clear-field-count
 set-clear-field-count!
 reset-clear-field-count!
 increment-clear-field-count!
 get-mine-field-values
 set-mine-field-values!
 
 ; Game logic functions
 all-coordinates
 cord-in-bounds
 adjacent-fields
 generate-mine-field
 mine-field-value
)