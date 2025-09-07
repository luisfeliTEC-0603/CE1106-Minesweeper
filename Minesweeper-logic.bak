#lang racket

; Minesweeper game logic functions - Pure Functional Implementation

; Game configuration
(define grid-size-hor 9)
(define grid-size-vrt 9)
(define mine-count 10)

; Game state as immutable data structure
(define initial-game-state
  (cons null 0)) ; (mine-field-values . clear-field-count)

; Helper functions for game state
(define (get-mine-field-values state)
  (car state))

(define (get-clear-field-count state)
  (cdr state))

(define (set-mine-field-values state values)
  (cons values (cdr state)))

(define (set-clear-field-count state count)
  (cons (car state) count))

(define (reset-clear-field-count state)
  (cons (car state) 0))

(define (increment-clear-field-count state)
  (cons (car state) (+ (cdr state) 1)))

; Generate all possible coordinates using recursion
(define (all-coordinates rows cols)
  (define (row-coords row col acc)
    (cond
      [(>= col cols) acc]
      [else (row-coords row (+ col 1) (cons (cons row col) acc))]))
  
  (define (all-rows current-row acc)
    (cond
      [(>= current-row rows) acc]
      [else (all-rows (+ current-row 1) 
                     (append (row-coords current-row 0 '()) acc))]))
  
  (all-rows 0 '()))

; Check if coordinates are within bounds
(define (cord-in-bounds row col rows cols)
  (and (>= row 0) (>= col 0) (< row rows) (< col cols)))

; Get adjacent fields coordinates
(define (adjacent-fields row col rows cols)
  (define all-adjacent
    (list (cons (- row 1) (- col 1))
          (cons (- row 1) col)
          (cons (- row 1) (+ col 1))
          (cons row (- col 1))
          (cons row (+ col 1))
          (cons (+ row 1) (- col 1))
          (cons (+ row 1) col)
          (cons (+ row 1) (+ col 1))))
  
  (filter (lambda (coord) 
            (cord-in-bounds (car coord) (cdr coord) rows cols))
          all-adjacent))

; Check if a coordinate is in a list of coordinates
(define (member? coord coord-list)
  (cond
    [(null? coord-list) #f]
    [(and (= (car coord) (car (car coord-list)))
          (= (cdr coord) (cdr (car coord-list)))) #t]
    [else (member? coord (cdr coord-list))]))

; Set intersection using recursion
(define (set-intersect set1 set2)
  (cond
    [(null? set1) '()]
    [(member? (car set1) set2)
     (cons (car set1) (set-intersect (cdr set1) set2))]
    [else (set-intersect (cdr set1) set2)]))

; Count adjacent mines for a position
(define (count-adjacent-mines row col mine-positions rows cols)
  (define adj-coords (adjacent-fields row col rows cols))
  (length (set-intersect adj-coords mine-positions)))

; Generate a row of the mine field
(define (generate-row row cols mine-positions rows)
  (define (generate-col col acc)
    (cond
      [(>= col cols) (reverse acc)]
      [else 
       (define current-pos (cons row col))
       (cond
         [(member? current-pos mine-positions)
          (generate-col (+ col 1) (cons "💣" acc))]
         [else
          (define adj-mines (count-adjacent-mines row col mine-positions rows cols))
          (generate-col (+ col 1) (cons adj-mines acc))])]))
  
  (generate-col 0 '()))

; Take first n elements from a list (functional implementation)
(define (take lst n)
  (cond
    [(or (<= n 0) (null? lst)) '()]
    [else (cons (car lst) (take (cdr lst) (- n 1)))]))

; Fisher-Yates shuffle implementation (functional version)
(define (shuffle lst)
  (define (iter remaining result)
    (cond
      [(null? remaining) result]
      [else
       (define index (random (length remaining)))
       (define item (list-ref remaining index))
       (define new-remaining (append (take remaining index) (drop remaining (+ index 1))))
       (iter new-remaining (cons item result))]))
  (iter lst '()))

; Drop first n elements from a list (functional implementation)
(define (drop lst n)
  (cond
    [(or (<= n 0) (null? lst)) lst]
    [else (drop (cdr lst) (- n 1))]))

; Length implementation (functional)
(define (length lst)
  (define (iter lst count)
    (cond
      [(null? lst) count]
      [else (iter (cdr lst) (+ count 1))]))
  (iter lst 0))

; Append implementation (functional)
(define (append lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [else (cons (car lst1) (append (cdr lst1) lst2))]))

; List ref implementation (functional)
(define (list-ref lst index)
  (cond
    [(null? lst) (error "Index out of bounds")]
    [(= index 0) (car lst)]
    [else (list-ref (cdr lst) (- index 1))]))

; Reverse implementation (functional)
(define (reverse lst)
  (define (iter lst acc)
    (cond
      [(null? lst) acc]
      [else (iter (cdr lst) (cons (car lst) acc))]))
  (iter lst '()))

; Filter implementation (functional)
(define (filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
    [else (filter pred (cdr lst))]))

; Generate the mine field with random mine placement
(define (generate-mine-field rows cols)
  (define all-coords (all-coordinates rows cols))
  (define mine-positions (take (shuffle all-coords) mine-count))
  
  (define (generate-rows current-row acc)
    (cond
      [(>= current-row rows) (reverse acc)]
      [else 
       (generate-rows (+ current-row 1) 
                     (cons (generate-row current-row cols mine-positions rows) acc))]))
  
  (generate-rows 0 '()))

; Get value at specific coordinates using recursion
(define (mine-field-value row col field-values)
  (define (get-row current-row rows remaining)
    (cond
      [(null? remaining) null]
      [(= current-row row) (car remaining)]
      [else (get-row (+ current-row 1) rows (cdr remaining))]))
  
  (define row-data (get-row 0 (length field-values) field-values))
  (cond
    [(null? row-data) (error "Row out of bounds")]
    [else
     (define (get-col current-col cols remaining)
       (cond
         [(null? remaining) null]
         [(= current-col col) (car remaining)]
         [else (get-col (+ current-col 1) cols (cdr remaining))]))
     (get-col 0 (length row-data) row-data)]))

; Provide functions to be used by the GUI module
(provide
 grid-size-hor
 grid-size-vrt
 mine-count
 initial-game-state
 
 ; Game state accessors
 get-mine-field-values
 get-clear-field-count
 set-mine-field-values
 set-clear-field-count
 reset-clear-field-count
 increment-clear-field-count
 
 ; Game logic functions
 all-coordinates
 cord-in-bounds
 adjacent-fields
 generate-mine-field
 mine-field-value
)