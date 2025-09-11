#lang racket

; Minesweeper GUI implementation with sprites
(require racket/gui/base)
(require "Minesweeper-logic.rkt") ; Import the functions module

; Global variables
(define mine-field-canvases null)
(define game-over #f)
(define frame #f)
(define new-game-button #f)
(define panel #f)
(define current-game-state initial-game-state) ; Use the functional game state

; Load sprites
(define number-sprites
  (for/list ([i (in-range 9)])
    (make-object bitmap% (string-append "sprites/" (number->string i) ".png"))))

(define mine-sprite (make-object bitmap% "sprites/mine.png"))
(define flag-sprite (make-object bitmap% "sprites/flag.png"))
(define default-sprite (make-object bitmap% "sprites/default.png")) ; Assuming you have a default/covered sprite

; Get sprite dimensions (assuming all sprites are the same size)
(define sprite-width (send default-sprite get-width))
(define sprite-height (send default-sprite get-height))

; Cell state tracking
(define cell-states (make-vector (* grid-size-hor grid-size-vrt) 'covered)) ; covered, revealed, or flagged

; Get cell state
(define (get-cell-state row col)
  (vector-ref cell-states (+ (* row grid-size-vrt) col)))

; Set cell state
(define (set-cell-state! row col state)
  (vector-set! cell-states (+ (* row grid-size-vrt) col) state))

; Custom canvas class for minefield cells
(define mine-cell-canvas%
  (class canvas%
    (init-field row col)
    (inherit refresh get-dc)
    
    (define/override (on-event event)
      (when (not game-over)
        (cond
          [(send event button-down? 'left)
           (when (eq? (get-cell-state row col) 'covered)
             (try-clear-field row col))]
          [(send event button-down? 'right)
           (cond
             [(eq? (get-cell-state row col) 'covered)
              (set-cell-state! row col 'flagged)
              (refresh)]
             [(eq? (get-cell-state row col) 'flagged)
              (set-cell-state! row col 'covered)
              (refresh)])])))
    
    (define/override (on-paint)
      (define dc (get-dc))
      (case (get-cell-state row col)
        ['covered (send dc draw-bitmap default-sprite 0 0)]
        ['flagged (send dc draw-bitmap flag-sprite 0 0)]
        ['revealed
         (define field-value (mine-field-value row col (get-mine-field-values current-game-state)))
         (cond
           [(number? field-value) (send dc draw-bitmap (list-ref number-sprites field-value) 0 0)]
           [(equal? field-value "💣") (send dc draw-bitmap mine-sprite 0 0)])]))
    
    (super-new [style '(border)] 
               [min-width 16] 
               [min-height 16]
               [stretchable-width #f]
               [stretchable-height #f])))

; Get canvas at specific coordinates
(define (mine-field-canvas row col) 
  (list-ref (list-ref mine-field-canvases row) col))

; Handle game over (lose)
(define (loose-game)
  (set! game-over #f)
  (send new-game-button set-label "🤕")
  ; Reveal all mines
  (for ([row (in-range grid-size-hor)])
    (for ([col (in-range grid-size-vrt)])
      (define field-value (mine-field-value row col (get-mine-field-values current-game-state)))
      (when (equal? field-value "💣")
        (set-cell-state! row col 'revealed)
        (send (mine-field-canvas row col) refresh)))))

; Handle game win
(define (win-game)
  (set! game-over #t)
  (send new-game-button set-label "😎")
  (send (new dialog%
             [parent frame]
             [label "You won!"]
             [min-width 200]
             [min-height 50]) show #t))

; Clear a field (reveal it)
(define (clear-field row col field-value)
  (when (eq? (get-cell-state row col) 'covered)
    (set-cell-state! row col 'revealed)
    (send (mine-field-canvas row col) refresh)
    (set! current-game-state (increment-clear-field-count current-game-state))
    (when (= (get-clear-field-count current-game-state) 
             (- (* grid-size-hor grid-size-vrt) mine-count))
      (win-game))))

; Try to clear a field (handle mine or number)
(define (try-clear-field row col)
  (define field-values (get-mine-field-values current-game-state))
  (define field-value (mine-field-value row col field-values))
  (cond
    [(number? field-value)
     (if (= 0 field-value)
         (clear-0-fields row col)
         (clear-field row col field-value))]
    [else (loose-game)]))

; Clear adjacent fields for zero-value fields
(define (clear-0-fields row col)
  (define field-values (get-mine-field-values current-game-state))
  (define field-value (mine-field-value row col field-values))
  (when (and (eq? (get-cell-state row col) 'covered) (equal? 0 field-value))
    (clear-field row col field-value)
    (define adj-fields (adjacent-fields row col grid-size-hor grid-size-vrt))
    (for ([adjacent-field (in-list adj-fields)])
      (clear-0-fields (car adjacent-field) (cdr adjacent-field)))))

; Initialize a new game
(define (new-game)
  (send new-game-button set-label "🙂")
  (set! current-game-state 
        (set-mine-field-values 
         (reset-clear-field-count current-game-state)
         (generate-mine-field grid-size-hor grid-size-vrt)))
  (set! game-over #f)
  ; Reset all cell states
  (set! cell-states (make-vector (* grid-size-hor grid-size-vrt) 'covered))
  ; Refresh all canvases
  (for ([row (in-range grid-size-hor)])
    (for ([col (in-range grid-size-vrt)])
      (send (mine-field-canvas row col) refresh))))

; Create the main GUI
(define (create-gui)
  ; Calculate window size based on grid and sprite dimensions
  (define window-width (+ 50 (* grid-size-vrt sprite-width)))
  (define window-height (+ 150 (* grid-size-hor sprite-height)))
  
  (set! frame (new frame% 
                   [label "Minesweeper"] 
                   [width window-width] 
                   [height window-height]
                   [stretchable-width #f]  ; Prevent window resizing
                   [stretchable-height #f]))
  
  ; Create main panel to hold everything
  (define main-panel (new vertical-panel% 
                          [parent frame]
                          [alignment '(center center)]
                          [stretchable-width #f]
                          [stretchable-height #f]))
  
  ; Create "new game" button
  (set! new-game-button
        (new button%
             [parent main-panel]
             [label "🙂"]
             [font (make-object font% 25 'default 'normal 'ultraheavy)]
             [callback (lambda (b e) (new-game))]))
  
  ; Instructions message
  (new message%
       [parent main-panel]
       [label "Left-Click -> Clear   |   Right-Click -> Flag"])
  
  ; Create game grid container
  (define grid-container (new horizontal-panel% 
                              [parent main-panel] 
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [alignment '(center center)]))
  
  ; Create game grid
  (set! mine-field-canvases
        (for/list ([i (in-range grid-size-hor)])
          (define sub-panel (new vertical-panel% 
                                 [parent grid-container] 
                                 [stretchable-width #f]
                                 [stretchable-height #f]))
          (for/list ([j (in-range grid-size-vrt)])
            (new mine-cell-canvas% 
                 [parent sub-panel] 
                 [row i] 
                 [col j]))))
  
  ; Initialize the game state
  (set! current-game-state 
        (set-mine-field-values 
         current-game-state 
         (generate-mine-field grid-size-hor grid-size-vrt)))
  (send frame show #t))

; Start the game
(create-gui)