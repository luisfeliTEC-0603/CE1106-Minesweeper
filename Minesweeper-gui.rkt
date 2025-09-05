#lang racket

; Minesweeper GUI implementation
(require racket/gui/base)
(require "Minesweeper-logic.rkt") ; Import the functions module

; Global variables
(define mine-field-buttons null)
(define game-over #f)
(define frame #f)
(define new-game-button #f)
(define panel #f)

; Custom button class for right-click functionality
(define right-click-button%
  (class button%
    (init-field [right-click-callback null] [is-clickable-func (lambda () #t)])
    (super-new)
    (define/override (on-subwindow-event r e)
      (cond
        [(and (send e button-down? 'right) (is-clickable-func))
         (cond
           [(not (null? right-click-callback)) (right-click-callback this)])]
        [(is-clickable-func) (super on-subwindow-event r e)]))))

; Set button label and disable it
(define (set-button-label button value)
  (send button enable #f)
  (cond
    [(number? value) (send button set-label (number->string value))]
    [else (send button set-label value)]))

; Get button at specific coordinates
(define (mine-field-button row col) 
  (list-ref (list-ref mine-field-buttons row) col))

; Handle game over (lose)
(define (loose-game)
  (set! game-over #t)
  (send new-game-button set-label "🤕")
  ; Reveal all mines
  (for ([row (in-range grid-size-hor)])
    (for ([col (in-range grid-size-vrt)])
      (define field-value (mine-field-value row col))
      (define field-button (mine-field-button row col))
      (cond [(equal? field-value "💣") (send field-button set-label field-value)]))))

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
(define (clear-field button field-value)
  (cond [(equal? (send button get-label) "")
         (begin
           (set-button-label button field-value)
           (increment-clear-field-count!)
           (cond [(= (get-clear-field-count) (- (* grid-size-hor grid-size-vrt) mine-count)) (win-game)]))]))

; Try to clear a field (handle mine or number)
(define (try-clear-field row col)
  (define field-value (mine-field-value row col))
  (define field-button (mine-field-button row col))
  (cond [(number? field-value)
         (if (= 0 field-value)
             (clear-0-fields row col)
             (clear-field field-button field-value))]
        [else (loose-game)]))

; Clear adjacent fields for zero-value fields
(define (clear-0-fields row col)
  (define field-value (mine-field-value row col))
  (define field-button (mine-field-button row col))
  (cond
    [(and (not (equal? (send field-button get-label) "0")) (equal? 0 field-value))
     (begin
       (clear-field field-button field-value)
       (define adj-fields (adjacent-fields row col grid-size-hor grid-size-vrt))
       ; Recursively clear adjacent zero fields
       (for ([adjacent-field (in-list adj-fields)])
         (clear-0-fields (car adjacent-field) (cdr adjacent-field))
         ))]
    [else (clear-field field-button field-value)]))

; Initialize a new game
(define (new-game)
  (begin
    (send new-game-button set-label "🙂")
    (set-mine-field-values! (generate-mine-field grid-size-hor grid-size-vrt))
    (set! game-over #f)
    (reset-clear-field-count!)
    ; Reset all buttons
    (for ([row (in-range grid-size-hor)])
      (for ([col (in-range grid-size-vrt)])
        (begin (send (mine-field-button row col) set-label "")
               (send (mine-field-button row col) enable #t))))))

; Create the main GUI
(define (create-gui)
  (set! frame (new frame% [label "Minesweeper"] [width 500] [height 500]))
  
  ; Create "new game" button
  (set! new-game-button
        (new button%
             [parent frame]
             [label "🙂"]
             [font (make-object font% 25 'default 'normal 'ultraheavy)]
             [callback (lambda (b e) (new-game))]))
  
  ; Instructions message
  (new message%
       [parent frame]
       [label "Left-Click -> Clear   |   Right-Click -> Flag"])
  
  ; Create game grid
  (set! panel (new horizontal-panel% [parent frame] [stretchable-width #f]))
  (set! mine-field-buttons
        (for/list ([i (in-range grid-size-hor)])
          (define sub-panel (new vertical-panel% [parent panel]))
          (for/list ([j (in-range grid-size-vrt)])
            (new right-click-button%
                 [parent sub-panel]
                 [min-width 65]
                 [min-height 65]
                 [stretchable-width #f]
                 [stretchable-height #f]
                 [vert-margin 0]
                 [horiz-margin 0]
                 [font (make-object font% 25 'default 'normal 'ultraheavy)]
                 [label ""]
                 [is-clickable-func (lambda () (not game-over))]
                 [right-click-callback (lambda (b)
                                         (cond [(equal? "🚩" (send b get-label))
                                                (send b set-label "")]
                                               [(equal? "" (send b get-label))
                                                (send b set-label "🚩")]))]
                 [callback (lambda (b e)
                             (cond [(equal? "" (send b get-label)) (try-clear-field i j)]))]))))
  
  ; Initialize the game state
  (set-mine-field-values! (generate-mine-field grid-size-hor grid-size-vrt))
  (send frame show #t))

; Start the game
(create-gui)