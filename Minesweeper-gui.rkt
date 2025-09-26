#lang racket

; ==========================================
; Minesweeper GUI implementation with sprites
; ==========================================
(require racket/gui/base)
(require "Minesweeper-logic.rkt") ; Import the logic module

; ================================
; Configurable variables
; ================================
(define grid-size-hor (box 8))   ; columns
(define grid-size-vrt (box 8))   ; rows
(define mine-count  (box 10))    ; number of mines

; ================================
; Global variables
; ================================
(define mine-field-canvases null)
(define game-over #f)
(define frame #f)
(define new-game-button #f)
(define current-game-state initial-game-state)
(define main-menu-frame #f)
(define cell-states null)

; ===============================================
; Funciones para tamaños relativos de la pantalla
; ===============================================
(define (get-screen-dimensions)
  (with-handlers ([exn:fail? (lambda (e) 
                               (printf "Error detectando pantalla, usando 1366x768~%")
                               (values 1366 768))])
    (let-values ([(w h) (get-display-size #f)])
      (printf "DEBUG: Pantalla detectada: ~ax~a píxeles~%" w h)
      (values w h))))

(define (calculate-window-and-cell-size rows cols)
  (let-values ([(screen-width screen-height) (get-screen-dimensions)])
    
    ; AJUSTE MÁS FINO SEGÚN TAMAÑO DE PANTALLA Y CUADRÍCULA
    (let-values ([(width-percentage height-percentage horiz-margin vert-margin)
                  (cond 
                    ; Pantalla pequeña con cuadrícula grande
                    [(and (<= screen-width 1400) (> (* rows cols) 100)) 
                     (values 0.9 0.9 20 40)]   ; Máximo espacio posible
                    ; Pantalla pequeña con cuadrícula normal
                    [(<= screen-width 1400) 
                     (values 0.85 0.85 30 60)]
                    ; Pantalla grande con cuadrícula grande  
                    [(> (* rows cols) 100) 
                     (values 0.65 0.75 80 150)]
                    ; Pantalla grande normal
                    [else 
                     (values 0.7 0.8 100 180)])])
      
      (printf "DEBUG: Ajuste fino: ~a%x~a%, Márgenes: ~ax~a~%" 
              (* width-percentage 100) (* height-percentage 100) horiz-margin vert-margin)
      
      (let ([max-cell-width (/ (- (* screen-width width-percentage) horiz-margin) cols)]
            [max-cell-height (/ (- (* screen-height height-percentage) vert-margin) rows)]
            [max-size (if (<= screen-width 1400) 50 80)])  ; Límite más bajo para pantallas pequeñas
        
        (let ([cell-size (exact-round (min max-size (min max-cell-width max-cell-height)))])
          
          (printf "DEBUG: Celda: ~apx (pantalla ~ax~a)~%" cell-size screen-width screen-height)
          
          (values (+ (* cols cell-size) horiz-margin)
                  (+ (* rows cell-size) vert-margin)
                  cell-size  
                  cell-size))))))

(define (calculate-sprite-size rows cols)
  (let-values ([(window-width window-height sprite-width sprite-height) 
                (calculate-window-and-cell-size rows cols)])
    ; El tamaño del sprite es el mismo que el de la celda calculada
    (values sprite-width sprite-height)))

; ================================
; Sprite configuration 
; ================================
(define target-sprite-size 40) ; Valor por defecto, se actualizará

(define (load-and-scale-sprite path target-size)
  (define original-bmp (make-object bitmap% path))
  (define original-width (send original-bmp get-width))
  (define original-height (send original-bmp get-height))
  (define scaled-bmp (make-object bitmap% target-size target-size))
  (define dc (new bitmap-dc% [bitmap scaled-bmp]))
  (send dc set-smoothing 'aligned)
  (send dc set-scale (/ target-size original-width)
                    (/ target-size original-height))
  (send dc draw-bitmap original-bmp 0 0)
  scaled-bmp)

; Variables globales para los sprites (se inicializarán después)
(define number-sprites null)
(define mine-sprite null)
(define flag-sprite null)
(define default-sprite null)
(define sprite-width 0)
(define sprite-height 0)

(define (initialize-sprites! sprite-size)
  (set! target-sprite-size sprite-size)
  (set! number-sprites
        (for/list ([i (in-range 9)])
          (load-and-scale-sprite (string-append "Resources/Sprites/" (number->string i) ".png") sprite-size)))
  (set! mine-sprite (load-and-scale-sprite "Resources/Sprites/mine.png" sprite-size))
  (set! flag-sprite (load-and-scale-sprite "Resources/Sprites/flag.png" sprite-size))
  (set! default-sprite (load-and-scale-sprite "Resources/Sprites/hidden.png" sprite-size))
  (set! sprite-width sprite-size)
  (set! sprite-height sprite-size))

; ================================
; Cell states functions
; ================================
(define (initialize-cell-states!)
  (set! cell-states (make-vector (* (unbox grid-size-vrt) (unbox grid-size-hor)) 'covered)))

(define (get-cell-state row col)
  (vector-ref cell-states (+ (* row (unbox grid-size-hor)) col)))

(define (set-cell-state! row col state)
  (vector-set! cell-states (+ (* row (unbox grid-size-hor)) col) state))

; ================================
; Main Menu
; ================================
(define (start-game-from-menu rows-input cols-input difficulty-choice)
  (define user-rows (string->number (send rows-input get-value)))
  (define user-cols (string->number (send cols-input get-value)))

  ; Input validation
  (cond
    [(or (not (number? user-rows)) (not (number? user-cols)))
     (send (new message% [parent main-menu-frame] [label "Please enter valid numbers"]) show #t)]
    [(or (<= user-rows 0) (<= user-cols 0))
     (send (new message% [parent main-menu-frame] [label "Rows and columns must be > 0"]) show #t)]
    [(> (* user-rows user-cols) 400)
     (send (new message% [parent main-menu-frame] [label "Grid too large (max 400 cells)"]) show #t)]
    [else
     ; Calcular porcentaje de minas
     (define user-percent
       (cond [(= (send difficulty-choice get-selection) 0) 0.10]
             [(= (send difficulty-choice get-selection) 1) 0.15]
             [else 0.20]))
     (define total-cells (* user-rows user-cols))
     (set-box! mine-count (max 1 (inexact->exact (floor (* total-cells user-percent)))))
     (set-box! grid-size-hor user-cols)
     (set-box! grid-size-vrt user-rows)
     (send main-menu-frame show #f)
     (create-gui)]))

(define (show-main-menu)
  (set! main-menu-frame (new frame% [label "Minesweeper - Main Menu"] [width 350] [height 250]))
  (new message% [parent main-menu-frame] [label "🎮 Welcome to Minesweeper"])
  (define rows-input (new text-field% [parent main-menu-frame] [label "Columns:"] [init-value "8"]))
  (define cols-input (new text-field% [parent main-menu-frame] [label "Rows:"] [init-value "8"]))
  (define difficulty-choice
    (new radio-box% [parent main-menu-frame]
         [label "Difficulty (% mines)"]
         [choices '("Easy (10%)" "Medium (15%)" "Hard (20%)")]
         [style '(vertical)]))
  (new button% [parent main-menu-frame] [label "Start Game"]
       [callback (lambda (b e) (start-game-from-menu rows-input cols-input difficulty-choice))])
  (new button% [parent main-menu-frame] [label "Exit"] [callback (lambda (b e) (exit))])
  (send main-menu-frame show #t))

; ================================
; Mine cell canvas class 
; ================================
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
             [(eq? (get-cell-state row col) 'covered) (set-cell-state! row col 'flagged) (refresh)]
             [(eq? (get-cell-state row col) 'flagged) (set-cell-state! row col 'covered) (refresh)])])))
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
               [min-width sprite-width] [min-height sprite-height]
               [stretchable-width #f] [stretchable-height #f])))

; ================================
; Game logic functions
; ================================
(define (mine-field-canvas row col) (list-ref (list-ref mine-field-canvases row) col))

(define (loose-game)
  (set! game-over #t)
  (send new-game-button set-label "🤕")
  (for ([row (in-range (unbox grid-size-vrt))])
    (for ([col (in-range (unbox grid-size-hor))])
      (define val (mine-field-value row col (get-mine-field-values current-game-state)))
      (when (equal? val "💣")
        (set-cell-state! row col 'revealed)
        (send (mine-field-canvas row col) refresh)))))

(define (win-game)
  (set! game-over #t)
  (send new-game-button set-label "😎")
  (send (new dialog% [parent frame] [label "You won!"] [min-width 200] [min-height 50]) show #t))

(define (clear-field row col val)
  (when (eq? (get-cell-state row col) 'covered)
    (set-cell-state! row col 'revealed)
    (send (mine-field-canvas row col) refresh)
    (set! current-game-state (increment-clear-field-count current-game-state))
    (when (= (get-clear-field-count current-game-state)
             (- (* (unbox grid-size-vrt) (unbox grid-size-hor)) (unbox mine-count)))
      (win-game))))

(define (try-clear-field row col)
  (define val (mine-field-value row col (get-mine-field-values current-game-state)))
  (cond
    [(number? val) (if (= val 0) (clear-0-fields row col) (clear-field row col val))]
    [else (loose-game)]))

(define (clear-0-fields row col)
  (define val (mine-field-value row col (get-mine-field-values current-game-state)))
  (when (eq? (get-cell-state row col) 'covered)
    (clear-field row col val)
    (when (= val 0)
      (for ([adj (in-list (adjacent-fields row col (unbox grid-size-vrt) (unbox grid-size-hor)))])
        (define r (car adj))
        (define c (cdr adj))
        (when (eq? (get-cell-state r c) 'covered)
          (clear-0-fields r c))))))

; ================================
; New game
; ================================
(define (new-game)
  (send new-game-button set-label "🙂")
  (set! game-over #f)
  (initialize-cell-states!)
  (set! current-game-state
        (set-mine-field-values
         (reset-clear-field-count current-game-state)
         (generate-mine-field (unbox grid-size-vrt) (unbox grid-size-hor) (unbox mine-count))))
  (for ([row (in-range (unbox grid-size-vrt))])
    (for ([col (in-range (unbox grid-size-hor))])
      (send (mine-field-canvas row col) refresh))))

; ================================
; Create GUI 
; ================================
(define (create-gui)
  ; Calcular tamaños relativos
  (let-values ([(window-width window-height sprite-width sprite-height) 
                (calculate-window-and-cell-size (unbox grid-size-vrt) (unbox grid-size-hor))])
    
    (printf "RESUMEN: Ventana: ~ax~a, Sprites: ~ax~a~%" 
            window-width window-height sprite-width sprite-height)
    
    ; Inicializar sprites con el tamaño calculado
    (initialize-sprites! sprite-width)
    
    (set! frame (new frame% [label "Minesweeper"] 
                     [width window-width] [height window-height]
                     [alignment '(center center)]
                     [stretchable-width #f] [stretchable-height #f]))
    
    (define main-panel (new vertical-panel% [parent frame] [alignment '(center center)]
                            [stretchable-width #f] [stretchable-height #f]))
    
    (set! new-game-button (new button% [parent main-panel] [label "🙂"]
                               [font (make-object font% (max 20 (exact-round (* sprite-width 0.5))) 
                                                 'default 'normal 'ultraheavy)]
                               [callback (lambda (b e) (new-game))]))
    
    (new message% [parent main-panel] 
         [label "Left-Click -> Clear   |   Right-Click -> Flag"]
         [font (make-object font% (max 10 (exact-round (* sprite-width 0.2))) 
                           'default 'normal 'normal)])
    
    (define grid-container (new horizontal-panel% [parent main-panel] 
                                [alignment '(center center)]
                                [spacing 0] [border 0]))
    
    (set! mine-field-canvases
          (for/list ([i (in-range (unbox grid-size-vrt))])
            (define sub-panel (new vertical-panel% [parent grid-container]
                                   [spacing 0] [border 0]))
            (for/list ([j (in-range (unbox grid-size-hor))])
              (new mine-cell-canvas% [parent sub-panel] [row i] [col j]))))
    
    (initialize-cell-states!) ; Inicializar estados de las celdas
    (new-game) ; inicializa el juego al crear la GUI
    
    (send frame center)
    (send frame show #t)))

; ================================
; Start application 
; ================================
(show-main-menu)