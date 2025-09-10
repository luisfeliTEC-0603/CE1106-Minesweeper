#lang racket

; Minesweeper GUI implementation
(require racket/gui/base)
(require "Minesweeper-logic.rkt") ; Import the functions module

; Variables configurables (usadas en todo el juego)
(define grid-size-hor (box 8))   ; columnas
(define grid-size-vrt (box 8))   ; filas
(define mine-count  (box 10))    ; número de minas

; ================================
; Global variables
; ================================
(define mine-field-buttons null)
(define game-over #f)
(define frame #f)
(define new-game-button #f)
(define panel #f)
(define current-game-state initial-game-state)
(define main-menu-frame #f)
; ========================
; MENÚ PRINCIPAL
; ========================

(define (start-game-from-menu rows-input cols-input difficulty-choice)
  (define user-rows (string->number (send rows-input get-value)))
  (define user-cols (string->number (send cols-input get-value)))

  (cond
  [(or (not (number? user-rows)) (not (number? user-cols)))
   (send (new message%
              [parent main-menu-frame]
              [label "Por favor ingresa números válidos para filas y columnas"])
         show #t)
   (error "Entrada no numérica")]
  
  [(or (<= user-rows 0) (<= user-cols 0))
   (send (new message%
              [parent main-menu-frame]
              [label "Las filas y columnas deben ser mayores a 0"])
         show #t)
   (error "Valores menores o iguales a 0")]
  
  [(= (* user-rows user-cols) 0)
   (send (new message%
              [parent main-menu-frame]
              [label "La cuadrícula no puede ser 0x0. Mínimo 1x1"])
         show #t)
   (error "Cuadrícula 0x0")]
  
  [(> (* user-rows user-cols) 400)  ; Opcional: límite máximo para evitar problemas de rendimiento
   (send (new message%
              [parent main-menu-frame]
              [label "La cuadrícula es demasiado grande. Máximo 400 celdas (ej: 20x20)"])
         show #t)
   (error "Cuadrícula demasiado grande")])

  (define user-percent
    (cond [(= (send difficulty-choice get-selection) 0) 0.10]   ; Fácil: 10%
          [(= (send difficulty-choice get-selection) 1) 0.15]   ; Medio: 15%
          [else 0.20]))                                         ; Difícil: 20%

  (define total-cells (* user-rows user-cols))
  (define calculated-mines (inexact->exact (floor (* total-cells user-percent))))
  
  (printf "Debug: Filas: ~a, Columnas: ~a, Total celdas: ~a, Porcentaje: ~a%, Minas calculadas: ~a~%"
          user-rows user-cols total-cells (* user-percent 100) calculated-mines)
  
  (set-box! mine-count (max 1 calculated-mines)) ; Mínimo 1 mina
  (set-box! grid-size-hor user-cols)
  (set-box! grid-size-vrt user-rows)

  (send main-menu-frame show #f)
  (create-gui))

(define (show-main-menu)
  ; Usar la variable global main-menu-frame, no crear una local
  (set! main-menu-frame
        (new frame%
             [label "Minesweeper - Menú Principal"]
             [width 350] [height 250]))

  ;; Mensaje de bienvenida
  (new message%
       [parent main-menu-frame]
       [label "🎮 Bienvenido a Minesweeper"])

  ;; Entradas para filas y columnas
  (define rows-input
    (new text-field% [parent main-menu-frame] [label "Columnas:"] [init-value "8"]))
  (define cols-input
    (new text-field% [parent main-menu-frame] [label "Filas:"] [init-value "8"]))

  ;; Elección de dificultad
  (define difficulty-choice
    (new radio-box%
         [parent main-menu-frame]
         [label "Dificultad (% minas)"]
         [choices (list "Fácil (10%)" "Medio (15%)" "Difícil (20%)")]
         [style '(vertical)]))

  ;; Botón iniciar juego
  (new button%
       [parent main-menu-frame]
       [label "Iniciar Juego"]
       [callback (lambda (b e)
                   (start-game-from-menu rows-input cols-input difficulty-choice))])

  ;; Botón salir
  (new button%
       [parent main-menu-frame]
       [label "Salir"]
       [callback (lambda (b e) (exit))])

  ;; Mostrar menú
  (send main-menu-frame show #t))

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
  (for ([row (in-range (unbox grid-size-vrt))])
    (for ([col (in-range (unbox grid-size-hor))])
      (define field-value (mine-field-value row col (get-mine-field-values current-game-state)))
      (define field-button (mine-field-button row col))
      (cond [(equal? field-value "💣") (send field-button set-label field-value)]))))

; Handle game win
(define (win-game)
  (set! game-over #t)
  (send new-game-button set-label "😎")
  (send (new dialog%
             [parent frame]
             [label "¡Ganaste!"]
             [min-width 200]
             [min-height 50]) show #t))

; Clear a field (reveal it)
(define (clear-field button field-value)
  (cond [(equal? (send button get-label) "")
         (begin
           (set-button-label button field-value)
           (set! current-game-state (increment-clear-field-count current-game-state))
           (cond [(= (get-clear-field-count current-game-state) 
                     (- (* (unbox grid-size-vrt) (unbox grid-size-hor)) (unbox mine-count))) 
                  (win-game)]))]))

; Try to clear a field (handle mine or number)
(define (try-clear-field row col)
  (define field-values (get-mine-field-values current-game-state))
  (define field-value (mine-field-value row col field-values))
  (define field-button (mine-field-button row col))
  (cond [(number? field-value)
         (cond [(= 0 field-value)
                (clear-0-fields row col)]
               [else
                (clear-field field-button field-value)])]
        [else (loose-game)]))

; Clear adjacent fields for zero-value fields
(define (clear-0-fields row col)
  (define field-values (get-mine-field-values current-game-state))
  (define field-value (mine-field-value row col field-values))
  (define field-button (mine-field-button row col))
  (cond
    [(and (not (equal? (send field-button get-label) "0")) (equal? 0 field-value))
     (begin
       (clear-field field-button field-value)
       (define adj-fields (adjacent-fields row col (unbox grid-size-vrt) (unbox grid-size-hor)))
       ; Recursively clear adjacent zero fields
       (for ([adjacent-field (in-list adj-fields)])
         (clear-0-fields (car adjacent-field) (cdr adjacent-field))
         ))]
    [else (clear-field field-button field-value)]))

; ================================
; Inicializar un nuevo juego
; ================================
(define (new-game)
  (send new-game-button set-label "🙂")
  (set! current-game-state 
        (set-mine-field-values 
         (reset-clear-field-count current-game-state)
         (generate-mine-field (unbox grid-size-vrt) (unbox grid-size-hor) (unbox mine-count))))
  (set! game-over #f)
  (for ([row (in-range (unbox grid-size-vrt))])
    (for ([col (in-range (unbox grid-size-hor))])
      (begin (send (mine-field-button row col) set-label "")
             (send (mine-field-button row col) enable #t)))))

; ================================
; Crear GUI principal
; ================================
(define (create-gui)
  (set! frame (new frame% 
                   [label "Minesweeper"] 
                   [width (+ (* 65 (unbox grid-size-hor)) 50)] 
                   [height (+ (* 65 (unbox grid-size-vrt)) 120)]))
  
  (set! new-game-button
        (new button%
             [parent frame]
             [label "🙂"]
             [font (make-object font% 25 'default 'normal 'ultraheavy)]
             [callback (lambda (b e) (new-game))]))
  
  (new message%
       [parent frame]
       [label "Left-Click -> Clear   |   Right-Click -> Flag"])
  
  (set! panel (new horizontal-panel% [parent frame] [stretchable-width #f]))
  
  (set! mine-field-buttons
        (for/list ([i (in-range (unbox grid-size-vrt))])
          (define sub-panel (new vertical-panel% [parent panel]))
          (for/list ([j (in-range (unbox grid-size-hor))])
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
                             (cond [(equal? "" (send b get-label)) 
                                    (try-clear-field i j)]))]))))
  
  ; CORRECCIÓN IMPORTANTE: Pasar el número de minas a generate-mine-field
  (set! current-game-state 
        (set-mine-field-values 
         current-game-state 
         (generate-mine-field (unbox grid-size-vrt) (unbox grid-size-hor) (unbox mine-count))))
  
  (send frame show #t))

(show-main-menu)