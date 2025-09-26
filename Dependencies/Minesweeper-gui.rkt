; Dependencies/Minesweeper-gui.rkt
#lang racket

(require racket/gui/base)
(require "Minesweeper-logic.rkt")

; ================================
; PARAMETROS Y VARIABLES DEL JUEGO
; ================================

(define ancho-tablero (box 8))
(define alto-tablero (box 8))
(define num-minas  (box 10))

(define lienzos-campo-minas null)
(define juego-terminado? #f)
(define marco-principal #f)
(define estado-juego-actual init-estado)
(define marco-menu-principal #f)
(define estados-celdas null)

; ================================
; TAMAÑOS RELATIVOS
; ================================

(define (obtener-dimensiones-pantalla)
  (with-handlers ([exn:fail? (lambda (e) 
                               (values 1366 768))])
    (let-values ([(ancho-pantalla alto-pantalla) (get-display-size #f)])
      (values ancho-pantalla alto-pantalla))))

(define (calcular-tamaño-ventana-y-celda filas columnas)
  (let-values ([(ancho-pantalla alto-pantalla) (obtener-dimensiones-pantalla)])
    
    ; Ajustes de presición de acuerdo con pantalla
    (let-values ([(porcentaje-ancho porcentaje-alto margen-horizontal margen-vertical)
                  (cond 
                    [(and (<= ancho-pantalla 1400) (> (* filas columnas) 100)) 
                     (values 0.9 0.9 20 40)]
                    [(<= ancho-pantalla 1400) 
                     (values 0.85 0.85 30 60)]
                    [(> (* filas columnas) 100) 
                     (values 0.65 0.75 80 150)]
                    [else 
                     (values 0.7 0.8 100 180)])])

      (let ([maximo-ancho-celda (/ (- (* ancho-pantalla porcentaje-ancho) margen-horizontal) columnas)]
            [maximo-alto-celda (/ (- (* alto-pantalla porcentaje-alto) margen-vertical) filas)]
            [tamaño-maximo (if (<= ancho-pantalla 1400) 50 80)])

        (let ([tamaño-celda (exact-round (min tamaño-maximo (min maximo-ancho-celda maximo-alto-celda)))])          
          (values (+ (* columnas tamaño-celda) margen-horizontal)
                  (+ (* filas tamaño-celda) margen-vertical)
                  tamaño-celda  
                  tamaño-celda))))))

(define (calcular-tamaño-sprite filas columnas)
  (let-values ([(ancho-ventana alto-ventana ancho-sprite alto-sprite) 
                (calcular-tamaño-ventana-y-celda filas columnas)])
    (values ancho-sprite alto-sprite)))

; ================================
; CONFIGURACIÓN DE SPRITES
; ================================

(define tamaño-sprite-objetivo 40)

(define (cargar-y-escalar-sprite ruta tamaño-objetivo)
  (with-handlers ([exn:fail? 
                   (lambda (e)
                     (printf "Error cargando sprite: ~a. Usando alternativa.~%" ruta)
                     ; Crear un bitmap de respaldo
                     (define bitmap-respaldo (make-object bitmap% tamaño-objetivo tamaño-objetivo))
                     (define dc (new bitmap-dc% [bitmap bitmap-respaldo]))
                     (send dc set-brush "lightgray" 'solid)
                     (send dc set-pen "black" 1 'transparent)
                     (send dc draw-rectangle 0 0 tamaño-objetivo tamaño-objetivo)
                     (send dc set-font (make-object font% 12 'default))
                     (send dc set-text-foreground "black")
                     (send dc draw-text (format "~a" (path->string ruta)) 5 5)
                     bitmap-respaldo)])
    (define bitmap-original (make-object bitmap% ruta))
    (define ancho-original (send bitmap-original get-width))
    (define alto-original (send bitmap-original get-height))
    (define bitmap-escalado (make-object bitmap% tamaño-objetivo tamaño-objetivo))
    (define dc (new bitmap-dc% [bitmap bitmap-escalado]))
    (send dc set-smoothing 'aligned)
    (send dc set-scale (/ tamaño-objetivo ancho-original)
                      (/ tamaño-objetivo alto-original))
    (send dc draw-bitmap bitmap-original 0 0)
    bitmap-escalado))

; Variables globales para los sprites
(define sprites-numeros null)
(define sprite-mina null)
(define sprite-bandera null)
(define sprite-predeterminado null)
(define ancho-sprite 0)
(define alto-sprite 0)

(define (crear-sprite-respaldo tamaño texto)
  (define bitmap (make-object bitmap% tamaño tamaño))
  (define dc (new bitmap-dc% [bitmap bitmap]))
  (send dc set-brush "lightgray" 'solid)
  (send dc set-pen "black" 1 'solid)
  (send dc draw-rectangle 0 0 tamaño tamaño)
  (send dc set-font (make-object font% (max 8 (/ tamaño 3)) 'default))
  (send dc set-text-foreground "black")
  (send dc draw-text texto (/ tamaño 4) (/ tamaño 4))
  bitmap)

(define (inicializar-sprites! tamaño-sprite)
  (set! tamaño-sprite-objetivo tamaño-sprite)
  
  ; Intentar múltiples rutas posibles para los sprites
  (define rutas-posibles 
    (list "Resources/Sprites/" "Sprites/" "./Sprites/" "../Sprites/"))
  
  (define (intentar-cargar-sprite nombre)
    (for/first ([ruta-base rutas-posibles]
                #:when (file-exists? (string-append ruta-base nombre)))
      (cargar-y-escalar-sprite (string-append ruta-base nombre) tamaño-sprite)))
  
  (set! sprites-numeros
        (for/list ([i (in-range 9)])
          (or (intentar-cargar-sprite (string-append (number->string i) ".png"))
              (crear-sprite-respaldo tamaño-sprite (number->string i)))))
  
  (set! sprite-mina (or (intentar-cargar-sprite "mine.png")
                       (crear-sprite-respaldo tamaño-sprite "X")))
  (set! sprite-bandera (or (intentar-cargar-sprite "flag.png")
                          (crear-sprite-respaldo tamaño-sprite "F")))
  (set! sprite-predeterminado (or (intentar-cargar-sprite "hidden.png")
                                 (crear-sprite-respaldo tamaño-sprite "?")))
  (set! ancho-sprite tamaño-sprite)
  (set! alto-sprite tamaño-sprite))

; ================================
; ESTADOS DE CELDAS
; ================================

(define (inicializar-estados-celdas!)
  (set! estados-celdas (make-vector (* (unbox alto-tablero) 
                                      (unbox ancho-tablero)) 
                                   'cubierta)))

(define (obtener-estado-celda fila columna)
  (vector-ref estados-celdas (+ (* fila (unbox ancho-tablero)) columna)))

(define (establecer-estado-celda! fila columna estado)
  (vector-set! estados-celdas (+ (* fila (unbox ancho-tablero)) columna) estado))

; ================================
; ASPECTOS GRÁFICOS
; ================================

; Menú inicial 
(define (iniciar-juego-desde-menu entrada-filas entrada-columnas seleccion-dificultad)
  (define filas-usuario (string->number (send entrada-filas get-value)))
  (define columnas-usuario (string->number (send entrada-columnas get-value)))

  ; Validación de entrada
  (cond
    [(or (not (number? filas-usuario)) (not (number? columnas-usuario)))
     (send (new message% [parent marco-menu-principal] 
                [label "Por favor ingrese números válidos"]) show #t)]
    [(or (<= filas-usuario 0) (<= columnas-usuario 0))
     (send (new message% [parent marco-menu-principal] 
                [label "Las filas y columnas deben ser > 0"]) show #t)]
    [(> (* filas-usuario columnas-usuario) 400)
     (send (new message% [parent marco-menu-principal] 
                [label "Cuadrícula demasiado grande (máx 400 celdas)"]) show #t)]
    [else
     ; Calcular porcentaje de minas
     (define porcentaje-usuario
       (cond [(= (send seleccion-dificultad get-selection) 0) 0.10]
             [(= (send seleccion-dificultad get-selection) 1) 0.15]
             [else 0.20]))
     (define total-celdas (* filas-usuario columnas-usuario))
     (set-box! num-minas (max 1 (inexact->exact (floor (* total-celdas porcentaje-usuario)))))
     (set-box! ancho-tablero columnas-usuario)
     (set-box! alto-tablero filas-usuario)
     (send marco-menu-principal show #f)
     (crear-interfaz-grafica)]))

(define (mostrar-menu-principal)
  (set! marco-menu-principal (new frame% [label "BusCEMinas"] 
                                  [width 350] [height 250]))
  (new message% [parent marco-menu-principal] [label "(⌐■_■) : Bienvenido a BusCEMinas...\n"])
  (define entrada-filas (new text-field% [parent marco-menu-principal] 
                             [label "Columnas:"] [init-value "8"]))
  (define entrada-columnas (new text-field% [parent marco-menu-principal] 
                                [label "Filas:"] [init-value "8"]))
  (define seleccion-dificultad
    (new radio-box% [parent marco-menu-principal]
         [label "Dificultad (% minas)"]
         [choices '("Fácil (10%)" "Medio (15%)" "Difícil (20%)")]
         [style '(vertical)]))
  (new button% [parent marco-menu-principal] [label "Iniciar Juego"]
       [callback (lambda (boton evento) 
                  (iniciar-juego-desde-menu entrada-filas entrada-columnas seleccion-dificultad))])
  (new button% [parent marco-menu-principal] [label "Salir"] 
       [callback (lambda (boton evento) (exit))])
  (send marco-menu-principal show #t))

; Lienzo (unidad del tablero de juego)
(define lienzo-celda-mina%
  (class canvas%
    (init-field fila columna)
    (inherit refresh get-dc)
    (define/override (on-event evento)
      (when (not juego-terminado?)
        (cond
          [(send evento button-down? 'left)
           (when (eq? (obtener-estado-celda fila columna) 'cubierta)
             (intentar-descubrir-campo fila columna))]
          [(send evento button-down? 'right)
           (cond
             [(eq? (obtener-estado-celda fila columna) 'cubierta) 
              (establecer-estado-celda! fila columna 'bandera) (refresh)]
             [(eq? (obtener-estado-celda fila columna) 'bandera) 
              (establecer-estado-celda! fila columna 'cubierta) (refresh)])])))
    (define/override (on-paint)
      (define dc (get-dc))
      (case (obtener-estado-celda fila columna)
        ['cubierta (send dc draw-bitmap sprite-predeterminado 0 0)]
        ['bandera (send dc draw-bitmap sprite-bandera 0 0)]
        ['descubierta
         (define valor-campo (obtener-valor-campo fila columna 
                                (get-estado-minas estado-juego-actual)))
         (cond
           [(number? valor-campo) 
            (send dc draw-bitmap (list-ref sprites-numeros valor-campo) 0 0)]
           [(equal? valor-campo "X") 
            (send dc draw-bitmap sprite-mina 0 0)])]))
    (super-new [style '(border)] 
               [min-width ancho-sprite] [min-height alto-sprite]
               [stretchable-width #f] [stretchable-height #f])))

; ================================
; FUNCIONES LÓGICAS DEL JUEGO
; ================================

(define (obtener-lienzo-campo-minas fila columna) 
  (list-ref (list-ref lienzos-campo-minas fila) columna))

(define (mostrar-ventana-resultado mensaje)
  (define ventana-resultado (new dialog% [parent marco-principal] 
                                [label "Resultado del Juego"]
                                [width 300] [height 150]))
  
  (new message% [parent ventana-resultado] [label mensaje]
       [font (make-object font% 16 'default 'normal 'bold)])
  
  (define panel-botones (new horizontal-panel% [parent ventana-resultado]
                            [alignment '(center center)]))
  
  ; Botón para jugar otra vez con las mismas condiciones
  (new button% [parent panel-botones] [label "Jugar Otra Vez"]
       [callback (lambda (b e)
                  (send ventana-resultado show #f)
                  (nuevo-juego))])
  
  ; Botón para volver al menú principal
  (new button% [parent panel-botones] [label "Volver al Menú"]
       [callback (lambda (b e)
                  (send ventana-resultado show #f)
                  (send marco-principal show #f)
                  (mostrar-menu-principal))])
  
  (send ventana-resultado center)
  (send ventana-resultado show #t))

; Derrota
(define (perder-juego)
  (set! juego-terminado? #t)
  
  ; Revelar todas las minas
  (for ([fila (in-range (unbox alto-tablero))])
    (for ([columna (in-range (unbox ancho-tablero))])
      (define valor (obtener-valor-campo fila columna 
                       (get-estado-minas estado-juego-actual)))
      (when (equal? valor "X")
        (establecer-estado-celda! fila columna 'descubierta)
        (send (obtener-lienzo-campo-minas fila columna) refresh))))
  
  ; Mostrar ventana de resultado (derrota)
  (mostrar-ventana-resultado "  (✖﹏✖)\nYOU DIED"))

; Victoria
(define (ganar-juego)
  (set! juego-terminado? #t)
  
  ; Marcar todas las minas con bandera
  (for ([fila (in-range (unbox alto-tablero))])
    (for ([columna (in-range (unbox ancho-tablero))])
      (define valor (obtener-valor-campo fila columna 
                       (get-estado-minas estado-juego-actual)))
      (when (equal? valor "X")
        (establecer-estado-celda! fila columna 'bandera)
        (send (obtener-lienzo-campo-minas fila columna) refresh))))
  
  ; Mostrar ventana de resultado
  (mostrar-ventana-resultado " ヽ(•‿•)ノ \n¡Victoria!"))

(define (descubrir-campo fila columna valor)
  (when (eq? (obtener-estado-celda fila columna) 'cubierta)
    (establecer-estado-celda! fila columna 'descubierta)
    (send (obtener-lienzo-campo-minas fila columna) refresh)
    (set! estado-juego-actual (incrementar-contador-campos-descubiertos estado-juego-actual))
    
    ; Verificar condición de victoria
    (when (= (get-estado-minas-desc estado-juego-actual)
             (- (* (unbox alto-tablero) 
                   (unbox ancho-tablero)) 
                (unbox num-minas)))
      (ganar-juego))))

; Descubrir campo en el tablero
(define (intentar-descubrir-campo fila columna)
  (define valor (obtener-valor-campo fila columna 
                    (get-estado-minas estado-juego-actual)))
  (cond
    [(equal? valor "X") (perder-juego)]
    [(number? valor) (if (= valor 0) 
                        (descubrir-campos-cero fila columna) 
                        (descubrir-campo fila columna valor))]
    [else (perder-juego)]))

; Campo descubierto es cero
(define (descubrir-campos-cero fila columna [visitados (set)])
  (define posicion-actual (cons fila columna))
  (when (not (miembro-conjunto? visitados posicion-actual))
    (define nuevos-visitados (agregar-conjunto visitados posicion-actual))
    (define valor (obtener-valor-campo fila columna 
                      (get-estado-minas estado-juego-actual)))
    
    (when (and (eq? (obtener-estado-celda fila columna) 'cubierta) (number? valor))
      (descubrir-campo fila columna valor)
      
      (when (= valor 0)
        (for ([adyacente (in-list (get-adyacentes fila columna 
                                    (unbox alto-tablero) 
                                    (unbox ancho-tablero)))])
          (define f (car adyacente))
          (define c (cdr adyacente))
          (when (coordenada-dentro-limites? f c (unbox alto-tablero) 
                                           (unbox ancho-tablero))
            (descubrir-campos-cero f c nuevos-visitados)))))))

; ================================
; NUEVO JUEGO
; ================================

(define (nuevo-juego)
  (set! juego-terminado? #f)
  (inicializar-estados-celdas!)
  
  ; Inicializar estado del juego con nuevo campo de minas
  (set! estado-juego-actual
        (set-estado-val-minas
         (reiniciar-contador-campos-descubiertos init-estado)
         (generar-tablero (unbox alto-tablero) 
                             (unbox ancho-tablero) 
                             (unbox num-minas))))
  
  (for ([fila (in-range (unbox alto-tablero))])
    (for ([columna (in-range (unbox ancho-tablero))])
      (send (obtener-lienzo-campo-minas fila columna) refresh))))

; ================================
; FUNCIONES AUXILIARES
; ================================

(define (miembro-conjunto? conjunto elemento)
  (cond
    [(set? conjunto) (set-member? (set->list conjunto) elemento)]
    [(list? conjunto) (member elemento conjunto equal?)]
    [else #f]))

(define (agregar-conjunto conjunto elemento)
  (if (miembro-conjunto? conjunto elemento)
      conjunto
      (cons elemento conjunto)))

; ================================
; TABLERO
; ================================

(define (crear-interfaz-grafica)
  (let-values ([(ancho-ventana alto-ventana ancho-sprite alto-sprite) 
                (calcular-tamaño-ventana-y-celda (unbox alto-tablero) 
                                                (unbox ancho-tablero))])
    
    (inicializar-sprites! ancho-sprite)
    
    (set! marco-principal (new frame% [label "BusCEMinas"] 
                               [width ancho-ventana] [height alto-ventana]
                               [alignment '(center center)]
                               [stretchable-width #f] [stretchable-height #f]))
    
    (define panel-principal (new horizontal-panel% [parent marco-principal] 
                                 [alignment '(center center)]
                                 [stretchable-width #f] [stretchable-height #f]))
    
    (set! lienzos-campo-minas
          (for/list ([i (in-range (unbox alto-tablero))])
            (define sub-panel (new vertical-panel% [parent panel-principal]
                                   [spacing 0] [border 0]))
            (for/list ([j (in-range (unbox ancho-tablero))])
              (new lienzo-celda-mina% [parent sub-panel] [fila i] [columna j]))))
    
    (inicializar-estados-celdas!)
    (nuevo-juego)
    
    (send marco-principal center)
    (send marco-principal show #t)))

; Exportar funciones necesarias a main.rkt
(provide mostrar-menu-principal)