(in-package :the-book-of-shaders)

(defun-g g-plot ((st :vec2) (pct :float))
  (- (smoothstep (- pct .02) pct (y st))
     (smoothstep pct (+ pct .02) (y st))))

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         
         ;;; the "function"
         ;; (y (x st))
         ;; (y (pow (x st) 5))
         ;; (y (sqrt(x st)))
         ;; (y (* .4 (cos (x st))))
         
         ;;; interpolation functions
         ;;; step() will return 0.0 unless the value
         ;;; is over .5 in which case will return 1.0
         ;;(y (step .5 (x st)))

         (y (smoothstep .1 .9 (x st)))
         
         (color (v3! y))
         ;; adds the plot
         (pct (g-plot st y))
         ;; resamples plot, due zero where there is no plot
         (color (+ (* (- 1 pct) color)
                   ;; makes it green
                   (* pct (v! 0 1 0))))
         )
    (v! color 1)))

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
           ;; 1
;;;           (sin x)
           ;; 2
;;;           (sin (- time x))
;;;           (sin (+ time x))
           ;; 3
;;;           (sin (* 3.1415926535 x))
           ;; 4
;;;           (sin (* time x))
;;;           (sin (* (- time 100000) x))
           ;; 5
;;;           (+ 1.0 (sin x))
           ;; 6
;;;           (* 2 (sin x))
           ;; 7
           ;;(abs (sin x)) ;; ping-pong
           ;; 8
           ;;(fract (sin x))
           ;;(- (sin x) (fract (sin x)))
           ;; 9
           (+ (floor (sin x)) (ceil (sin x)))
           )
         uv
         (v! -7 7 -3 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some extra useful functions ;;;


(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
;           (fract x)
;           (ceil x)
;           (floor x)
;           (sign x)
           (abs x)
           (clamp x 0 1)
;           (min 0 x)
;           (max 0 x)
           )
         uv
         (v! -7 7 -3 3)))
