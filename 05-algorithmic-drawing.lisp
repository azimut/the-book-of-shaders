(in-package :the-book-of-shaders)

;;----------------------------------------
;; Shaping functions

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         
         ;;; the "function"
         (y (x st))
         ;;(y (pow (x st) 2))
         ;;(y (sqrt(x st)))
         ;;(y (* .4 (cos (x st))))         
         
         ;; adds the plot
         (color (v3! y))
         (pct   (g-plot st y))

         ;; resets the colors of the background
         (color (+ (* (- 1f0 pct) color)
                   ;; makes it green
                   (* pct (v! 0 1 0)))))
    (v! color 1)))

;;----------------------------------------
;; Step and smoothstep

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         
         ;;; interpolation functions
         ;;; step() will return 0.0 unless the value
         ;;; is over .5 in which case will return 1.0
         ;;(y (step .5 (x st)))
         ;;(y (smoothstep .1 .9 (x st)))
         (y (- (smoothstep .2 .5 (x st))
               (smoothstep .5 .8 (x st))))
         
         ;; adds the plot
         (color (v3! y))
         (pct   (g-plot st y))

         ;; resets the colors of the background
         (color (+ (* (- 1f0 pct) color)
                   ;; makes it green
                   (* pct (v! 0 1 0)))))
    (v! color 1)))

;;----------------------------------------
;; Sine and cosine

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
           ;; 1
;;          (sin x)
           ;; 2
;;;           (sin (- time x))
;;           (sin (+ time x))
           ;; 3
;           (sin (* 3.1415926535 x))
           ;; 4
;;           (tan (* time x))
;;          (sin (* (- time 100000) x))
           ;; 5
          ;; (+ 1.0 (sin x))
           ;; 6
;;          (* 2 (sin x))
           ;; 7
;;           (abs (sin x)) ;; ping-pong
           ;; 8
           ;;(fract (sin x))
          ;; (- (sin x) (fract (sin x)))
           ;; 9
          ;; (+ (floor (sin x)) (ceil (sin x)))
           )
         uv
         (v! -7 7 -3 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some extra useful functions ;;;

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
;;           (mod (sin x) .5)
;;           (pow (+ -2 x) 100)
           ;; (- (pow (+ -1 x) 2)
           ;;    (pow x 3))
;;           (clamp (* 1.3 (sin (* 7 3.1415 x))) 0 1)
           ;;           (+ .9 (- (clamp x 0 1)))
           ;;           (abs (+ -1 (* 2 x)))
           ;;           (clamp (* 2 (+ .5  (- x))) -2 2)
;;           (* 4 (+ 1 (- (clamp (+ .5 x) -1 2))))
;;           (+  (* 3 (cos (+ (* .2  (- x)) time))))
           ;;           (+ -.9  (pow (+ .3 x) 3))
           ;;           (* (abs (cos time)) x)
           (cos (* 16 x))
;;           (abs (cos x))
           ;;(fract x)
           ;; (ceil x)
           ;; (floor x)
           ;; (sign x)
;;           (ceil x)
;;           (* .1 (ceil (* 100 x)))
           ;; (clamp x 0 1)
           ;; (min 0 x)
           ;; (max 0 x)
           )
         uv
         (v! -1 1 -1 1)))w



;;--------------------------------------------------
;; END

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
           ;; triangle wave
;;           (* 4 (abs (- (/ 2) (fract (+ (/ 4) (* x (/ 2)) ))) ))
           ;;           (step (cos (* 10 x)) .1)
           (sin (* 3.1415 2 x))
;;           (* .1 (ceil (* 10 x)))
           )
         uv
         (v! 0 1 -1 1)))

;;--------------------------------------------------

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (+ (graph (lambda ((x :float))
              (- (cos (* 3.1415 x)))
              )
            uv
            (v! -1 1 -1 1)
            (v! 1 0 0 0.004)
            )
     (graph (lambda ((x :float))
              (cos (* 3.1415 x)))
            uv
            (v! -1 1 -1 1)
            (v! 0 1 0 0.004))
     ;; (graph (lambda ((x :float))
     ;;          (cos (* 12 x)))
     ;;        uv
     ;;        (v! -1 1 -1 1)
     ;;        (v! 0 0 1 0.004))
     ))
