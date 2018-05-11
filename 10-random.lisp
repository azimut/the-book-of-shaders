(in-package :the-book-of-shaders)

;;;;;;;;;;;;;;
;;; Random ;;;

;; We are extracting the fractional content of a sine wave.
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float)) (fract (* (sin x)
                                   1f0)))
         uv
         (v! -7 7 -1 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Controlling chaos ;;;

;; https://pixelero.wordpress.com/2008/04/24/various-functions-and-various-distributions-with-mathrandom/

;; Deterministic random
(defun-g g-rand ((x :float))
  (fract (* (sin x)
            100000f0)))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
           (g-rand x)
;;;           (sqrt (g-rand x)) ;; move things away from zero
;;;           (* (g-rand x) (g-rand x)) ;; moves things closer (bis) to zero
;;;           (pow (g-rand x) 5) ;; ditto
           )
         uv
         (v! -1 1 -1 2)))

;;;;;;;;;;;;;;;;;
;;; 2D Random ;;;

;; Try changing the fixed values. See how the random pattern changes and think about what we can learn from this.
;; Hook this random function to the mouse interaction (u_mouse) and time (u_time) to understand better how it works.

(defun-g g-random ((st :vec2))
  (fract (* (sin (dot st
                      (v! 12.9898
                          78.233
                          )))
            43758.543123)))

(defun-g g-random ((st :vec2) (time :float))
  (fract (* (sin (dot st
                      (v! 12.9898 ;; moves noise
                          (tan time)      ;;;78.233
                          )))
;;;            43758.543123
            (abs (* 2 (cos time)))
            )))

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)

               (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (rnd (g-random st time)))
    (v! (v3! rnd) 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Using the chaos ;;;

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st   (* st 10))  ;; scale coord by 10
         (ipos (floor st)) ;; get the integer coords
         (fpos (fract st)) ;; get the fractional coords
         ;; assign a random value based on the integer coord
         (color (v3! (g-random ipos time)))
         ;; subdivided grid view
;;         (color (v! fpos 0f0))
         )
    (v! color 1)))

;;--------------------------------------------------

(defun-g truchet-pattern ((st :vec2) (index :float))
  (let ((index (fract (* 2.0 (- index .5)))))
    (cond ((> index .75) (setf st (- (v2! 1.0) st)))
          ((> index .5)  (setf st (v! (- 1.0 (x st))
                                      (y st))))
          ((> index .25) (setf st (- (v2! 1.0) (v! (- 1.0 (x st))
                                                   (y st))))))
    st))

(defun-g frag ((uv :vec2) &uniform (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (* 10 st))
         ;;; variation 1 - camera/movement/zoom
         ;; (st (* (- st (v2! 5.0))
         ;;        (* 5 (abs (sin (* time .2))))))
         ;; (st (v! (+ (x st) (* 3 time)) (y st)))
         ;;; end variation 1
         (ipos (floor st)) ;; integer
         (fpos (fract st)) ;; fraction
         (tile (truchet-pattern fpos (g-random fpos)))
         ;;; Maze
         (color (- (smoothstep (- (x tile) .3)
                               (x tile)
                               (y tile))
                   (smoothstep (x tile)
                               (+ (x tile) .3)
                               (y tile))))
         ;;; Circles
         ;; (color (+ (- (step (length tile) .6) (step (length tile) .4))
         ;;           (- (step (length (- tile (v2! 1.))) .6)
         ;;              (step (length (- tile (v2! 1.))) .4))))
         ;;; Truchet (2 triangles)
;;         (color (step (x tile) (y tile)))
         )
    (v! (v3! color) 1.0)))
