(in-package :the-book-of-shaders)

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (left   (step .1 (x st)))
         (bottom (step .1 (y st)))
         ;; * is like a logical AND
         (color (v3! (* left bottom))))
    color))

;; both sides in one step() call - unclear pct on tbos
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         ;; bottom left
         (bl (step (v2! .1) st))
         (pct (* (x bl) (y bl)))
         ;; top right
         (tr (step (v2! .1) (- 1 st)))
         (pct (* pct (x tr) (y tr)))
         (color (v3! pct)))
    color))

;; Before going forward, try the following exercises:

;; * Change the size and proportions of the rectangle.
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (bt (* (step .3 (y st)) ;; B
                (step .5 (x st)) ;; L
                (step .4 (- 1 (y st)))   ;; T
                (step .2 (- 1 (x st))))) ;;R
         (color (v3!  bt)))
    color))w

;; * Experiment with the same code but using [`smoothstep()`](../glossary/?search=smoothstep) instead of [`step()`](../glossary/?search=step). Note that by changing values, you can go from blurred edges to elegant smooth borders.
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (bt (* (smoothstep .3 .4 (y st)) ;; B
                (smoothstep .5 .6 (x st)) ;; L
                (smoothstep .4 .5 (- 1 (y st)))   ;; T
                (smoothstep .2 .2 (- 1 (x st))))) ;;R
         (color (v3!  bt)))
    color))

;; * Do another implementation that uses [`floor()`](../glossary/?search=floor).
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (color  (* (floor (* 4 (x st)))
                    (floor (* 2 (y st)))
                    (floor (* 2 (- 1 (x st))))
                    (floor (* 3 (- 1 (y st)))))))
    (v3! color)))

;; * Choose the implementation you like the most and make a function of it that you can reuse in the future. Make your function flexible and efficient.

(defun-g user-box ((st :vec2) (dimensions :vec4))
  (* (step (x dimensions) (y st))
     (step (y dimensions) (x st))
     (step (z dimensions) (- 1 (y st)))
     (step (w dimensions) (- 1 (x st)))))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (bt (user-box st (v! .1 .0 .1 .0)))
         (color (v3! bt)))
    color))w

;; * Make another function that just draws the outline of a rectangle.
(defun-g user-outbox ((st :vec2) (dimensions :vec4) (border :vec4))
  (let ((xd (x dimensions))
        (yd (y dimensions))
        (zd (z dimensions))
        (wd (w dimensions))
        (xs (x st))
        (ys (y st)))
    (* (* (step xd ys)
          (step yd xs)
          (step zd (- 1 xs))
          (step wd (- 1 ys)))
       (- 1 (* (step (+ xd (x border)) ys)
               (step (+ yd (y border)) xs)
               (step (+ zd (z border)) (- 1 xs))
               (step (+ wd (w border)) (- 1 ys)))))))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (bt (user-outbox st (v! .2 .2 .2 .2) (v! .01 .01 .01 .01)))
         (color (v3!  bt)))
    color))

;; * How do you think you can move and place different rectangles in the same billboard? If you figure out how, show off your skills by making a composition of rectangles and colors that resembles a [Piet Mondrian](http://en.wikipedia.org/wiki/Piet_Mondrian) painting.
(defun-g user-box-color ((st :vec2) (dimensions :vec4) (color :vec3))
  (* color (* (step (x dimensions) (y st))
              (step (y dimensions) (x st))
              (step (z dimensions) (- 1 (y st)))
              (step (w dimensions) (- 1 (x st))))))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (bt (- (user-box-color st (v! .2 .2 .2 .2) (v! .5 .5 .5))
                (user-box-color st (v! .3 .3 .3 .3) (v! .2 .5 .9))))
         (color bt))
    color))

;;--------------------------------------------------
;; Circles
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st  (/ (s~ gl-frag-coord :xy) resolution))
         ;; 1 - DISTANCE
         ;;(pct (distance st (v2! .5)))
         ;; 2 - LENGTH
         ;;(pct (length (- (v2! .5) st)))
         ;; 3 - SQRT
         (tc  (- (v2! .5) st))
         (pct (sqrt
               (+ (pow (x tc) (* 5 (abs (cos time))))
                  (pow (y tc) (* 7 (abs (sin time)))))))
         (pct (* 10 pct ))
         (color (v3! pct))
         (color (+ color (v! .2 .7 .9))))
    color))

;; Circle
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st  (/ (s~ gl-frag-coord :xy) resolution))
         (mfact (+ .5 (* .4 (abs (cos (* 4 time))))))         
         (time (* time 3))
         (pct  (- 1 (smoothstep
                     (* mfact .1)
                     (* mfact .14)
                     (distance
                      st
                      (v2! (+ .5 (* .3 (sin time)))
                           (+ .5 (* .2 (cos time)))
                           )))))
         (color (v3! pct)))
    color))

;;--------------------------------------------------
;; For your toolbox
;;
;; dot() is more performant than the sqrt() related f()
(defun-g user-circle ((st :vec2) (radius :float))
  (let ((dist (- st .5)))
    (- 1 (smoothstep (- radius (* .1 radius))
                     (+ radius (* .01 radius))
                     (* 4 (dot dist dist))))))

(defun-g user-circle ((st :vec2)
                      (radius :float)
                      (center :vec2)
                      (color :vec3)
                      )
  (let ((dist (- st center)))
    (* color
       (smoothstep (- radius (* .3 radius))
                   (+ radius (* .9 radius))
                   (* 4 (dot dist dist))))))

(defun-g frag ((uv :vec2)
               &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (pct
          (+ (user-circle
              st
              (+ .04 (* .04 (abs (sin time))))
              (v! (+ .5 (* .4 (cos time)))
                  (+ .5 (* .2 (sin time))))
              (v! .2 .7 1))
             (user-circle
              st
              (+ .04 (* .04 (abs (sin time))))
              (v! (- .5 (* .4 (sin time)))
                  (+ .5 (* .2 (cos time))))
              (v! 1 .2 .7)))
           )
         (color pct)
;;         (color (* color (v! .2 .7 1)))
         )
    (v! color 1)))

;; (v! .2 .7 1) blueish

;;--------------------------------------------------
;; Useful properties of a Distance Field

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         ;; ?????
         ;; (st (v! (* (x st)
         ;;            (/ (x resolution) (y resolution)))
         ;;         (y st)))
         ;; remap the space to -1,1
         (zoo 2)
        (time (* .5 time))
        (zoo  (* 5 (+ .5 (* .5 (cos time)))))
        (st   (- (* zoo st) (/ zoo 2)))
         ;; make the sdf
         (d  (length (- (abs st) .5)))
         ;; 1)
         ;;(d (length (min (- (abs st) .3) 0)))
         ;; 2)
         ;;(d (length (max (- (abs st) .3) 0)))
         ;;(color (v3! (fract (* 10 d))))
         ;; 1)
         (color (v3! (step .3 d)))
         ;; 2)
         ;;(color (v3! (* (step .3 d) (step d .4))))
         ;; 3)
         ;;(color (v3! (* (smoothstep .3 .4 d)
         ;;               (smoothstep .6 .5 d))))
         )
    (v! color 1)))

;;--------------------------------------------------
;; Polar shapes

(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
           ;; (cos (* 3 x))
           ;; (abs (cos (* 3 x)))
           ;; (+ .3 (* .5 (abs (cos (* 2.5 x)))))
           (+ .1 (* .8 (abs (* (cos (* 12 x))
                               (sin (* 3 x))))))
           ;;(+ .5 (* .2 (smoothstep -.5 1 (cos (* 10 x)))))
           )
         uv
         (v! -3 3 -3 3)))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (pos (- st .5))
         (r (* 2 (length pos)))
         (a (atan (y pos) (x pos)))
         ;;(f (cos (* a 3)))
         ;; 1)
         ;;(f (abs (cos (* a 3))))
         ;; 2)
         ;;(f (+ .3 (* .5 (abs (cos (* a 2.5))))))
         ;; 3)
         ;; (f (+ .1 (* .8 (abs (* (cos (* a 12))
         ;;                        (sin (* a 3)))))))
         ;; 4)
         (f (+ .5 (* .2 (smoothstep -.5 1 (cos (* a 10))))))
         (color (v3! (- 1 (smoothstep f (+ f .02) r)))))
    color))


;; Try to:
;; - Animate these shapes.
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/  (s~ gl-frag-coord :xy) resolution))
         (pos (- st .5))
         (r (* 2 (length pos)))
         (a (atan (y pos) (x pos)))
         (f (+ .5 (* .2 (smoothstep -.5 1 (cos (* a 10))))))
         (color (v3! (- 1 (smoothstep f (+ f .02) r)))))
    color))

;; - Combine different shaping functions to *cut holes* in the shape to make flowers, snowflakes and gears.
;; - Use the `plot()` function we were using in the *Shaping Functions Chapter* to draw just the contour.
