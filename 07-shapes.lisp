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
    color))

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


;; * Choose the implementation you like the most and make a function of it that you can reuse in the future. Make your function flexible and efficient.
;; * Make another function that just draws the outline of a rectangle.
;; * How do you think you can move and place different rectangles in the same billboard? If you figure out how, show off your skills by making a composition of rectangles and colors that resembles a [Piet Mondrian](http://en.wikipedia.org/wiki/Piet_Mondrian) painting.
