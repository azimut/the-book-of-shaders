(in-package :the-book-of-shaders)

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((yellow  (v! 1 1 0))
         (magenta (s~ yellow :xzy))
         (green   (s~ yellow :zyz))
         (color green)) ;; swap!
    color))

;;--------------------------------------------------
;; Mixing color
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((a (v! .149 .141 .912))
         (b (v!    1 .833 .224))
         (pct (abs (sin time)))
;;         (color (v3! pct))
         (color (mix a b pct)))
    (v! color 1)))

;; - Make an expressive transition between colors.
;; Think of a particular emotion.
;; What color seems most representative of it?
;; How does it appear? How does it fade away?
;; Think of another emotion and the matching color for it.
;; Change the beginning and ending color of the above code to match those emotions.
;; Then animate the transition using shaping functions.
;; Robert Penner developed a series of popular shaping functions
;; for computer animation known as easing functions,
;; you can use this example as research and inspiration
;; but the best result will come from making your own transitions.

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((a (v! 1 0 0))
         (b (v! 0 0 0))
         (pct   (mod (abs time) 1))
         (color (mix a b pct)))
    (v! color 1)))

;;--------------------------------------------------
;; Playing with gradients

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (a (v! .149 .141 .912))
         (b (v!    1 .933 .224))
         ;; basic gradient
         (pct (v3! (x st)))
         ;; advanced gradient
         (pct (v! (smoothstep 0 1 (x st))
                  (sin (* 3.1415 (x st)))
                  (pow (x st) .5)))
         (color (mix a b pct))
         ;; line
         (color (mix color (v! 1 0 0)
                     (g-plot st (x pct))))
         (color (mix color (v! 0 1 0)
                     (g-plot st (y pct))))
         (color (mix color (v! 0 0 1)
                     (g-plot st (z pct))))
         )
    color))

;; - Compose a gradient that resembles a
;;   William Turner sunset
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (a (v! .051 .157 .271))
         (b (v! .767 .578 .539))
         ;;         (pct (x st))
;;         (pct (fract (x st)))
         ;; extra - bars
         ;;(pct (* .1 (ceil (* 10 (x st)))))         
         (pct (- (pow (+ -1.1 (y st)) 6)
                 (pow (y st) 3)))
         (color (mix a b pct)))
    color))

;; - Animate a transition between a sunrise and sunset using u_time.
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (a (v! .051 .157 .271))
         (b (v! .767 .578 .539))
         (c (+ (* 2 (cos time))
               (y st)))
         (color (mix b a c)))
    color))

;; - Can you make a rainbow using what we have learned so far?
;; TODO: ... close enough
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (a (v! 1 1 1))
         (b (v! 0 0 0))
         (pct (v3! (tan (x st))
                   (sin (* 3.1415 2 (x st)))
                   (cos (* 3.1415 2 (x st)))))
         (color (mix a b pct)))
    (v! color 0)))

;; - Use the step() function to create a colorful flag.
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (a (v! 1 1 1))
         (b (v! 0 0 0))
         (pct (v3! (step (y st) .33)
                   (step (y st) .66)
                   .9))
         (color (mix a b pct)))
    (v! color 0)))

;;--------------------------------------------------
;; HSB

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (hsb2rgb (v! (x st) 1 (y st)))))
    (v! color 0)))
