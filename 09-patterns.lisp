(in-package :the-book-of-shaders)
;; WIP

;; Patterns
;; fract() — compute the fractional part of the argument
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (st    (* 3 st))
         (color (fract st)))
    (v! color 0 0)))

;; Now we have 3 spaces that goes from 0-1
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (st    (fract (* 3 st)))
         (color (v3! (circle st .5))))
    color))

;; Try some of the following exercises to get a deeper understanding:
;; - Multiply the space by different numbers. Try with floating point values and also with different values for x and y.
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (st    (fract (* (v! 1 2.5) st)))
         (color (v3! (circle st .2))))
    color))

;; - Make a reusable function of this tiling trick.
(defun-g circlify ((st :vec2) (grid :vec2))
  (let ((st (fract (* grid st))))
    (v3! (circle st .2))))

(defun-g circlify ((st :vec2) (grid :vec2) (color :vec3))
  (let ((st (fract (* grid st))))
    (* color (v3! (circle st .2)))))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (color (+ (circlify st
                             (v! 2 1))
                   (circlify (* 2 (+ (* .1 time) st))
                             (v! 4 2)
                             (v! 0 1 0)))))
    (v! color 0)))

;; - Divide the space into 3 rows and 3 columns.
;;   Find a way to know in which column and row the thread is
;;   and use that to change the shape that is displaying.
;;   Try to compose a tic-tac-toe match.
(defun-g tilify ((st :vec2) (grid :vec2))
  (fract (* grid st)))

;; TODO: helpers for each grid cell, return boolean
(defun-g tilify ((st :vec2) (grid :vec2))
  (if (and (< (x st) (* 2 (/ (x grid))))
           (> (x st) (/ (x grid)))
           (> (y st) (* 2 (/ (y grid)))))
      (v2! (circle (fract (* grid st)) .3))
      (fract (* grid st))))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (st    (tilify st (v! 3 3)))
         (color (v! st 1f0)))
    color))

;;--------------------------------------------------
;; Apply matrices inside patterns

;; Test box
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (st    (fract (* 3 st)))
         (color (v3! (box st (v! .5 .5) .5))))
    color))

;; using rotate2d
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st  (/ (s~ gl-frag-coord :xy)
                   resolution))
         (st  (tile st 4))
         (st  (rotate-2d st (* 3.1415 .25)))
         (color (v3! (box st (v2! .7) .1))))
    color))

;; - Think of interesting ways of animating this pattern.
;;   Consider animating color, shapes and motion.
;;   Make three different animations.

;; - Recreate more complicated patterns by composing different shapes.
;; - Combine different layers of patterns to compose your own Scottish Tartan Patterns.
