(in-package :the-book-of-shaders)

;; Using nineveh (graph) function
(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
           (let ((amplitude 1f0)
                 (frequency 1f0))
             (* amplitude (sin (* frequency x)))))
         uv
         (v! -10 10 -5 5)))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (graph (lambda ((x :float))
           (let ((amplitude 2f0)
                 (frequency 2f0)
                 (tt (* .01 (* 130f0 (* -1 time)))))
             (* amplitude .06
                (+ (* (sin (* frequency x)))
                   (* 4.5 (sin (+ tt (* frequency x 2.1))))
                   (* 4.0 (sin (+ (* tt 1.121)
                                  (* frequency x 1.72))))
                   (* 5.0 (sin (+ (* tt .437)
                                  (* frequency x 2.221))))
                   (* 2.5 (sin (+ (* tt 4.269)
                                  (* frequency x 3.1122))))))))
         uv
         (v! -2 2 -5 5)))

;; Experiment by changing the frequency and amplitude for the additional waves.
;; Is it possible to make two waves cancel each other out? What will that look like?
;; Is it possible to add waves in such a way that they will amplify each other?

(defun-g g-random ((st :vec2))
  (fract (* 43758.5453123
            (sin (dot st
                      (v2! 12.9898 78.233))))))

(defun-g noise ((st :vec2))
  (let* ((i (floor st))
         (f (fract st))
         (a (g-random i))
         (b (g-random (+ i (v2! 1.0 0.0))))
         (c (g-random (+ i (v2! 0.0 1.0))))
         (d (g-random (+ i (v2! 1.0 1.0))))
         (u (* f f (- 3.0 (* 2.0 f)))))
    (+ (mix a b (x u))
       (* (- c a) (y u) (- 1.0 (x u)))
       (* (- d b) (x u) (y u)))))


(defun-g fbm ((st :vec2))
  (let* ((value 0f0)
         (amplitude .5)
         (frequency 0f0)
         (octaves 6))
    (for (i 0) (< i octaves) (++ i)
         (incf value (* amplitude (noise st)))
         (multf st (v2! 2f0))
         (multf amplitude .5))
    value))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2)
               (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (v! (* (x st) (/ (x resolution)
                              (y resolution)))
                 (y st)))
         (color (v3! 0f0))
         (color (+ color (fbm (* st 3f0)))))
    (v! color 1f0)))

;; Reduce the number of octaves (foor loop count)
;; Modify the lacunarity of the fBm 
;; Explore by changing the gain 

;;; http://www.iquilezles.org/www/articles/morenoise/morenoise.htm

(defun-g fbm ((st :vec2))
  (let* ((value 0f0)
         (amplitude .5)
         (frequency 0f0)
         (octaves 6))
    (for (i 0) (< i octaves) (++ i)
         (incf value (* amplitude (abs (snoise st))))
         (multf st (v2! 2f0))
         (multf amplitude .5))
    value))


;; http://www.iquilezles.org/www/articles/warp/warp.htm
