(in-package :the-book-of-shaders)
;; 02

(defun-g g-plot ((st :vec2) (pct :float))
  (- (smoothstep (- pct .02) pct (y st))
     (smoothstep pct (+ pct .02) (y st))))

;; 09

(defun-g circle ((st :vec2) (radius :float))
  (let ((l (- st .5)))
    (- 1 (smoothstep (- radius (* radius .01))
                     (+ radius (* radius .01))
                     (* 4 (dot l l))))))

(defun-g rotate-2d ((st :vec2) (angle :float))
  (let* ((st (- st .5))
         (st (* st (mat2 (cos angle) (- (sin angle))
                         (sin angle) (cos angle))))
         (st (+ st .5)))
    st))

(defun-g tile ((st :vec2) (zoom :float))
  (fract (* zoom st)))

(defun-g box
    ((st :vec2) (size :vec2) (smooth-edges :float))
  (let* ((size (- (v2! .5)
                  (* .5 size)))
         (aa   (v2! (* .5 smooth-edges)))
         (uv   (smoothstep size (+ size aa) st))
         (uv   (* uv (smoothstep size (+ size aa) (- (v2! 1f0) st)))))
    (* (x uv) (y uv))))

;; 10


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

;; Deterministic random
(defun-g g-rand ((x :float))
  (fract (* (sin x)
            100000f0)))


;; 13

(defun-g mod289 ((x :vec3))
  (- x (* (floor (* x (/ 1f0 289))) 289f0)))
(defun-g mod289 ((x :vec2))
  (- x (* (floor (* x (/ 1f0 289))) 289f0)))

(defun-g permute ((x :vec3))
  (mod289 (* x (+ 1f0 (* x 34f0)))))

(defun-g snoise ((v :vec2))
  ;; Precompute values for skewed triangular grid
  (let* ((C (v! 0.211324865405187 0.366025403784439
                 -0.577350269189626 0.024390243902439))
         ;; First corner (x0)
         (i  (floor (+ v (dot v (v2! (y C))))))
         (x0 (+ (- v i)  (dot i (v2! (x C)))))
         ;; Other two corners x1, x2
         (i1 (if (> (x x0) (y x0)) (v! 1f0 0f0) (v! 0f0 1f0)))
         (x1 (- (+ x0 (v2! (x C))) i1))
         (x2 (+ x0 (v2! (z C))))
         ;; do some permutations to avoid
         ;; truncation effects in permutation
         (i (mod289 i))
         (p (permute (+ (x i) (v! 0f0 (x i1) 1f0)
                        (permute (+ (y i) (v! 0f0 (y i1) 1f0))))))
         (m (max (- .5 (v! (dot x0 x0)
                           (dot x1 x1)
                           (dot x2 x2)))
                 0f0))
         (m (* m m))
         (m (* m m))
         (x (- (* 2f0 (fract (* p (v3! (w C)))))
               1f0))
         (h (- (abs x) .5))
         (ox (floor (+ x .5)))
         (a0 (- x ox))
         (m (* m (- 1.79284291400159
                    (* 0.85373472095314 (+ (* a0 a0)
                                           (* h h))))))
         (g (v! (+ (* (x a0) (x x0))
                   (* (x h) (y x0)))
                (+ (* (s~ a0 :yz) (v! (x x1) (x x2)))
                   (* (s~ h :yz)  (v! (y x1) (y x2)))))))
    (* 130f0 (dot m g))))
