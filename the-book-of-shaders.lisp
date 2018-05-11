(in-package :the-book-of-shaders)

(defun-g frag ((uv :vec2)
               &uniform (resolution :vec2)
               (time :float))
  (v! 0 0 0 1))

(defpipeline-g pipe (:points)
  :fragment (frag :vec2))

(defparameter *bs*
  (make-buffer-stream nil :primitive :points))

(defun draw! ()
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport))
          res)
    (as-frame
      (map-g #'pipe *bs*
             :resolution res
             :time (/ (get-internal-real-time)
                      1500f0)))))

(def-simple-main-loop play ()
  (draw!))
