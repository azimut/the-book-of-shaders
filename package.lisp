(uiop:define-package the-book-of-shaders
    (:use #:cl #:cepl #:rtg-math #:vari #:nineveh
          :livesupport)
  (:export #:g-plot
           #:g-random
           #:g-rand
           #:mod289
           #:permute
           #:snoise))
