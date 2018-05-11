(asdf:defsystem "the-book-of-shaders"
  :description "cepl/varjo translation of the book of shaders"
  :author "azimut <azimut.github@protonmail.com>"
  :license "BSD-3"
  :serial t
  :depends-on (
               #:swank
               #:cepl.sdl2
               #:livesupport
               #:skitter
               #:cepl.skitter.sdl2
               #:rtg-math
               #:rtg-math.vari
               #:dendrite
               #:nineveh)
  :components ((:file "package")
               (:file "lib/helpers")))
