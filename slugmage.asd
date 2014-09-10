
(asdf:defsystem #:slugmage
  :name "slugmage"
  :serial t
  :depends-on (:lispbuilder-sdl)
  :components ((:file "arrangement")
               (:file "game")))
