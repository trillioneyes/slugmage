
(asdf:defsystem #:slugmage
  :name "slugmage"
  :serial t
  :depends-on (:lispbuilder-sdl)
  :components ((:file "generics")
               (:file "arrangement")
               (:file "game")))
