(defgeneric width (thing))
(defgeneric height (thing))
(defgeneric lerp (start end alpha))
(defgeneric snap (value))
(defgeneric scale (factor value))
(defgeneric color-dist (one two))
(defgeneric distance (p1 p2))

;;; arrangements
(defgeneric space-width (thing))
(defgeneric space-height (thing))
(defgeneric draw-arrangement (arr)
  (:documentation "Returns an SDL surface containing the pictorial representation of `arr'."))
(defgeneric world-at (thing x y))


;;; Game
(defgeneric cast-spell (spell spot))
(defgeneric mate (slug1 slug2))
(defgeneric mutate (slug &optional tolerance))
(defgeneric take-turn (monster world))
(defgeneric find-home-font (world slug))
(defgeneric draw (object x y window))
(defgeneric move (object dx dy))
(defgeneric random-move (monster world))
(defgeneric walk-toward (monster spot world))
(defgeneric draw-slug-info (slug w h))
(defgeneric slug-info-string (slug))
