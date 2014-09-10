(defclass rectangular-arrangement ()
  ((items :accessor items
          :initarg :items
          :type 'list)
   (dimensions :accessor dimensions
               :initarg :dimensions
               :type '(vector number 2))
   (spacing :accessor spacing
            :initarg :spacing
            :type '(vector number 2))))

(defmethod width ((arr rectangular-arrangement))
  (aref (dimensions arr) 0))
(defmethod height ((arr rectangular-arrangement))
  (aref (dimensions arr) 1))
(defmethod space-width ((arr rectangular-arrangement))
  (aref (spacing arr) 0))
(defmethod space-height ((arr rectangular-arrangement))
  (aref (spacing arr) 1))

(defgeneric draw-arrangement (arr)
  (:documentation "Returns an SDL surface containing the pictorial representation of `arr'."))

(defmethod draw-arrangement ((arr rectangular-arrangement))
  (declare (special *tile-size*))
  (let* ((w (width arr))
         (h (height arr))
         (dx (space-width arr))
         (dy (space-height arr))
         (arr-surface (sdl:create-surface w h)))
    (loop for i from 0 upto (* w h) for item in (items arr) do
         (multiple-value-bind (x y) (mod* i (/ w *tile-size*))
           (draw item x y arr-surface)))
    arr-surface))
