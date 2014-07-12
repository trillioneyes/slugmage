(eval-when (:compile-toplevel)
   (ql:quickload :lispbuilder-sdl))

;;; Initialized later in main
(defparameter *world* nil)
(defparameter *game* nil)

(defparameter sdl:*default-font* sdl:*font-8x8*)
(sdl:initialise-default-font)
(defparameter *tile-size* 10)

(sdl:enable-key-repeat 300 100)

(defparameter *hooks-registry* (make-hash-table)
  "The keys are game statuses (e.g. :playing and :end-turn). The values should be hashes from hook types ('key-up, 'key-down, 'mouse-up, 'mouse-down) to lists of functions.")

(defparameter *spells* (list :lightning :hand :fire :dawn))
(setf (get :lightning 'mana) 1
      (get :hand 'mana) 5
      (get :fire 'mana) 10
      (get :dawn 'mana) 100
      (get :lightning 'description) "Single target"
      (get :fire 'description) "Many targets"
      (get :hand 'description) "Far grab"
      (get :dawn 'description) "Win")

(defparameter *game-modes* (list :playing :end-turn :grab :menu :magic :drop
				 :cast :win))

(dolist (status *game-modes*)
  (setf (gethash status *hooks-registry*) (make-hash-table)))

(defparameter *mouse-button-down-hooks* nil
  "When a :mouse-button-down event is received, the hooks are called with ~
arguments (x y button)")
(defparameter *mouse-button-up-hooks* nil
  "When a :mouse-button-up event is received, the hooks are called with ~
arguments (x y button)")

(defparameter *key-down-hooks* nil
  "When a :key-down-event is received, the hooks are called with arguments ~
 (key)")
(defparameter *key-up-hooks* nil
  "When a :key-down-event is received, the hooks are called with arguments ~
 (key)")

(defparameter *directions* (list :sdl-key-a :sdl-key-s :sdl-key-d :sdl-key-w
				 :sdl-key-q :sdl-key-e :sdl-key-z :sdl-key-x
				 :sdl-key-c))

;;; Functions for dealing with randomness

;;; Return a random integer in the range (-n/2, n/2)
(defun random-delta (n) (- (random n) (floor (/ n 2))))
;;; Return t with probability 1/n; else return nil
(defun one-in (n) (= 0 (random n)))
;;; A probability function with a positive derivative
;;; A higher penalty decreases the probability for low values of x, but has
;;; vanishing impact for large x.
(defun prob (x &optional (penalty 10))
  (< (random (+ x penalty)) x))

(defun directional (key) (member key *directions*))
(setf (get :sdl-key-a 'direction) (list -1 0)
      (get :sdl-key-s 'direction) (list 0 0)
      (get :sdl-key-d 'direction) (list 1 0)
      (get :sdl-key-w 'direction) (list 0 -1)
      (get :sdl-key-q 'direction) (list -1 -1)
      (get :sdl-key-e 'direction) (list 1 -1)
      (get :sdl-key-z 'direction) (list -1 1)
      (get :sdl-key-x 'direction) (list 0 1)
      (get :sdl-key-c 'direction) (list 1 1))

(defmacro key-down-hook (&body body)
  `(push (lambda (key) ,@body) *key-down-hooks*))

(defun process-mouse-hooks (hook-list x y b) 
  (setf hook-list 
	(template-delete hook-list
			 (mapcar (lambda (hook) (funcall hook x y b))
				 hook-list))))

(defun process-key-hooks (hook-list k)
  (setf hook-list 
	(template-delete hook-list
			 (mapcar (lambda (hook) (funcall hook k))
				 hook-list))))

(defun template-delete (list template)
  (loop for l in list for ? in template when ? collect l))

;;; Sanitize this and check that the bindings are hook variables
(defmacro sdl-loop (hook-bindings &body body)
  (flet ((lambdaify (hook functions)
	   (cons 'list
		 (mapcar
		  (lambda (function)
		    `(lambda ,(case hook
				    ((*mouse-button-down-hooks*
				      *mouse-button-up-hooks*)
				     '(x y button))
				    ((*key-up-hooks*
				      *key-down-hooks*)
				     '(key)))
		       ,function t))	; return t for "don't remove me"
		  functions))))
    `(progn 
       ,@(loop for binds in hook-bindings collect
	      (destructuring-bind (mode . hooks) binds
		`(let (,@(mapcar (lambda (hook) 
				   `(,(car hook) 
				      ,(lambdaify (car hook) (cdr hook))))
				 hooks))
		   (save-hooks ,mode))))
       
       (load-hooks :playing)

       (sdl:with-events ()
	 (:idle () (sdl:update-display))
	 (:quit-event () t)
	 (:mouse-button-down-event (:x x :y y :button b)
	   (process-mouse-hooks *mouse-button-down-hooks* x y b))
	 (:mouse-button-up-event (:x x :y y :button b)
	   (process-mouse-hooks *mouse-button-up-hooks* x y b))
	 (:key-down-event (:key key)
	   (process-key-hooks *key-down-hooks* key))
	 (:key-up-event (:key key)
	   (process-key-hooks *key-up-hooks* key))
	 ,@body))))


(defclass game ()
  ((status :accessor status
	   :initform :playing)
   (player :reader player
	   :initarg :player)
   (world  :accessor world
	   :initarg :world)
   (selected-slug :accessor selected-slug
		  :initform nil)
   (ui-top :accessor ui-top
	   :initform 550)
   (ui-border :accessor ui-border
	      :initform 5)
   (active-spell :accessor active-spell
		 :initform nil)
   (active-animations :accessor active-animations
		      :initform nil)))

(defclass world ()
  ((player   :accessor player
	     :initarg :player
	     :initform nil)
   (monsters :accessor monsters
	     :initform nil
	     :initarg  :monsters)
   (walls    :accessor walls
	     :initarg :walls
	     :initform nil)))

(defclass animation ()
  ((frames :accessor frames
	   :initarg :frames
	   :documentation "Length of this animation in frames. Can be nil if the animation can last an arbitrary number of frames.")
   (turns :accessor turns
	  :initarg :turns
	  :documentation "Length of this animation in turns. Can be nil if the animation can last an arbitrary number of turns.")
   (draw-fn :accessor draw-fn
	    :initarg :draw-fn
	    :documentation "Called each frame with the following arguments: ~
            (age-turns age-frames target-surface player-offset)")
   (age-turns :accessor age-turns
	      :initform 0
	      :documentation "The number of turns this animation has been active.")
   (age-frames :accessor age-frames
	       :initform 0
	       :documentation "The number of frames this animation has been active.")))

(defun draw-animation (anim surface)
  (let ((player-offset
	 (list
	  (- (/ 400 *tile-size*) (caar (player (world *game*))))
	  (- (/ 300 *tile-size*) (cadar (player (world *game*)))))))
    (funcall (draw-fn anim) (age-turns anim) (age-frames anim) surface player-offset)))
(defun expired? (anim)
  (with-slots (frames age-frames turns age-turns) anim
   (or (and frames (>= age-frames frames))
       (and turns (>= age-turns turns)))))


(defgeneric find-target (world))
(defmethod find-target (world)
  (nth (random (1+ (length (monsters world)))) 
       (cons (player world) (monsters world))))

(defun save-hooks (status)
  (let ((hooks-registry (gethash status *hooks-registry*)))
    (setf (gethash 'mouse-up hooks-registry) *mouse-button-up-hooks*
	  (gethash 'mouse-down hooks-registry) *mouse-button-down-hooks*
	  (gethash 'key-up hooks-registry) *key-up-hooks*
	  (gethash 'key-down hooks-registry) *key-down-hooks*)))
(defun load-hooks (status)
  (let ((hooks-registry (gethash status *hooks-registry*)))
    (setf *mouse-button-up-hooks* (gethash 'mouse-up hooks-registry)
	  *mouse-button-down-hooks* (gethash 'mouse-down hooks-registry)
	  *key-up-hooks* (gethash 'key-up hooks-registry)
	  *key-down-hooks* (gethash 'key-down hooks-registry))))
(defmethod (setf status) :before (value (object game))
  (save-hooks (status object))
  (load-hooks value))

(defun register (world &rest monsters)
  (make-instance 'world :monsters (append monsters (monsters world))
		 :player (player world) :walls (walls world)))

(defclass monster ()
  ((mana :accessor mana
	 :initform 1
	 :initarg :mana)
   (weight :accessor weight
	   :initform 1
	   :initarg  :weight)
   (daughters :accessor daughters
	      :initarg :daughters
	      :initform nil)))

(defclass player (monster)
  ((inventory :accessor inventory
	      :initform nil)))

(defclass slug (monster)
  ((color :accessor color
	  :initarg :color
	  :initform nil)
   (home-font :accessor home-font
	      :initarg :home-font
	      :initform nil)
   (target :accessor target
	   :initform (list (random 60) (random 60)))
   (life :accessor life
	 :initform 50
	 :initarg :life)
   (threshold :accessor threshold
	      :initform (mutate 25 40)
	      :initarg :threshold)
   (docility :accessor docility
	     :initarg :docility
	     :initform (mutate 10 40))
   (pers :accessor pers
	 :initarg :pers
	 :initform (mutate 6 3)
	 :documentation "perseverance")
   (weight :initform (mutate 5 5))
   (mana :initform (mutate 1 5))))

(defmethod color :before ((slug slug))
  (if (null (slot-value slug 'color))
      (with-slots (color mana weight docility) slug
	(setf color (sdl:color :r (min 255 (max 0(- 255 (* 15 docility))))
			       :g (min 255 (max 0(* 15 weight)))
			       :b (min 255 (max 0 (* 15 mana))))))))


(defgeneric color-dist (one two))
(defmethod color-dist ((slug1 slug) (slug2 slug))
  (1+ (ceiling (color-dist (color slug1) (color slug2)))))
(defmethod color-dist ((color1 sdl:color) (color2 sdl:color))
  (multiple-value-bind (r1 g1 b1) (sdl:color-* color1)
    (multiple-value-bind (r2 g2 b2) (sdl:color-* color2)
      (sqrt (+ (expt (- r1 r2) 2) (expt (- g1 g2) 2) (expt (- b1 b2) 2))))))

(defclass slug-font (slug)
  ())


(defun neighbors (spot world &key (dist 1) (filter-type 'slug))
  (loop for dx from (- dist) upto dist
	for dy from (- dist) upto dist
	when (not (and (= dx 0) (= dy 0)))
	when (typep (item-at world (+ (car spot) dx) (+ (cadr spot) dy))
		    filter-type)
	collect (item-at world (+ (car spot) dx) (+ (cadr spot) dy))))
(defun population (&rest args)
  (length (apply 'neighbors args)))

(defun sanitize-daughters (cell)
  (destructuring-bind ((x y) . mother) cell
    (prog1 (mapcar (lambda (d) (cons (list x y) d))
		   (daughters mother))
      (setf (daughters mother) nil))))

(defun birth-all (world)
  (setf (monsters world) 
	(append (monsters world)
		(apply #'append (mapcar #'sanitize-daughters (monsters world)))
		(sanitize-daughters (player world)))))

(defun play-one-round (game)
  (when (eq (status game) :end-turn) 
    (take-turns (world game)) 
    (birth-all (world game))
    (setf (active-animations game)
	  (remove-if 'expired? (active-animations game)))
    (dolist (anim (active-animations game))
      (incf (age-turns anim)))
    (setf (status game) :playing)))

(defun distance (p1 p2)
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))))

(defgeneric cast-spell (spell spot game))

(defmethod cast-spell ((spell (eql :fire)) spot game)
  (declare (ignore spell)) 
  (dolist (monster (monsters (world game)))
    (when (< (distance (car monster) spot) 5)
      (kill monster (world game)))))

(defmethod cast-spell ((spell (eql :lightning)) spot game)
  (declare (ignore spell))
  (destructuring-bind (x y) spot
    (kill (world-at (world game) x y) (world game))))

(defmethod cast-spell ((spell (eql :hand)) spot game)
  (declare (ignore spell))
  (destructuring-bind (x y) spot
    (grab (world game) x y)))

(defmethod cast-spell ((spell (eql :dawn)) spot game)
  (declare (ignore spot spell))
  (setf (status game) :win))

(defun take-turns (world)
  ;; Using maplist so that we can modify the list in-place
  ;; We are only ever interested in (car cells), not cells itself
  (maplist (lambda (cells)
	     (if (car cells)
		 (setf (car cells)
		       (take-turn (cdar cells) (caar cells) world))))
	   (monsters world))
  (setf (monsters world)
	(remove-if-not #'cdr (monsters world))))


(defgeneric mate (slug1 slug2))
(defmethod mate ((slug1 slug) (slug2 slug))
  (with-slots (mana threshold pers) slug1
    (with-slots (weight docility (d threshold)) slug2
      (let ((tolerance (+ 2 (floor (/ 200 d)))))
	(make-instance 'slug :pers (mutate pers tolerance)
		       :mana (mutate mana tolerance)
		       :threshold (mutate threshold tolerance)
		       :weight (mutate weight tolerance)
		       :docility (mutate docility tolerance))))))

(defgeneric mutate (slug &optional tolerance))

(defmethod mutate ((slug slug) &optional tolerance)
  (declare (ignore tolerance))
  (with-slots (mana weight threshold docility pers) slug
    (make-instance 'slug :threshold (mutate threshold)
		   :docility (mutate docility)
		   :mana (mutate mana)
		   :docility (mutate docility)
		   :pers (mutate pers)
		   :weight (mutate weight))))

(defmethod mutate ((stat number) &optional (tolerance 20))
  (max 1 (+ stat (random-delta tolerance))))

(defun eat (slug1 slug2 world)
  (incf (mana slug1) (ceiling (/ (mana slug2) 2)))
  (incf (life slug1) (ceiling (/ (weight slug2) 2)))
  (kill slug2 world))

(defun fight (slug1 slug2 world)
  (flet ((power (slug) (max 1 (- (mana slug) (docility slug)))))
    (decf (life slug1) (power slug2))
    (decf (life slug2) (power slug1)))
  (cond ((< (life slug1) 1 (life slug2))
	 (eat slug2 slug1 world))
	((< (life slug2) 1 (life slug1))
	 (eat slug1 slug2 world))))


;;; Used in take-turn for slugs
;;; Should the given slug turn into a font?
(defun transform? (slug coords world)
  (with-slots (home-font target life threshold pers docility mana weight)
	      slug
    (or (and (< life pers)
	     (one-in (+ 100 (- mana)
			(population coords world :dist 5 :filter-type 'slug-font)))))))
(defun set-explore-target (monster coords world)
  (loop while (prob (population (target monster) world :dist 5) 2) do
    (destructuring-bind (x y) coords
      (setf (target monster)
	    (list (+ x (random-delta (* 15 (pers monster))))
		  (+ y (random-delta (* 15 (pers monster)))))))))

(defgeneric take-turn (monster coords world))

(defmethod take-turn ((monster slug-font) coords world)
  (if (one-in (+ 15 (population coords world)))
      (let ((spawn (mutate monster)))
	(setf (home-font spawn) monster)
	(push spawn (daughters monster))))
  (cons coords monster))

(defmethod take-turn ((monster slug) coords world)
  (with-slots (home-font target life threshold pers docility mana weight)
      monster
    (unless (member home-font (monsters world) :test #'equal)
      (setf home-font (find-home-font world monster)))
    (if (one-in pers)
	(if (prob (population coords world :dist 5) 2)
	    (set-explore-target monster coords world)
	    (setf target (car (find-target world)))))
    (if (and home-font (< life threshold)) (setf target (car home-font))
	(dolist (other (neighbors coords world))
	  (cond ((one-in (max 1 (- docility
				   (floor (/ (- mana (mana other)) 2))))) 
		 (fight monster other world))
		((one-in (ceiling (* (color-dist monster other) 15)))
		 (push (mate monster other) (daughters monster))))))
    (if (or (not home-font) (> (distance coords (car home-font)) 3.5)) 
	(decf life (ceiling (/ mana 5)))
	(incf life))
    (cond
     ((transform? monster coords world)
      (change-class monster 'slug-font)
      (cons coords monster))
     ((> life 0)
      (walk-toward (cons coords monster) target world))
					; nil return value says to delete this monster
     (t (push (make-death-anim (car coords) (cadr coords))
	 (active-animations *game*))
	(cons coords nil)))))


(defgeneric find-home-font (world slug))
(defmethod find-home-font ((world world) (slug slug))
  (let* ((fonts (remove-if-not (lambda (x) (typep x 'slug-font))
			       (mapcar #'cdr (monsters world))))
	 (scores (mapcar (lambda (x) (color-dist slug x)) fonts))
	 (best (if scores (apply #'min scores)))
	 (favorite-font (cdr (assoc best (pairlis scores fonts)))))
    (rassoc favorite-font (monsters world))))


(defgeneric draw (object x y window))

(defmethod draw ((object world) x0 y0 window)
  (dolist (cell (monsters object))
    (destructuring-bind ((x y) . monster) cell
      (draw monster (+ x0 x) (+ y0 y) window))) 
  (if (player object)
      (destructuring-bind ((x y) . player) (player object)
	(draw player (+ x0 x) (+ y0 y) window))))


(defmethod draw ((object player) x y window)
  (let ((radius (/ *tile-size* 2)))
    (sdl:draw-filled-circle-* (+ radius (* *tile-size* x))
			      (+ radius (* *tile-size* y))
			      radius :surface window :color sdl:*green*)))

(defmethod draw ((object slug) x y window)
  (let ((radius (/ *tile-size* 2)))
    (sdl:draw-filled-circle-*
     (+ radius (* *tile-size* x))
     (+ radius (* *tile-size* y))
     radius :surface window :color (color object))
    (sdl:draw-circle-* (+ radius (* *tile-size* x))
		       (+ radius (* *tile-size* y))
		       radius :surface window :color sdl:*white*)))

(defun draw-death-at (x y)
  "Given a pair of x and y coordinates, returns a function suitable for use as the draw-fn of an animation"
  (lambda (turns frames surface player-offset)
    (declare (ignore turns))
    (if (< 0 (mod frames 10) 5)
	(let* ((px (car player-offset))
	       (py (cadr player-offset))
	       (x1 (+ x -1/2 px))
	       (y1 (+ y -1/2 py))
	       (x2 (+ x 1/2 px))
	       (y2 (+ y 1/2 py)))
	  (sdl:draw-line-* (* x1 *tile-size*) (* y1 *tile-size*)
			   (* x2 *tile-size*) (* y2 *tile-size*)
			   :surface surface
			   :color sdl:*red*)
	  (sdl:draw-line-* (* x1 *tile-size*) (* y2 *tile-size*)
			   (* x2 *tile-size*) (* y1 *tile-size*)
			   :surface surface
			   :color sdl:*red*)))))
(defun make-death-anim (x y)
  (make-instance 'animation :draw-fn (draw-death-at x y) :turns 2 :frames 40))

(defmethod draw ((object slug-font) x y window)
  (sdl:draw-box-* (* *tile-size* x)
		  (* *tile-size* y)
		  *tile-size* *tile-size*
		  :surface window :color (color object))
  (sdl:draw-rectangle-* (* *tile-size* x)
			(* *tile-size* y)
			*tile-size* *tile-size*
			:surface window :color sdl:*white*))


(defmethod draw ((object (eql :fire)) x y window)
  (sdl:draw-box-* (* *tile-size* x)
		  (* *tile-size* y)
		  *tile-size* *tile-size*
		  :surface window :color sdl:*red*))
(defmethod draw ((object (eql :lightning)) x y window)
  (sdl:draw-box-* (* *tile-size* x)
		  (* *tile-size* y)
		  *tile-size* *tile-size*
		  :surface window :color sdl:*yellow*))
(defmethod draw ((object (eql :hand)) x y window)
  (sdl:draw-box-* (* *tile-size* x)
		  (* *tile-size* y)
		  *tile-size* *tile-size*
		  :surface window :color sdl:*green*))
(defmethod draw ((object (eql :dawn)) x y window)
  (sdl:draw-box-* (* *tile-size* x)
		  (* *tile-size* y)
		  *tile-size* *tile-size*
		  :surface window :color sdl:*white*))


(defgeneric move (object dx dy))
(defmethod move ((object list) dx dy)
  (destructuring-bind ((x y) . monster) object
    (list* (list (+ x dx) (+ y dy)) monster)))
(defmethod move :before ((object list) dx dy)
  (if (typep (cdr object) 'player) (setf (status *game*) :end-turn)))


(defgeneric random-move (monster world coord))
(defmethod random-move ((monster slug) world coord)
  (flet ((random-choice (list)
	   (nth (random (length list)) list)))
    (let ((directions (list (list 1 0) (list -1 0) (list 0 1) (list 0 -1))))
     (destructuring-bind (dx dy)
	 (random-choice
	  (template-delete
	   (cons (list 0 0) directions)
	   (cons t
		 (mapcar (lambda (dir)
			   (destructuring-bind ((x y) dx dy) (cons coord dir)
			     (not (world-at world (+ x dx) (+ y dy)))))
			 directions))))
       (move (cons coord monster) dx dy)))))

(defmethod random-move ((cell list) world coord)
  (random-move (cdr cell) world coord))

(defgeneric walk-toward (monster spot world))
(defmethod walk-toward ((monster list) spot world)
  (destructuring-bind ((x y) . _) monster
    (declare (ignore _))
    (destructuring-bind (dx dy)
	(list (cond ((< x (car spot))  1)
		    ((> x (car spot)) -1)
		    (t                 0))
	      (cond ((< y (cadr spot))  1)
		    ((> y (cadr spot)) -1)
		    (t                  0)))
      (if (world-at world (+ x dx) (+ y dy))
	  (random-move monster world (list x y))
	  (move monster dx dy)))))



(defgeneric world-at (world x y))
(defmethod world-at (world x y)
  (if (player world)
      (destructuring-bind ((x1 y1) . player) (player world)
	(declare (ignore player))
	(if (and (= x1 x) (= y1 y))
	    (player world)
	    (assoc (list x y) (monsters world) :test #'equal)))
      (assoc (list x y) (monsters world) :test #'equal)))

(defgeneric item-at (world x y))
(defmethod item-at (world x y)
  (cdr (world-at world x y)))


(defun kill (monster world)
  (let ((coords (car (rassoc monster (monsters world)))))
   (setf (monsters world)
	 (delete monster (monsters world)
		 :test (lambda (monster cell) (equal (cdr cell) monster))))
   (push (make-death-anim (car coords) (cadr coords))
	 (active-animations *game*))))

(defun grab (world x y)
  (let ((cell (world-at world x y))
	(item (item-at world x y)))
    (when (and item (< (+ (inv-weight (cdr (player world)))
			  (weight item))
		       (weight (cdr (player world)))))
      (push item (inventory (cdr (player world))))
      (kill cell world))))

(defun inv-weight (player)
  (reduce #'+ (mapcar #'weight (inventory player))))


(defun drop (obj world)
  (push obj (daughters (cdr (player world))))
  (setf (inventory (cdr (player world))) 
	(delete obj (inventory (cdr (player world))))))



(defun draw-game (game window) 
  (draw (world game) 
	(- (/ 400 *tile-size*) (caar (player (world game))))
	(- (/ 300 *tile-size*) (cadar (player (world game)))) 
	window)
  (dolist (anim (active-animations game))
    (unless (expired? anim) (draw-animation anim window))
    (incf (age-frames anim))))

(defun mod* (num divisor)
  (let ((x (mod num divisor)))
    (values x (floor (/ num divisor)))))

(defgeneric draw-slug-info (slug window x y w h))
(defmethod draw-slug-info ((slug slug) window x y w h)
  (let ((surface (sdl:create-surface w h))
	(height (sdl:char-height sdl:*default-font*))
	(width (sdl:char-width sdl:*default-font*))
	(sdl:*default-color* (if (< (color-dist (color slug) sdl:*white*) 300)
				 sdl:*black*
				 sdl:*white*)))
    (labels ((att-name (att)
	       (concatenate 
		'string
		(case att
		  (pers "PERS")
		  (docility "TAME")
		  (mana "MANA")
		  (weight "WGHT")
		  (t (subseq (symbol-name att) 0 4)))
		":~2d")) 
	     (print-att (att column row)
	       (sdl:draw-string-solid-* 
		(format nil (att-name att) (funcall att slug))
		(* column (* 7 (+ 1 width))) (+ 2 (* row (+ 1 height))) 
		:surface surface)))
      (sdl:clear-display (color slug) :surface surface)
      (sdl:draw-string-solid-*
       (if (typep slug 'slug-font) "Font" "Slug")
       0 0 :surface surface)
      (print-att 'mana 0 1)
      (print-att 'docility 0 2)
      (print-att 'pers 0 3)
      (print-att 'weight 1 1)
      (print-att 'life 1 2)
      (print-att 'threshold 1 3)
      (sdl:draw-surface-at-* surface x y :surface window))))

(defun draw-spell-info (spell window x y w h)
  (if spell
      (let ((surface (sdl:create-surface w h))
	    (height (sdl:char-height sdl:*default-font*)))
	(sdl:draw-string-solid-* (format nil "~a:~3d" spell (get spell 'mana))
				 0 0 :surface surface)
	(sdl:draw-string-solid-* (get spell 'description) 0 height
				 :surface surface)
	(sdl:draw-surface-at-* surface x y :surface window))))

(defun inventory-world (list w h)
  (if list
      (apply #'register (make-instance 'world)
	     (loop for i upto (* w h) for slug in list collect
		  (cons (multiple-value-list (mod* i (/ w *tile-size*)))
			slug)))
      (make-instance 'world)))

 (defun draw-inventory (game window x y w h)
  (let ((inventory-surface (sdl:create-surface w h))
	(inventory (inventory-world 
		    (reverse (inventory (cdr (player (world game)))))
				    w h)))
    (draw inventory 0 0 inventory-surface)
    (sdl:draw-surface-at-* inventory-surface x y :surface window)))

(defun draw-spells (game window x y w h)
  (let ((surface (sdl:create-surface w (+ 19 h))))
    (sdl:draw-string-solid-* (format nil "M:~3d"
				     (mana (cdr (player (world game)))))
			     0 (+ 1 h) :surface surface)
    (sdl:draw-string-solid-* (format nil "W:~3d"
				     (inv-weight (cdr (player (world game)))))
			   0 (+ 1 h h) :surface surface)
    (draw (inventory-world *spells* 40 10) 0 0 surface)
    (sdl:draw-surface-at-* surface x y :surface window)))

(defun draw-ui (game window)
  (let ((top (ui-top game))
	(border (ui-border game)))
    (sdl:draw-box-* 0 top 800 80 :surface window
		    :color (sdl:color :r 25 :g 25 :b 255))

    (draw-inventory game window border (+ top border) 80 40)
    
    (draw-spells game window (+ border 80 border) (+ top border)
		 40 10)

    (typecase (selected-slug game)
      (slug (draw-slug-info (selected-slug game) window
		       (+ border 80 border 40 border)
		       (+ top border) 200 40))
      (symbol (draw-spell-info (selected-slug game) window
			       (+ border 80 border 40 border)
			       (+ top border) 200 40)))))


(defun at-cursor (world x y x0 y0)
  (let ((x (+ (floor (/ x *tile-size*)) x0))
	(y (+ (floor (/ y *tile-size*)) y0))) 
    (item-at world x y)))

(defun inv-at-cursor (game x y)
  (at-cursor (inventory-world 
	      (reverse (inventory (cdr (player (world game))))) 80 40)
	     (- x (ui-border game))
	     (- y (ui-border game) (ui-top game))
	     0 0))

(defun spell-at-cursor (game x y)
  (at-cursor (inventory-world *spells* 40 10)
	     (- x (ui-border game) 80 (ui-border game))
	     (- y (ui-border game) (ui-top game))
	     0 0))

(defun eat-slug (game x y)
  (let ((slug (inv-at-cursor game x y)))
    (when slug
      (incf (mana (cdr (player (world game)))) (mana slug))
      (setf (inventory (cdr (player (world game))))
	    (delete slug (inventory (cdr (player (world game)))))))))

(defun win-message (x y window)
  (let ((h (+ 1 (sdl:char-height sdl:*default-font*))))
    (sdl:with-surface (window)
      (sdl:draw-string-solid-* "YOU WIN! :D" x y)
      (sdl:draw-string-solid-* "Press escape to return to game" x (+ y h))
      (sdl:draw-string-solid-* "or close the window to quit." x (+ y h h)))))

(defun game-loop (game)
  (sdl-loop
      ((:playing (*key-down-hooks*
		  (if (eq key :sdl-key-escape) (setf (status game) :end-turn))
		  (when (directional key)
		    (setf (player (world game))
			  (destructuring-bind (x y) (get key 'direction)
			    (move (player (world game)) x y))))
		  (when (eq key :sdl-key-g)
		    (setf (status game) :grab)
		    (key-down-hook
		      (when (directional key) (let ((key* key))
			 (destructuring-bind (x y) (car (player (world game))) 
			   (destructuring-bind (x1 y1) (get key* 'direction)
			     (grab (world game) (+ x x1) (+ y y1))))))
		      (setf *key-down-hooks* nil)
		      (setf (status game) :end-turn)))
		  (when (eq key :sdl-key-t) (setf (status game) :drop))
		  (when (eq key :sdl-key-m) (setf (status game) :magic))))
       (:drop (*mouse-button-down-hooks*
	       (when (= button 1)
		 (let ((thing (inv-at-cursor game x y)))
		   (when thing
		     (drop thing (world game))
		     (setf (status game) :end-turn))))))
       (:magic (*mouse-button-down-hooks*
		(when (= button 1)
		  (if (< x (+ (* (ui-border game) 2) 80))
		      (eat-slug game x y)
		      (let ((spell (spell-at-cursor game x y)))
			(when (and spell (<= (get spell 'mana)
					     (mana (cdr (player (world game))))))
			  (setf (active-spell game) spell
				(status game) :cast)))))))
       (:cast (*mouse-button-down-hooks*
	       (if button 
		   (cast-spell (active-spell game)
			       (destructuring-bind (x0 y0) 
				   (car (player (world game)))
				 (list (- (floor (/ x *tile-size*)) 40 (- x0))
				       (- (floor (/ y *tile-size*)) 30 (- y0))))
			       game))
	       (progn x y button 
		      (unless (eq (active-spell game) :dawn)
			(setf (status game) :end-turn))))))
    (:idle () 
	   (sdl:clear-display sdl:*black*) ; later make it clear with the map
	   (if (eq (status game) :win)
	       (win-message 280 100 sdl:*default-display*)
	       (progn (draw-game game sdl:*default-display*)
		      (draw-ui game sdl:*default-display*)
		      (play-one-round game)))
	   (if (eq (status game) :playing) 
	       (setf (mana (cdr (player (world game))))
		     0)))
    (:mouse-motion-event (:x x :y y)
      (setf (selected-slug game) 
	    (cond ((< y (ui-top game))
		   (destructuring-bind (x0 y0) (car (player (world game)))
		     (at-cursor (world game) x y (- x0 40) (- y0 30))))
		  ((< x (+ (ui-border game) 80))
		   (inv-at-cursor game x y))
		  (t (spell-at-cursor game x y)))))))

(defun main ()
  (setf *random-state* (make-random-state t))
  (setf *world*
	(register (make-instance 'world :player `((8 12) .
						  ,(make-instance 'player
								  :weight 30))) 
		  (list* '(-5 -5) (make-instance 'slug-font))
		  (list* '(30 25) (make-instance 'slug-font))
		  (list* '(-13 29) (make-instance 'slug-font))))
  (setf *game* (make-instance 'game :world *world*))
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "Cannibal Slugmage of Eden")
    (game-loop *game*)))
