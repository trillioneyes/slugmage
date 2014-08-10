(eval-when (:compile-toplevel)
   (ql:quickload :lispbuilder-sdl))

;;; Initialized later in main
(defparameter *world* nil)
(defparameter *game* nil)

;;; SDL surfaces; initialized later in main
(defparameter *skull* nil)
(defparameter *bash* nil)
(defparameter *heart* nil)

(defparameter sdl:*default-font* sdl:*font-8x8*)
(sdl:initialise-default-font)
(defparameter *font-height* 8)
(defparameter *font-width* 8)
(defparameter *tile-size* 10)

(defparameter *hooks-registry* (make-hash-table)
  "The keys are game statuses (e.g. :playing and :end-turn). The values should be hashes from hook types ('key-up, 'key-down, 'mouse-up, 'mouse-down) to lists of functions.")

(defparameter *spells* '(:lightning :hand :fire :dawn))
(setf (get :lightning 'mana) 1
      (get :hand 'mana) 5
      (get :fire 'mana) 10
      (get :dawn 'mana) 100
      (get :lightning 'description) "Instantly kill a single slug from any distance."
      (get :fire 'description) "Instantly kill a single slug from any distance."
      (get :hand 'description) "Pull a slug into your inventory from any distance."
      (get :dawn 'description) "Remake the world in your own image and take your place as its god.")

(defparameter *game-modes*
  '(:playing :end-turn :grab :menu :magic :drop :cast :win))

(defparameter *ai-states*
  '(:hunt :graze :mate :idle ; :explore :herd :flee
          ))

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
(defun one-in (n) (if (<= n 0) t (= 0 (random n))))
;;; A probability function with a positive derivative
;;; A higher penalty decreases the probability for low values of x, but has
;;; vanishing impact for large x.
(defun prob (x &optional (penalty 10))
  (and (> x 0) (< (random (+ x penalty)) x)))
(defun random-choice (list)
  (if (null list)
      nil
    (nth (random (length list)) list)))


(defgeneric lerp (start end alpha))
(defmethod lerp ((start number) (end number) (alpha number))
  (+ start (* alpha (- end start))))

(defgeneric snap (value))
(defmethod snap ((val number)) (round val))

(defgeneric scale (factor value))
(defmethod scale ((factor number) (value number)) (* factor value))

(defun v+ (v1 v2) (coord (+ (x v1) (x v2)) (+ (y v1) (y v2))))

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
                       ,function t))    ; return t for "don't remove me"
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
         (:key-down-event (:key key :unicode char)
           (if (= char 63) ;; this should be the value for ?
               (setf (render-help? *game*) t)
               (setf (render-help? *game*) nil))
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
                      :initform nil)
   (render-help? :accessor render-help?
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
         (coord
          (- (/ 400 *tile-size*) (x (player (world *game*))))
          (- (/ 300 *tile-size*) (y (player (world *game*)))))))
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

(defclass coord ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))
(defun coord (x y) (make-instance 'coord :x x :y y))
(defmethod lerp ((start coord) (end coord) (alpha number))
  (coord (lerp (x start) (x end) alpha) (lerp (y start) (y end) alpha)))
(defmethod snap ((val coord)) (coord (snap (x val)) (snap (y val))))
(defmethod scale ((factor number) (val coord))
  (coord (* factor (x val)) (* factor (y val))))

(defclass monster ()
  ((pos :accessor pos
        :initarg :pos
        :type coord)
   (mana :accessor mana
         :initform 1
         :initarg :mana)
   (weight :accessor weight
           :initform 1
           :initarg  :weight)
   (daughters :accessor daughters
              :initarg :daughters
              :initform nil)))
(defmethod x ((thing monster)) (x (pos thing)))
(defmethod y ((thing monster)) (y (pos thing)))

(defclass player (monster)
  ((inventory :accessor inventory
              :initform nil)))

(defclass slug (monster)
  ((color :accessor color
          :initarg :color
          :initform nil)
   (ai-state :accessor ai-state
             :initarg :ai-state
             :initform :idle)
   (home-font :accessor home-font
              :initarg :home-font
              :initform nil)
   (target :accessor target
           :initform (coord (random-delta 100) (random-delta 100)))
   (food :accessor food
         :initform 50
         :initarg :food)
   (life :accessor life
         :initarg :life
         :initform nil)
   (max-life :accessor max-life
             :initarg :max-life
             :initform (mutate 15 20))
   (weapon :accessor weapon
           :initarg :weapon
           :initform (mutate 5 8))
   (armor :accessor armor
          :initarg :armor
          :initform (mutate 3 5))
   (k-strat :accessor k-strat
            :initarg :k-strat
            :initform (mutate 30 40)
            :documentation "Referring to the biological K-strategy, a higher value here means fewer but more powerful offspring; lower value means closer to R-strategy (many weak offspring).")
   (grazing :accessor grazing
            :initarg :grazing
            :initform (mutate 30 40)
            :documentation "Grazing efficiency. Higher value means more nutrition recovered from being in the vicinity of a friendly font.")
   (hunting :accessor hunting
            :initarg :hunting
            :initform (mutate 30 40)
            :documentation "Hunting efficiency. Higher value means more nutrition recovered from eating another slug.")
   (social :accessor social
           :initarg :social
           :initform (mutate 30 40))
   (aggression :accessor aggression
               :initarg :aggression
               :initform (mutate 30 40))
   (curiosity :accessor curiosity
              :initarg :curiosity
              :initform (mutate 30 40)) 
   (weight :initform (mutate 5 5))
   (mana :initform (mutate 1 5))))

(defmethod initialize-instance :after
  ((slug slug) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (aggression color mana weight) slug
        (setf color (sdl:color :r (min 255 (max 0 (* 2 aggression)))
                               :g (min 255 (max 0 (* 5 weight)))
                               :b (min 255 (max 0 (* 15 mana))))))
  (setf (slot-value slug 'life) (max-life slug)
        (slot-value slug 'food) (feeding-threshold slug)))


(defun trait-magnitude (slug)
  "Returns the vector magnitude of all traits of a given slug that are objectively advantageous."
  (with-slots (hunting grazing weapon armor max-life) slug
    (flet ((sq (x) (expt x 2)))
     (sqrt
      (+ (sq hunting) (sq grazing) (sq weapon) (sq armor) (sq max-life))))))
(defun metabolic-cost (slug)
  (ceiling (/ (trait-magnitude slug) 10)))
(defun scale-traits (k slug)
  "Scale the objectively-advantageous traits of a slug so that they have the given magnitude when considered as a vector."
  (let ((factor (/ k (trait-magnitude slug))))
    (macrolet ((% (x) `(setf (,x slug) (snap (* (,x slug) factor)))))
      (% hunting) (% grazing) (% weapon) (% armor) (% max-life))))

(defun cost-of-turns (slug n)
  "The amount of food the slug will lose over the next n turns, assuming normal operation."
  (* (metabolic-cost slug) n))
(defun feeding-threshold (slug)
  "Calculate the food value at which this slug should start to look for food. Should be about 30-50 turns before starvation."
  (cost-of-turns slug 40))
(defun stop-feeding-threshold (slug)
  "Calculate the food value above which this slug should stop feeding and move on to something else. Should be about 50-75 turns above the feeding threshold."
  (+ (feeding-threshold slug) (cost-of-turns slug 70)))


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
  (loop for dx from (- dist) upto dist append
       (loop for dy from (- dist) upto dist
          when (not (and (= dx 0) (= dy 0)))
          when (typep (item-at world (+ (x spot) dx) (+ (y spot) dy))
                      filter-type)
          collect (item-at world (+ (x spot) dx) (+ (y spot) dy)))))
(defun population (&rest args)
  (length (apply 'neighbors args)))
(defun density (&rest args)
  (destructuring-bind (spot world &key (dist 1) &allow-other-keys) args
    (declare (ignore spot world))
    (/ (apply 'population args) (* 2 dist 2 dist))))

(defun sanitize-daughters (mother)
  (prog1 (daughters mother)
    (dolist (d (daughters mother))
      (setf (pos d) (pos mother)))
    (setf (daughters mother) nil)))

(defun birth-all (world)
  (setf (monsters world) 
        (append (monsters world)
                (apply #'append (mapcar #'sanitize-daughters (monsters world)))
                (sanitize-daughters (player world)))))

(defun play-one-round ()
  (when (eq (status *game*) :end-turn) 
    (take-turns (world *game*)) 
    (birth-all (world *game*))
    (setf (active-animations *game*)
          (remove-if 'expired? (active-animations *game*)))
    (dolist (anim (active-animations *game*))
      (and (age-turns anim) (incf (age-turns anim))))
    (setf (status *game*) :playing)))

(defgeneric distance (p1 p2))
(defmethod distance ((p1 coord) (p2 coord))
  (with-slots ((x1 x) (y1 y)) p1
    (with-slots ((x2 x) (y2 y)) p2
      (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))))
(defmethod distance ((m1 monster) (m2 monster))
  (distance (pos m1) (pos m2)))

(defgeneric cast-spell (spell spot))

(defmethod cast-spell ((spell (eql :fire)) spot)
  (declare (ignore spell)) 
  (dolist (monster (monsters (world *game*)))
    (when (< (distance (car monster) spot) 5)
      (kill monster (world *game*)))))

(defmethod cast-spell ((spell (eql :lightning)) spot)
  (declare (ignore spell))
  (destructuring-bind (x y) spot
    (push (make-zap-anim (pos (player *world*)) (coord x y)) (active-animations *game*))
    (if (world-at *world* x y)
        (kill (world-at (world *game*) x y) (world *game*)))))

(defmethod cast-spell ((spell (eql :hand)) spot)
  (declare (ignore spell))
  (destructuring-bind (x y) spot
    (if (item-at *world* x y)
        (push (make-grab-anim (item-at *world* x y) (pos (player *world*)) 60)
              (active-animations *game*)))
    (grab (world *game*) x y)))

(defmethod cast-spell ((spell (eql :dawn)) spot)
  (declare (ignore spot spell))
  (setf (status *game*) :win))

(defun take-turns (world)
  ;; Using maplist so that we can modify the list in-place
  ;; We are only ever interested in (car cells), not cells itself
  (maplist (lambda (cells)
             (if (car cells)
                 (setf (car cells)
                       (take-turn (car cells) world))))
           (monsters world))
  (setf (monsters world)
        (remove nil (monsters world))))


(defgeneric mate (slug1 slug2))
(defmethod mate ((slug1 slug) (slug2 slug))
  (push (make-mate-anim (pos slug1)) (active-animations *game*))
  (push (make-mate-anim (pos slug2)) (active-animations *game*))
  (with-slots (mana weapon max-life social home-font) slug1
    (with-slots (weight armor grazing hunting aggression) slug2
      (let* ((tolerance 5)
             (spawn
              (make-instance 'slug :home-font home-font
                             :mana (mutate mana tolerance)
                             :weight (mutate weight tolerance)
                             :weapon (mutate weapon tolerance)
                             :max-life (mutate max-life tolerance)
                             :social (mutate social tolerance)
                             :armor (mutate armor tolerance)
                             :grazing (mutate grazing tolerance)
                             :hunting (mutate hunting tolerance)
                             :aggression (mutate aggression tolerance))))
        (scale-traits (k-strat slug1) spawn)
        (decf (food slug1) (trait-magnitude spawn))
        (decf (food slug2) (trait-magnitude spawn))
        (push spawn (daughters slug1))))))

(defgeneric mutate (slug &optional tolerance))

(defmethod mutate ((slug slug) &optional tolerance)
  (declare (ignore tolerance))
  (with-slots (mana weight weapon max-life social armor grazing hunting aggression)
      slug
    (make-instance 'slug 
                   :mana (mutate mana)
                   :weight (mutate weight)
                   :weapon (mutate weapon)
                   :armor (mutate armor)
                   :max-life (mutate max-life)
                   :social (mutate social)
                   :grazing (mutate grazing)
                   :hunting (mutate hunting)
                   :aggression (mutate aggression))))

(defmethod mutate ((stat number) &optional (tolerance 20))
  (max 1 (+ stat (random-delta tolerance))))


;;; 
;;; Functions related to slug behavior
;;; 

(defun eat (slug1 slug2 world)
  (incf (mana slug1) (ceiling (/ (mana slug2) 2)))
  (incf (food slug1) (ceiling (/ (weight slug2) 2)))
  (kill slug2 world))

(defun fight (slug1 slug2 world) 
  (decf (life slug1) (max 1 (- (weapon slug2) (armor slug1))))
  (decf (life slug2) (max 1 (- (weapon slug1) (armor slug2))))
  (cond ((< (life slug1) 1 (life slug2))
         (eat slug2 slug1 world))
        ((< (life slug2) 1 (life slug1))
         (eat slug1 slug2 world))
        (t
         (push (make-bash-anim (pos slug1))
               (active-animations *game*))
         (push (make-bash-anim (pos slug2))
               (active-animations *game*)))))


;;; Used in take-turn for slugs
;;; Should the given slug turn into a font?
(defun transform? (slug coords world)
  (with-slots (home-font target food mana weight)
              slug
    (or
     (and (< food 3)
          (one-in
           (+ 100 (- mana)
              (population coords world :dist 5 :filter-type 'slug-font)))))))
(defun set-explore-target (monster coords world)
  (loop while (prob (population (target monster) world :dist 5) 2) do
        (setf (target monster)
              (coord (+ (x coords) (random-delta 100))
                     (+ (y coords) (random-delta 100))))))

(defgeneric take-turn (monster world))

(defmethod take-turn ((monster slug-font) world)
  (let ((coords (pos monster)))
    (if (one-in (+ 15 (population coords world)))
        (let ((spawn (mutate monster)))
          (setf (home-font spawn) monster)
          (push spawn (daughters monster))))
    (if (< (life monster) (max-life monster))
        (incf (life monster))))
  monster)

(defmethod take-turn ((monster slug)  world)
  (with-slots (aggression home-font target food mana weight k-strat
               grazing  hunting max-life social ai-state (coords pos))
      monster
    (unless (member home-font (monsters world) :test #'equal)
      (setf home-font (find-home-font world monster)))
    ;; Act according to current state
    (ecase ai-state
      (:idle (idle-behavior monster))
      (:hunt (hunting-behavior monster))
      (:graze (grazing-behavior monster))
      (:mate (mating-behavior monster)))
    ;; Possibly transition to another state, and set an appropriate
    ;; target for that state
    (cond
     ((and (not (member ai-state '(:hunt :graze)))
           (< food (feeding-threshold monster)))
      (if (prob grazing hunting)
          (setf ai-state :graze
                target   home-font)
        (setf ai-state :hunt
              target (or (random-choice (neighbors coords *world* :dist 25))
                         home-font))))
     ((and (not (eq ai-state :mate))
           (> food (* 2 k-strat)))
      (setf ai-state :mate
            target (or (random-choice (neighbors coords *world* :dist 10))
                       (random-choice (neighbors coords *world* :dist 25))
                       monster))))
    ;; Metabolism
    (unless (>= (life monster) max-life)
        (incf (life monster))
        (decf food (metabolic-cost monster)))
    (decf food (metabolic-cost monster))
    (cond
     ((transform? monster coords world)
      (change-class monster 'slug-font)
      monster) 
     ;; nil return value says to delete this monster
     ((or (< (life monster) 0) (< food 0))
      (kill monster world)
      nil)
     (t monster))))

(defun mating-behavior (monster)
  (cond ((and (< (distance monster (target monster)) 2)
              (< (density (pos monster) *world* :dist 3) 3/4))
         (mate monster (target monster)))
        ((not (one-in 30))
         (walk-toward monster (pos (target monster)) *world*))
        (t
         (setf (target monster) (random-choice (monsters *world*))))))

(defun idle-behavior (monster)
  (cond ((one-in 10)
         (setf (target monster)
               (v+ (pos monster) (coord (random-delta 8) (random-delta 8)))))
        (t
         (walk-toward monster (target monster) *world*))))

(defun grazing-behavior (monster)
  (cond ((> (distance monster (target monster)) 8)
         (walk-toward monster (target monster) *world*))
        ((< (density (pos monster) *world* :dist 4)
            (/ (grazing monster) 100))
         (incf (food monster) (ceiling (/ (grazing monster) 5)))
         (random-move monster *world*))
        (t (random-move monster *world*))))

(defun hunting-behavior (monster)
  (cond ((> (distance monster (target monster)) 2)
         (fight monster (target monster) *world*))
        (t
         (walk-toward monster (target monster) *world*))))


(defgeneric find-home-font (world slug))
(defmethod find-home-font ((world world) (slug slug))
  (let* ((fonts (remove-if-not (lambda (x) (typep x 'slug-font))
                               (monsters world)))
         (scores (mapcar (lambda (x) (color-dist slug x)) fonts))
         (best (if scores (apply #'min scores)))
         (favorite-font (cdr (assoc best (pairlis scores fonts)))))
    favorite-font))




;;; 
;;; Functions related to drawing
;;; 

(defun blink-image (coord period image)
  "Returns a function suitable for use as the draw-fn of an animation.
x and y are the coordinates to draw to. period is the length of one full blink-on, blink-off cycle in frames."
  (with-slots (x y) coord
   (unless (and x y)
     (print (monsters *world*))
     (error "Here we go! The bug is here! Backtrace! o.o"))
   (lambda (turns frames surface player-offset)
     (declare (ignore turns))
     (let* ((px (x player-offset))
            (py (y player-offset))
            (x1 (+ x px))
            (y1 (+ y py)))
       (if (< 0 (mod frames period) (ceiling (/ period 2)))
           (sdl:draw-surface-at-* image (* x1 *tile-size*)
                                  (* y1 *tile-size*)
                                  :surface surface))))))

(defun make-death-anim (coord)
  (make-instance 'animation :draw-fn (blink-image coord 15 *skull*)
                 :turns 2 :frames 60))
(defun make-bash-anim (coord)
  (make-instance 'animation :draw-fn (blink-image coord 15 *bash*)
                 :turns 2 :frames 60))
(defun make-mate-anim (coord)
  (make-instance 'animation :draw-fn (blink-image coord 15 *heart*)
                 :turns 2 :frames 60))

(defun make-grab-anim (slug player-pos flight-time)
  (let ((player-x (x player-pos))
        (player-y (y player-pos))
        (slug-x (x slug))
        (slug-y (y slug)))
    (make-instance 'animation :frames flight-time :turns 2
                   :draw-fn
                   (lambda (turns frames surface player-offset)
                     (declare (ignore turns)) 
                     (draw slug
                           (+ (round (lerp slug-x player-x (/ frames flight-time))) (x player-offset))
                           (+ (round (lerp slug-y player-y (/ frames flight-time))) (y player-offset))
                           surface)))))

(defun make-zap-anim (start-pos end-pos)
  (let ((paths (loop repeat 3 collect (random-lightning-path))))
   (make-instance 'animation :frames 40 :turns 2 :draw-fn
    (lambda (turns frames surface player-offset)
      (declare (ignore turns))
      (dolist (path paths)
        (draw-lightning-path (scale *tile-size*
                                    (move (v+ start-pos player-offset) 1/2 1/2))
                             (scale *tile-size*
                                    (move (v+ end-pos player-offset) 1/2 1/2))
                             path surface sdl:*white*))
      (if (= (mod frames 5) 0)
          (progn (setf (car paths) (random-lightning-path))
                 (rotatef (car paths) (cadr paths) (caddr paths))))))))

(defun draw-lightning-path (start end offsets surface color)
  "Takes a start coord, an end coord, and a list of (alpha off-x off-y) triples, and draws a zigzag path according to them."
  (loop for (alpha off-x off-y) in offsets with endpoint = start with nextpoint do
        (setf nextpoint (snap (move (lerp start end alpha) off-x off-y)))
        (sdl:draw-line-* (x endpoint)  (y endpoint)
                         (x nextpoint) (y nextpoint) 
                         :surface surface :color color)
        (setf endpoint nextpoint)))
(defun random-lightning-path ()
  (loop for alpha = 0 then (+ alpha (/ (random 10) 30))
        while (<= alpha 1)
        collect (list alpha (* (sqrt alpha) (random-delta 20))
                            (* (sqrt alpha) (random-delta 20)))))

(defgeneric draw (object x y window))

(defmethod draw ((object world) x0 y0 window)
  (dolist (monster (monsters object))
    (let ((x (x monster))
          (y (y monster))) 
      (draw monster (+ x0 x) (+ y0 y) window))) 
  (if (player object)
      (let* ((player (player object))
             (x (x player))
             (y (y player)))
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
(defmethod move ((monster monster) dx dy)
  (setf (pos monster) (move (pos monster) dx dy))
  monster)
(defmethod move ((pos coord) dx dy)
  (coord (+ dx (x pos)) (+ dy (y pos))))
(defmethod move :before ((object player) dx dy)
  (setf (status *game*) :end-turn))


(defgeneric random-move (monster world))
(defmethod random-move ((monster monster) world)
  (let ((directions (list (list 1 0) (list -1 0) (list 0 1) (list 0 -1))))
    (destructuring-bind (dx dy)
        (random-choice
         (template-delete
          (cons (list 0 0) directions)
          (cons t
                (mapcar (lambda (dir)
                          (with-slots (x y) (pos monster)
                            (destructuring-bind (dx dy) dir
                             (not (world-at world (+ x dx) (+ y dy))))))
                        directions))))
      (move monster dx dy))))

(defgeneric walk-toward (monster spot world))
(defmethod walk-toward ((monster monster) spot world)
  (let ((x (x monster))
        (y (y monster)))
    (destructuring-bind (dx dy)
        (list (cond ((< x (x spot))  1)
                    ((> x (x spot)) -1)
                    (t               0))
              (cond ((< y (y spot))  1)
                    ((> y (y spot)) -1)
                    (t                0)))
      (if (world-at world (+ x dx) (+ y dy))
          (random-move monster world)
          (move monster dx dy)))))



(defgeneric world-at (world x y))
(defmethod world-at (world x y)
  (if (and (player world) (= x (x (player world))) (= y (y (player world))))
      (player world)
      (find-if (lambda (slug) (and slug (= (x slug) x) (= (y slug) y)))
               (monsters world))))

(defgeneric item-at (world x y))
(defmethod item-at (world x y)
  ;; These used to have slightly different behavior. item-at is no longer
  ;; useful.
  (world-at world x y))


(defun remove-monster (monster world)
  (setf (monsters world)
        (remove monster (monsters world))))


(defun kill (monster world)
  (let ((coords (pos monster)))
    (unless (and (x coords) (y coords))
      (print monster)
      (print (monsters world)))
    (remove-monster monster world)
    (push (make-death-anim coords)
          (active-animations *game*))))

(defun grab (world x y)
  (let ((item (item-at world x y)))
    (when (and item (< (+ (inv-weight (player world))
                          (weight item))
                       (weight (player world))))
      (push item (inventory (player world)))
      (remove-monster item world))))

(defun inv-weight (player)
  (reduce #'+ (mapcar #'weight (inventory player))))


(defun drop (obj world)
  (push obj (daughters (player world)))
  (setf (inventory (player world))
        (delete obj (inventory (player world)))))



(defun draw-game (window)
  (draw (world *game*)
        (- (/ 400 *tile-size*) (x (player (world *game*))))
        (- (/ 300 *tile-size*) (y (player (world *game*))))
        window)
  (dolist (anim (active-animations *game*))
    (unless (expired? anim) (draw-animation anim window))
    (and (age-frames anim) (incf (age-frames anim)))))

(defun mod* (num divisor)
  (let ((x (mod num divisor)))
    (values x (floor (/ num divisor)))))

(defgeneric draw-slug-info (slug w h))
(defmethod draw-slug-info ((slug slug) w h)
  (let ((surface (sdl:create-surface w h))
        (height *font-height*)
        (width (sdl:char-width sdl:*default-font*))
        (sdl:*default-color* (if (< (color-dist (color slug) sdl:*white*) 300)
                                 sdl:*black*
                                 sdl:*white*)))
    (labels ((att-name (att)
               (concatenate
                'string
                (case att
                  (weapon "ATTK")
                  (aggression "AGRO")
                  (mana "MANA")
                  (weight "WGHT")
                  (social "SOCL")
                  (armor "ARMR")
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
      (print-att 'aggression 0 2)
      (print-att 'weapon 0 3)
      (print-att 'weight 1 1)
      (print-att 'food 1 2)
      (print-att 'life 1 3)
      (print-att (if (< (grazing slug) (hunting slug)) 'hunting 'grazing)
                 2 1)
      (print-att 'social 2 2)
      (print-att 'armor 2 3)
      (list surface))))
(defmethod draw-slug-info ((slug null) w h)
  ())
(defmethod draw-slug-info ((slug player) w h)
  ())

(defun draw-spell-info (spell w h)
  (if spell
      (let ((surface (sdl:create-surface w h))
            (height *font-height*))
        (sdl:draw-string-solid-* (format nil "~a:~3d" spell (get spell 'mana))
                                 0 0 :surface surface)
        (sdl:draw-surface-at-* (render-text (get spell 'description) w)
                               0 height :surface surface) 
        (list surface))))

(defun inventory-world (list w h)
  (if list
      (apply #'register (make-instance 'world)
             (loop for i upto (* w h) for slug in list do
                   (setf (pos slug)
                         (multiple-value-call
                          #'coord (mod* i (/ w *tile-size*))))
                   collect slug))
      (make-instance 'world)))

 (defun draw-inventory (w h)
  (let ((inventory-surface (sdl:create-surface w h))
        (inventory (inventory-world
                    (reverse (inventory (player (world *game*))))
                    w h)))
    (draw inventory 0 0 inventory-surface)
    (list inventory-surface)))

(defun draw-spells (w h)
  (let ((thumbnails (sdl:create-surface w h))
        (stats (sdl:create-surface w (+ (* 2 *font-height*) 3))))
    (sdl:draw-string-solid-* (format nil "M:~3d"
                                     (mana (player (world *game*))))
                             0 1 :surface stats)
    (sdl:draw-string-solid-* (format nil "W:~3d"
                                     (inv-weight (player (world *game*))))
                           0 (+ *font-height* 1) :surface stats)
    (loop for spell in *spells* for x0 from 0 do
          (draw spell x0 0 thumbnails))
    (list thumbnails stats)))

(defun get-help-string (mode) 
  (case mode
    (:playing "Use qwe ad zxc to move.
s to wait.
m for magic.
g to grab, t to drop.")
    (:magic "Click on a slug in your inventory to gain mana.
Click a spell to select it.")
    (:drop "Click on a slug in your inventory.")
    (:cast "Click a square to target.")))

(defun draw-controls (w)
  (if (render-help? *game*)
      (list (render-text (get-help-string (status *game*)) w 1))
      nil))

(defun draw-ui (window)
  (let ((top (ui-top *game*))
        (border (ui-border *game*))
        (ui-widgets (list (draw-inventory 80 40)
                          (draw-spells 40 10)
                          (draw-controls 200)
                          (draw-slug-info (selected-slug *game*) 200 40)
                          (draw-spell-info (active-spell *game*) 200 40))))
    (sdl:draw-box-* 0 top 800 80 :surface window
                    :color (sdl:color :r 25 :g 25 :b 255))

    (loop for column in ui-widgets with x = border do
         (progn
           (loop for widget in column with y = (+ top border)
              when widget do
                (progn (sdl:draw-surface-at-* widget x y :surface window)
                       (incf y (+ border (sdl:height widget))))) 
          (incf x (+ border (apply 'max 0 (mapcar 'sdl:width column))))))))


(defun at-cursor (world x y x0 y0)
  (let ((x (+ (floor (/ x *tile-size*)) x0))
        (y (+ (floor (/ y *tile-size*)) y0)))
    (item-at world x y)))

(defun inv-at-cursor (x y)
  (at-cursor (inventory-world
              (reverse (inventory (player (world *game*)))) 80 40)
             (- x (ui-border *game*))
             (- y (ui-border *game*) (ui-top *game*))
             0 0))

(defun spell-at-cursor (x y)
  (let ((spell-top (+ (ui-border *game*) (ui-top *game*)))
        (spell-left (+ 80 (* 2 (ui-border *game*)))))
   (and (< spell-top y (+ spell-top *tile-size*))
        (< spell-left x)
        (nth (floor (/ (- x spell-left) *tile-size*)) *spells*))))

(defun eat-slug (x y)
  (let ((slug (inv-at-cursor x y)))
    (when slug
      (incf (mana (player (world *game*))) (mana slug))
      (setf (inventory (player (world *game*)))
            (delete slug (inventory (player (world *game*))))))))

(defun win-message (x y window)
  (let ((h (+ 1 *font-height*)))
    (sdl:with-surface (window)
      (sdl:draw-string-solid-* "YOU WIN! :D" x y)
      (sdl:draw-string-solid-* "Press escape to return to game" x (+ y h))
      (sdl:draw-string-solid-* "or close the window to quit." x (+ y h h)))))

(defun subline (string start &optional stop)
  (let ((real-start (position-if-not (lambda (c) (char= c #\Space))
                                     string :start start :end stop))
        (real-stop (position-if-not (lambda (c) (char= c #\Space))
                                    string :start start :end stop :from-end t)))
    (if (and real-start real-stop)
        (subseq string real-start (1+ real-stop))
        "")))

(defun break-lines/no-newlines (string width)
  (let* ((breaks (loop for c across (format nil "~a " string) for i from 0 when (char= c #\Space)
                    collect (1+ i)))
         (lines (loop for pos in (append breaks '(nil)) for lastpos in (cons 1 breaks)
                   for left = breaks then (cdr left) with start = 0
                   when (or (not left)
                            (= (length (subline string start lastpos)) width)
                            (< (length (subline string start lastpos))
                               width
                               (length (subline string start (max 0 (1- pos))))))
                   collect (prog1 (if left (subline string start (1- lastpos))
                                      (subline string start))
                               (setf start lastpos)))))
    lines))

(defun break-lines (string width)
  (apply 'append (mapcar (lambda (s) (break-lines/no-newlines s width))
                         (loop for i = 0 then (1+ j)
                              as j = (position #\Newline string :start i)
                              collect (subseq string i j)
                              while j))))

(defun render-text (string width &optional (padding 1))
  "Return a surface consisting of the given text with newlines added to keep each line shorter than the given width (in pixels). As a second return value, also returns the height of this surface."
  (let* ((strings (break-lines string (/ width *font-width*)))
         (height (+ padding (* (length strings) (+ padding *font-height*))))
         (surface (sdl:create-surface width height)))
    (loop for line in strings for y = padding then (+ *font-height* padding y) with x = 0 do
         (sdl:draw-string-solid-* line x y :surface surface))
    surface))

(defun game-loop ()
  (sdl-loop
      ((:playing (*key-down-hooks*
                  (if (eq key :sdl-key-escape) (setf (status *game*) :end-turn))
                  (when (directional key)
                    (destructuring-bind (x y) (get key 'direction)
                      (move (player (world *game*)) x y)))
                  (when (eq key :sdl-key-g)
                    (setf (status *game*) :grab)
                    (key-down-hook
                      (when (directional key)
                        (let ((key* key))
                          (let ((x (x (player *world*)))
                                (y (y (player *world*))))
                            (destructuring-bind (x1 y1) (get key* 'direction)
                              (grab (world *game*) (+ x x1) (+ y y1))))))
                      (setf *key-down-hooks* nil)
                      (setf (status *game*) :end-turn)))
                  (when (eq key :sdl-key-t) (setf (status *game*) :drop))
                  (when (eq key :sdl-key-m) (setf (status *game*) :magic))))
       (:drop (*mouse-button-down-hooks*
               (when (= button 1)
                 (let ((thing (inv-at-cursor x y)))
                   (when thing
                     (drop thing (world *game*))
                     (setf (status *game*) :end-turn))))))
       (:magic (*mouse-button-down-hooks*
                (when (= button 1)
                  (if (< x (+ (* (ui-border *game*) 2) 80))
                      (eat-slug x y)
                      (let ((spell (spell-at-cursor x y)))
                        (when (and spell (<= (get spell 'mana)
                                             (mana (player (world *game*)))))
                          (setf (active-spell *game*) spell
                                (status *game*) :cast)))))))
       (:cast (*mouse-button-down-hooks*
               (if button
                   (cast-spell (active-spell *game*)
                               (let ((x0 (x (player *world*)))
                                     (y0 (y (player *world*))))
                                 (list (- (floor (/ x *tile-size*)) 40 (- x0))
                                       (- (floor (/ y *tile-size*)) 30 (- y0))))))
               (progn x y button
                      (unless (eq (active-spell *game*) :dawn)
                        (setf (status *game*) :end-turn))))))
    (:idle ()
           (sdl:clear-display sdl:*black*) ; later make it clear with the map
           (if (eq (status *game*) :win)
               (win-message 280 100 sdl:*default-display*)
               (progn (draw-game sdl:*default-display*)
                      (draw-ui sdl:*default-display*)
                      (play-one-round)))
           (if (eq (status *game*) :playing)
               (setf (mana (player (world *game*)))
                     0)))
    (:mouse-motion-event (:x x :y y)
      (setf (selected-slug *game*)
            (cond ((< y (ui-top *game*))
                   (let ((x0 (x (player *world*)))
                         (y0 (y (player *world*))))
                     (at-cursor (world *game*) x y (- x0 40) (- y0 30))))
                  ((< x (+ (ui-border *game*) 80))
                   (inv-at-cursor x y)))
            (active-spell *game*)
            (if (not (eq (status *game*) :cast))
                (spell-at-cursor x y)
                (active-spell *game*))))))

(defun main ()
  (setf *random-state* (make-random-state t))
  (setf *world*
        (register
         (make-instance 'world :player (make-instance 'player
                                               :weight 30
                                               :pos (coord 8 12)))
         (make-instance 'slug-font :pos (coord -5 -5))
         (make-instance 'slug-font :pos (coord 30 25))
         (make-instance 'slug-font :pos (coord -13 29) :social 100)))
  (setf *game* (make-instance 'game :world *world*))
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "Cannibal Slugmage of Eden")
    (sdl:enable-key-repeat 300 100)
    (sdl:enable-unicode)
    (setf *skull* (sdl:load-image "death.bmp")
          *bash* (sdl:load-image "hit.bmp")
          *heart* (sdl:load-image "heart.bmp"))
    (game-loop)))
