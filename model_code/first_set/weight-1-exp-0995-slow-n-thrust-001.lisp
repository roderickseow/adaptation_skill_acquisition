
;;; Changed for new ACT-R

; Model parameters:
; - Bad weight
;   - All controllers: -1
; - Decay
;   - Exponential: 0.995
; - Slowdown
;   - No
; - Controller min-thrust-time: 0.01
; - Max-comfort-speed
;   - Yes: 3.0
; - Controller min-press-point: 0

;;;global variables for monitoring state
  (defparameter *thrust* nil) ;has a thrust been issued
  (defparameter *vulnerability* 0); current vulnerability
  (defparameter *missiles* nil); number of missiles present
  (defparameter *points* 0)
  (defparameter *data* nil)
  (defparameter *rectangle1* nil)
  (defparameter *rectangle2* nil)
  (defparameter *turn* nil)
  (defparameter *tap-thrust* nil)
  (defparameter *max-comfort-speed* 3.0)
  

;;; This notes relevant information for purpose of updating the imaginal and game-state module in space track
(defun record-features-track (data)
    (setf *rectangle1* (getf data :rectangle1))
    (setf *rectangle2* (getf data :rectangle2))
    (game-state-features-track data)
  (let* ((points (getf data :pnts)) aim
        (ship (getf data :ship))
        (speed (speed-sf (getf ship :vx) (getf ship :vy)))
         (nextstep (chunk-slot-value-fct (buffer-read 'goal) 'nextstep))
         (target (chunk-slot-value-fct (buffer-read 'goal) 'ship-speed))
        (keys (getf data :keys)))
    (cond ((and  (equal nextstep 'maketurn2) *tap-thrust*)
          (setf *tap-thrust* nil) 
          ;(print (list (get-time) target speed (if (or (< speed 1.5) (> speed 2.5)) 1 0)))
          (if  (> speed *MAX-COMFORT-SPEED*) (update-slot 'badspeed) (update-slot 'goodspeed))))
    (cond ((member :thrust keys) (setf *thrust* t))
          ((not (null keys)) (setf *turn* t))
          ((and *turn* *thrust* ship) (setf *turn* nil *thrust* nil) 
           (setf aim (chunk-slot-value-fct (buffer-read 'game-state) 'offaim))
           (if (< aim 40) (update-slot 'good-aim) (update-slot 'bad-aim))))
    (if (and (numberp points) (numberp *points*))
      (cond ((> points *points*) (update-slot  'rectangle))
            ((< points *points*) (update-slot  'death))))
      (setf *points* points)))

(defun outward (x y vx vy)
       (let* ((x1 (- x 355))
              (y1 (- y 315))
              (dist1 (sqrt (+ (* x1 x1) (* y1 y1) )))
              (x2 (- (+ x vx) 355))
              (y2 (- (- y vy) 315))
              (dist2 (sqrt (+ (* x2 x2) (* y2 y2)))))
         (- dist2 dist1)))

;;; This notes relevant information for the metacognitive module after every tick
(defun game-state-features-track (data)
(setf *data* data)
 (if (car (no-output (buffer-chunk game-state))) 
   (cond ((equal (getf data :screen-type) "score" )
          (mod-buffer-chunk 'game-state (list 'status 'game-over 
                                              'orientation nil  'future-angle nil
                                                'moving-angle nil 'ship-alive nil 'r1 nil 'r2 nil
                                                'x nil 'y nil 'vx nil 'vy nil 'offaim nil
                                                'current-angle nil 'ship-speed nil)))
         ((getf (getf data :ship) :alive)
          (let* ((ship (getf data :ship))
                 (x (getf ship :x))
                 (y (getf ship :y))
                 (vx (getf ship :vx))
                 (vy (getf ship :vy))
                 (moving-angle (angle 0 0 vx vy))
                 (speed (getf ship :speed))
                 (orientation (mod (getf ship :orientation) 360))
                 (r1 (getf data :rectangle1))
                 (r2  (getf data :rectangle2))
                 (r1-angle  (box-angle r1))
                 (r2-angle  (box-angle r2))
                 (alive  (if (getf ship :alive) 'yes 'no))
                 (rect  (or (chunk-slot-value-fct (buffer-read 'goal) 'rectangle) r1)))
            (mod-buffer-chunk 'game-state (list 'orientation orientation 'game 'spacetrack
                                                'moving-angle moving-angle 'ship-alive alive 'r1 r1 'r2 r2
                                                'current-angle (if (equal rect r1) r1-angle r2-angle)
                                                'future-angle (if (equal rect r1) r2-angle)
                                                'x x 'y y 'vx vx 'vy vy 'offaim  (HIT-SIDES rect X Y VX VY)
                                               'ship-speed speed))))
         (t
          (mod-buffer-chunk 'game-state (list  'orientation nil  
                                                'moving-angle nil 'ship-alive 'no 'r1 nil 'r2 nil
                                                'x nil 'y nil 'vx nil 'vy nil 'offaim nil 'future-angle nil
                                                'current-angle nil 'ship-speed nil))))))


(defun update-slot (slot)   
       (schedule-mod-buffer-chunk 'imaginal (list slot 1) 0))

(defun speed-sf (vx vy) (sqrt (+ (* vx vx) (* vy vy)))) 

;calculates the difference between 2 angles dealing with looping at 360
(defun angle-offset (ang1 ang2)
          (let* ((offset (mod (- ang1 ang2) 360)))
            (if (> offset 180) (- 360 offset) offset)))

;what is angle of vector from first to second point?
(defun angle (x1 y1 x2 y2)
  (let* ((run (- x2 x1))
         (rise (- y2 y1))
         (a (if (not (zerop run))  (* (atan (/ rise run)) (/ 180 pi)))))
    (cond ((zerop run) (if (>= rise 0) 90 270))
          ((> run 0) (if (>= rise 0) a (+ 360 a)))
          (t (if (>= rise 0) (+ 180 a) (+ 180 a))))))


;how far from end will ship hit side of rectangle?
(defun hit-sides (rect x y vx vy)
  (max (hit-side (first rect) (second rect) x y vx vy) (hit-side (fourth rect) (third rect) x y vx vy)))

(defun hit-side (p1 p2 x y vx vy)
  (let* ((point (intersection-point p1 p2 x y vx vy)))
    (if  (and point (if (> vx 0) (< x (first point)) (> x (first point))))
      (sqrt (+ (sqr (- (first point) (second p2)))  (sqr (- (second point) (fourth p2))))) 0)))  

(defun safe-div (a b) (if (not (zerop b)) (/ a b) (progn (print 'bad-division) 1000)))

(defun intersection-point (p1 p2 x y vx vy) 
  (let* ((slope1  (safe-div (- vy) vx))
         (inter1 (- y (* x slope1)))
         (x1 (second p1))
         (x2 (second p2))
         (y1 (fourth p1))
         (y2 (fourth p2))
         (slope2 (if (not (equal x1 x2))  (safe-div (- y2 y1) (- x2 x1))))
         (inter2 (if slope2 (- y1 (* x1 slope2))))
         (point (line-intersect (list inter1 slope1)(if slope2 (list inter2 slope2) x2))))
    (if (and point (within (first point) x1  x2) (within (second point) y1  y2) ) point)))

(defun line-intersect (line1 line2)
  (cond ((and (listp line1) (listp line2))
         (let* ((a1 (first line1))
                (b1 (second line1))
                (a2 (first line2))
                (b2 (second line2)))
           (cond ((not (= b1 b2))
                  (let* ((x (safe-div (- a2 a1) (- b1 b2)))
                         (y (+ a1 (* b1 x))))
                    (list x y))))))
        ((listp line1)
         (list line2 (+ (first line1) (* line2 (second line1)))))
        ((listp line2)
         (list line1 (+ (first line2) (* line1 (second line2)))))))

(defun within (a b c)
  (or (and (>= b a) (>= a c)) (and (<= b a) (<= a c))))

;what is the orientation of a rectangle?
(defun box-angle (box)
  (if box
  (let* ((x1 (second (first box)))
         (x2 (second (second box)))
         (y1 (- (fourth (first box))))
         (y2 (- (fourth (second box)))))
          (angle x1 y1 x2 y2)) 0))         


;what angle to orient to to make a turn?   
(defun turn-angle (angle moving-angle) 
  (let* ((ntarget (if (> moving-angle angle) (+ 360 angle (- moving-angle)) (- angle moving-angle)))
         (naim (if (> ntarget 360) (- 270 (safe-div (- 360 ntarget) 2)) (+ 90 (/ ntarget 2)))))
    (mod (+ moving-angle naim) 360.0)))


;How far is the ship into the next rectangle?
(defun distance-in (rect x y vx vy)
  (let* ((p1 (first rect))
         (p2 (second rect))
         (p3 (third rect))
         (p4 (fourth rect))
         (result (list (in-help p1 p2 x y vx vy) (in-help p2 p3 x y vx vy)
                         (in-help p3 p4 x y vx vy) (in-help p4 p1 x y vx vy))))
    (if result (apply 'max result) -1000)))

(defun in-help (p1 p2 x y vx vy)
  (let* ((point (intersection-point p1 p2 x y vx vy))
         (distance (if point (sqrt (+ (sqr (- x (first point))) (sqr (- y (second point))))))))
    (cond ((null point) -1000)
          ((> vx 0) 
           (if (> x (first point)) distance (- distance)))
          (t (if (> x (first point)) (- distance) distance)))))

(defun sqr (x) (* x x))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Code to run conditions



(defparameter *alpha* .25)
(defparameter *factor* .5)

(defparameter *strategy* 1)
(defparameter *accelSeq* (list 0.4 0.4 0.4 0.4 0.4))
(defparameter *condition* "\"MMMMM\"")
(defparameter *trialList* (list 0))
(defparameter *acceleration* 0.4)

(defparameter *min-thrust-time* 0.01)
(defparameter *min-press-point* 0.0)


(defparameter *bad-weight* -1)

(defparameter *decayMethod* 'exponential)
(defparameter *decay* 0.995)


(defparameter *slowdown* "\"noslow\"")


(defparameter *decayType* "\"none\"")
(case *decayMethod*
  (nil (setf *decayType* "\"none\""))
  (power (setf *decayType* "\"power\""))
  (exponential (setf *decayType* "\"exp\"")))


(defparameter *decayValue* 0)
(if (not *decay*)
  (setf *decayValue* 0)
  (setf *decayValue* *decay*))



(defun run-condition (policy n experiment flag)
  (case experiment
    (1 
      (case policy
        (1 (setf *accelSeq* (list 0.4 0.4 0.4 0.4 0.4))
          (setf *condition* "\"MMMMM\""))
        (2 (setf *accelSeq* (list 0.2 0.2 0.2 0.2 0.4))
          (setf *condition* "\"LLLLM\""))
        (3 (setf *accelSeq* (list 0.6 0.6 0.6 0.6 0.4))
          (setf *condition* "\"HHHHM\""))
        (4 (setf *accelSeq* (list 0.2 0.6 0.2 0.6 0.4))
          (setf *condition* "\"LHLHM\""))
        (5 (setf *accelSeq* (list 0.6 0.2 0.6 0.2 0.4))
          (setf *condition* "\"HLHLM\""))))
    (2 
      (case policy
        (1 (setf *accelSeq* (list 0.6 0.6 0.6 0.6 0.6))
          (setf *condition* "\"HHHHH\""))
        (2 (setf *accelSeq* (list 0.2 0.2 0.2 0.2 0.6))
          (setf *condition* "\"LLLLH\""))
        (3 (setf *accelSeq* (list 0.4 0.4 0.4 0.4 0.6))
          (setf *condition* "\"MMMMH\""))
        (4 (setf *accelSeq* (list 0.2 0.4 0.2 0.4 0.6))
          (setf *condition* "\"LMLMH\""))
        (5 (setf *accelSeq* (list 0.4 0.2 0.4 0.2 0.6))
          (setf *condition* "\"MLMLH\"")))))
  (dolist (accel *accelSeq*)
    (dotimes (i 8)
      (push-last accel *trialList*)))
  (run-n-games n flag 1000))
      
      
(defun run-n-games (n flag speed)
  (setf *data-hook* 'record-features-track) 
  (setf *instructions* spacetrack-instructions)
  (setf *acceleration* (nth 1 *trialList*))
  (play-sf-games 1 :speed 1000 :draw flag :condition "nav" :config `(("ship.acceleration" ,*acceleration*)) 
    :extra `(("condition", *condition*) ("bad.weight", *bad-weight*) ("decay.value", *decayValue*) ("decay.method", *decayType*) 
      ("slowdown", *slowdown* )("min.thrust.time", *min-thrust-time*)("min.press.point", *min-press-point*)("max.comfort.speed", *max-comfort-speed*)))
  (do ((i 1 (1+ i))
       (quads (list (quad-lists 1)) (cons (quad-lists (1+ i)) quads)))
      ((= i n) (reverse quads))
    (setf *acceleration* (nth (+ 1 i) *trialList*))
    (play-sf-games 1 :speed speed :draw flag :cont t :config `(("ship.acceleration" ,*acceleration*)) 
      :extra `(("condition", *condition*) ("bad.weight", *bad-weight*)("decay.value", *decayValue*) ("decay.method", *decayType*) 
        ("slowdown", *slowdown*)("min.thrust.time", *min-thrust-time*)("min.press.point", *min-press-point*)("max.comfort.speed", *max-comfort-speed*)))))


(defun quad-lists (i)
   (cons (session-report)
          (mapcar (lambda (x) (list i x)) (no-output (print-tracker-stats)))))

(defun session-report ()
          (let* ((set2 (set-difference (no-output (pp)) (union session1 starting)))
                (temp0 (mapcar 'p-count starting))
                (temp1 (if session1 (mapcar 'p-count session1) (mapcar 'p-count set2)))
                (temp2 (if session1  (mapcar 'p-count set2) (mapcar 'p-count session1)))
                (types (list (- (length starting) (count 0 temp0)) (- (length temp1) (count 0 temp1)) (- (length temp2) (count 0 temp2))))
                (tokens (list (apply '+ temp0) (apply '+ temp1) (apply '+ temp2))))
            (reset-p-count (get-module :p-count))
             (append (list (length starting) (if session1 (length session1) (length set2)) (if session1 (length set2) (length session1))) types tokens)))

(defparameter *data-hook* 'record-features-track)


(defparameter starting '(GET-STARTED GET-STARTED-AGAIN GET-STARTED-NEW-GAME START-PLAYING-SPACETRACK START-PLAYING-SPACEFORTRESS START-PLAYING-AGAIN-SPACETRACK 
                                     START-PLAYING-AGAIN-SPACEFORTRESS DETECT-TRIAL-END DETECT-DESTROYED DO-SOMETHING DO-SOMETHING-FAST OBJECT-PRESENT OBJECT-NOT-PRESENT TEST-DIMENSION-SUCCESS 
                                     TEST-DIMENSION-FAIL TURN-TO-POINT1 SKIP-TURN START-PRESS-CLOCKWISE PRESS-CLOCKWISE-TAP START-PRESS-COUNTERCLOCKWISE PRESS-COUNTERCLOCKWISE-TAP TAP-KEY TAP-W TAPPPING-D
                                     PRESSING-D FINISH-PRESS-D PRESSING-A FINISH-PRESS-A TAPPING-A CALCULATE-TURN-ANGLE THRUST-NEW-RECTANGLE FINISH-THRUST INCREMENT-ANGLE DECREMENT-ANGLE EMERGENCY-THRUST 
                                     DELAY-OK PRESS-SPACEBAR DOUBLE-SPACEBAR DOUBLE-SHOT))

(defparameter session1 nil)

(defparameter spacetrack-instructions
'((spacetrack
  ISA OPERATOR
   PRE  spacetrack
   action  test-dimension 
   arg1 offaim
   success  adjust
   FAIL  test-speed)
(turn-to-target
  ISA OPERATOR
   PRE  adjust
   action calculate-turn-angle
   arg1 current-angle
   post tapthrust)
(speedtest
  ISA OPERATOR
   PRE  test-speed
   action TEST-DIMENSION
   arg1 ship-speed
   fail speedup
   success turn)
(speedup
      isa operator
      pre speedup
      action turn-to-point
      arg1 current-angle
      post tapthrust)
(turn
  ISA OPERATOR
   PRE  turn
   action calculate-turn-angle
   arg1 future-angle
   post maketurn2)
(thrust-turn
  ISA OPERATOR
   PRE  maketurn2
   action thrust-to-turn
   post done)
(thrustaction
      isa operator
      pre tapthrust
      action tap-key
      arg1 thrust
      post done)))




(defparameter *instructions* spacetrack-instructions)


(clear-all)

(define-model SF-player
   (setf *tap-thrust* nil)
  (sgp :p-count t) 

  (sgp :er t :esc t :egs .0001 :ol t :lf .1  :ans .01 :rt -1 :v t :pct nil :trace-detail low)
  ;; Temporal module's noise
  (eval `(sgp :sf-data-hook ,*data-hook*))

;; Temporal module's noise
  (sgp :TIME-NOISE .005)
  
 ;; Motor setup, timing, and randomization 
  (sgp :randomize-time 3)
  (sgp :dual-execution-stages t)
  (sgp :MOTOR-FEATURE-PREP-TIME 0.020)
  (sgp :MOTOR-INITIATION-TIME .020)
  (sgp :hold-until-timing .025)

  ;; visual configuration 
  (sgp :test-feats nil :visual-movement-tolerance 2 :do-not-harvest visual)
 
  (chunk-type gamestate x y orientation vx vy ship-alive status game  ;common
               offaim  moving-angle current-angle future-angle r1 r2 ship-speed   ;spacetrack
                time-to-outer thrust-angle vulnerability FORTRESS-ANGLE fortress-alive outward prop-to-big speed)   ;space fortess
  (chunk-type goal step state nextstep target aim game thrust-time;common
              rectangle  offaim  press-point stop-angle ship-speed  adjusted ;spacetrack
              time-to-outer time-thresh dist-thresh vulnerability last-action outward prop-to-big speed) ;space fortess
  (chunk-type operator pre action arg1 result post  fail success)
  (chunk-type tracker rectangle death bad-aim good-aim
              hit miss badspeed goodspeed bighexdeath shelldeath smallhexdeath)
  (chunk-type mapping function key)

(define-chunks yes no speed adjust offaim turn-to-point test-dimension pressing-d pressing-key pressing-left down target-angle
                      THRUST-TO-TURNTAP-THRUST death rectangle press-point stop-angle tapthrust TAPPING-KEY pressing-a
                      CALCULATE-TURN-ANGLE maketurn2 test-speed  start done connected play start-playing-again do-step game-over
                      start-playing current-angle future-angle pressing-thrust tap-key thrust-to-turn ship-speed slow-down?
                      NOFORTRESS FORTRESS-ALIVE TESTOBJECT DELAY? CHECK-DELAY ANGLE fortress-angle  double-space-bar speed-check
                             thrust thrust-angle increment-angle decrement-angle TIME-TO-OUTER space-bar shooting vulnerability? thrusting
                             double-spacebar state-changed reset hit time-thresh bighexdeath spacebar test-object prop-to-big
                             kill  shelldeath smallhexdeath thrust-time miss badspeed goodspeed)

(add-dm 


(w isa mapping function thrust key w)
(a isa mapping function counterclockwise key a)
(d isa mapping function clockwise key d)
)
(eval (cons 'add-dm *instructions*))


(sgp :ul t  :epl t :egs .05 :alpha .2 :iu 9  )  
(sgp-fct (list :tracker-decay-method *decayMethod* :tracker-decay *decay*))

(eval `(sgp :alpha ,*alpha*))
(eval `(sgp :initial-temp ,*factor*))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; General productions for playing a SF-style game

    ;;; This production continues play at the very beginning


  (p get-started
     "Make sure game is running"
     ?game>
       game-state connected
     ?goal>
       buffer empty
     ?imaginal>
       state free
     ?game-state>
       state free
     ==>
     +goal>
         isa goal
         state start-playing
     +imaginal>
        isa tracker
     +game-state>
        isa gamestate)

    ;;; This production continues play for another game.
(p get-started-again
     "Make sure game is running"
     ?game>
       game-state connected
    =game-state>
       ship-alive yes 
       game =game
     =goal>
       state game-over
       game =game
==>
    =game-state>
      status nil
     =goal>
       state start-playing-again
       rectangle nil)

;initiates for first game of space track
(p start-playing-spacetrack
     "once the ship is visible get going"
     =goal>
       isa goal
       state start-playing 
     -  game spacetrack 
    =game-state>
        game spacetrack
==>

    !safe-bind! =minThrustTime *min-thrust-time*
    !safe-bind! =badWeight *bad-weight*
    !safe-bind! =minPressPoint *min-press-point*
     +tracker>
         control-slot aim
         good-slot rectangle
         bad-slot death
         min 3
         max 20.0
         bad-weight =badWeight
     +tracker> 
         control-slot ship-speed
         good-slot rectangle
         bad-slot death
         min 1.0
         max 4.0
         bad-weight =badWeight
     +tracker>
         control-slot press-point
         good-slot rectangle
         bad-slot death
         min =minPressPoint
         max 100.0 
         bad-weight =badWeight
     +tracker>
         control-slot stop-angle
         good-slot rectangle
         bad-slot death
         min 5
         max 40.0
         bad-weight =badWeight
     +tracker>
         control-slot thrust-time
         good-slot rectangle
         bad-slot death
         min =minThrustTime
         max 0.167
         bad-weight =badWeight
    =goal> 
        state play
        offaim 25
        game spacetrack
     )

;applies at start of game or ship death  
(p start-playing-again-spacetrack
     =goal>
       isa goal
       state start-playing-again
       game spacetrack
     =game-state>
       ship-alive yes  
        game spacetrack
       r1 =rect
     ==>
   -retrieval>
     =goal>
         state play
        rectangle =rect
     )


  (spp start-playing-spacetrack :u 100)

  (spp start-playing-again-spacetrack :u 100)

;;; This production determines game over and requires that information in game-state
 (p detect-trial-end
    "If a trial is clear things out"
    =goal>
      isa goal
    -  state game-over
    =game-state>
      status game-over
    ?manual>
      processor free
==>
    =goal>
      state game-over
      rectangle nil
      step nil
    +manual>
     isa release-all-fingers
    )
 (spp detect-trial-end :u 10000 ) 

  ;;; This production handles death
(p detect-destroyed
     =goal>
       isa goal
     - state start-playing-again
     =game-state>
       ship-alive no  
     ?manual>
       processor free
     ==>
 -retrieval>    
     =goal>
       rectangle nil
       state start-playing-again 
       step nil 
     +manual>
       isa release-all-fingers 
     )
  
  (spp detect-destroyed :u 90 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; General instruction interpretation productions
    (p do-something
       =goal>
          isa goal
          state play
      ?retrieval>
           state free
       ?MANUAL>
          state FREE
    =game-state>
        game =game 
      ==>
        =goal>
           state do-step
        +retrieval>
           isa operator
           pre =game
     )


(p test-dimension-success
   =goal>
      state do-step
      =dimension =thresh
    =retrieval>
      isa operator
      action test-dimension
      arg1 =dimension
      success =newstep
    =game-state>
        =dimension =val     
   !SAFE-EVAL! (> =val =thresh)
     ==>
    +retrieval>
        isa operator
        pre =newstep)

(p test-dimension-fail
   =goal>
      state do-step
      =dimension =thresh
    =retrieval>
      isa operator
      action test-dimension
      arg1 =dimension
      fail =newstep
    =game-state>
        =dimension =val     
   !SAFE-EVAL! (<= =val =thresh)
     ==>
    +retrieval>
        isa operator
        pre =newstep)




;productions for turning general to both games
(p turn-to-point1
   =goal>
      state do-step
    =retrieval>
      isa operator
      action turn-to-point
      arg1 =slot
      post =next
    =game-state>
      =slot =angle
     ==>
   =goal>
      nextstep =next
      state turn-to-point
       TARGET =angle)

(p skip-turn 
   =goal>
      state turn-to-point
      target =angle2
      aim =aim
      nextstep =next
      step nil
    =game-state>
      orientation =angle1
!safe-eval!   (< (angle-offset =angle2 =angle1) =aim)
     ==>
   +RETRIEVAL>
       PRE =next
=goal> 
      state do-step)

(p start-press-clockwise
   =goal>
      state turn-to-point
      target =angle2
      aim =aim
      step nil
    =game-state>
      orientation =angle1
  ?manual>
       state free 
!safe-eval!   (>= (mod (- =angle2 =angle1) 360) 180)
!safe-eval!   (>= (angle-offset =angle2 =angle1) 20)
     ==>
   +retrieval>
      isa mapping
      function clockwise
   =goal>
     step pressing-key)


(p press-clockwise-tap
   =goal>
      state turn-to-point
      target =angle2
      aim =aim
      step nil
    =game-state>
      orientation =angle1
  ?manual>
       state free 
!safe-eval!   (>= (mod (- =angle2 =angle1) 360) 180)
!safe-eval!   (>= (angle-offset =angle2 =angle1) =aim)
!safe-eval!   (< (angle-offset =angle2 =angle1) 90)
     ==>
   +retrieval>
      isa mapping
      function clockwise
   =goal>
     step tapping-key)



(p start-press-counterclockwise
   =goal>
      state turn-to-point
      target =angle2
      aim =aim
      step nil
    =game-state>
      orientation =angle1
  ?manual>
       state free 
!safe-eval! (< (mod (- =angle2 =angle1) 360) 180)
!safe-eval!   (>= (angle-offset =angle2 =angle1) 20)
     ==>
   +retrieval>
      isa mapping
      function counterclockwise
   =goal>
     step pressing-key)

(p press-counterclockwise-tap
   =goal>
      state turn-to-point
      target =angle2
      aim =aim
      step nil
    =game-state>
      orientation =angle1
  ?manual>
       state free 
!safe-eval! (< (mod (- =angle2 =angle1) 360) 180)
!safe-eval!   (>= (angle-offset =angle2 =angle1) =aim)
!safe-eval!   (< (angle-offset =angle2 =angle1) 90)
     ==>
   +retrieval>
      isa mapping
      function counterclockwise
   =goal>
     step tapping-key)



;productions that turn action intentions into specific finger actions that reflect game key binding
(p tap-key
   =goal>
      state do-step
   =RETRIEVAL>
       ACTION tap-key
       arg1 =function
     ==>
   +retrieval>
      isa mapping
      function =function
   =goal>
     state tap-key)


(p tap-w
   =goal>
      state tap-key
      thrust-time =time
      ship-speed =target
   =RETRIEVAL>
       key w
   =game-state>
       ship-speed =actual
    ?manual>
      state free
     ==>
!safe-eval! (setf *tap-thrust* (> =target =actual))
   +MANUAL>
       CMD DELAYED-PUNCH
       FINGER MIDDLE
       HAND LEFT
       delay =time
   =goal>
      nextstep nil
      target nil
      state play)


(p tappping-d
   =goal>
      step tapping-key
   =RETRIEVAL>
       key d
    ?manual>
      state free
     ==>
   =goal>
     step nil
   +MANUAL>
       CMD DELAYED-PUNCH
       FINGER index
       HAND LEFT
       delay .09)

(p pressing-d
   =goal>
      step pressing-key
   =RETRIEVAL>
       key d
    ?manual>
      state free
     ==>
   =goal>
     step pressing-d
   +MANUAL>
       CMD hold-punch
       FINGER index
       HAND LEFT)

(p finish-press-d
   =goal>
      step pressing-d
      target =angle1
      nextstep =next
   =game-state>
     orientation =angle2
   ?manual-left>
      index down
  ?manual>
       preparation free 
!safe-eval! (<= (angle-offset =angle1 =angle2) 20)
     ==>
   +RETRIEVAL>
       PRE =next
   =goal>
      state do-step
      step nil
+manual>
  step nil
  cmd release
  hand left
  finger index )

(p pressing-a
   =goal>
      step pressing-key
   =RETRIEVAL>
       key a
    ?manual>
      state free
     ==>
   =goal>
     step pressing-a
   +MANUAL>
       CMD hold-punch
       FINGER ring
       HAND LEFT)

(p finish-press-a
   =goal>
      step pressing-a
      target =angle1
      nextstep =next
   =game-state>
     orientation =angle2
   ?manual-left>
      ring down
  ?manual>
       preparation free 
!safe-eval! (<= (angle-offset =angle1 =angle2) 20)
     ==>
   +RETRIEVAL>
       PRE =next
   =goal>
      state do-step
      step nil
+manual>
  step nil
  cmd release
  hand left
  finger ring )

(p tapping-a
   =goal>
      step tapping-key
   =RETRIEVAL>
       key a
    ?manual>
      state free
     ==>
   =goal>
     step nil
   +MANUAL>
       CMD DELAYED-PUNCH
       FINGER ring
       HAND LEFT
       delay .09)




;productions specific to space track
(p calculate-turn-angle
   =goal>
      state do-step
    =retrieval>
      isa operator
      action CALCULATE-TURN-ANGLE
      arg1 =slot
      post =next
    =game-state>
      =slot =angle
      moving-angle =moving-angle
    !SAFE-BIND! =TANGLE (TURN-ANGLE =ANGLE =moving-angle)
     ==>
   =goal>
      nextstep =next
      state turn-to-point
       TARGET =TANGLE)


(p thrust-new-rectangle
   =goal>
      state do-step
     press-point =distance
   =RETRIEVAL>
       ACTION thrust-to-turn
    =game-state>
      x =x
      y =y
      vx =vx
      vy =vy
      r2 =r2
    ?manual>
      state free
!safe-eval! (> (distance-in =r2 =x =y =vx =vy) =distance)
     ==>
   +MANUAL>
       CMD hold-PUNCH
       FINGER MIDDLE
       HAND LEFT
   =goal>
      rectangle =r2
      state pressing-thrust)

(p finish-thrust
   =goal>
      state pressing-thrust
      stop-angle =stop
   =game-state>
     current-angle =angle1
     moving-angle =angle2
   ?manual-left>
      middle down
  ?manual>
       preparation free 
!safe-eval! (<= (angle-offset =angle1 =angle2) =stop)
     ==>
   =goal>
      target nil
      nextstep nil
      state play
 +manual>
      cmd release
      hand left
      finger middle )


(spp (finish-thrust) :reward 10)
(spp (GET-STARTED GET-STARTED-AGAIN start-playing-again-spacetrack
                  DETECT-DESTROYED DETECT-TRIAL-END) :reward t)


)


