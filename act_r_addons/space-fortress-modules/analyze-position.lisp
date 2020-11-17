;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2015 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : analyze-position.lisp
;;; Version     : 1.0
;;; 
;;; Description : Determine whether the current ship features represent a good
;;;             : or bad situation for orbiting the fortress with respect to 
;;;             : either thrusting or turning.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2015.10.02 Dan [1.0]
;;;             : * Collect some code from other files here for consistency.
;;; 2015.10.05 Dan
;;;             : * Finish up the code to handle the time-to-act calculations.
;;; 2015.10.12 Dan
;;;             : * Added a safety check to will-it-shoot-fortress for when
;;;             :   the ship would be inside the fortress itself.  Say no if
;;;             :   it passes the inner hex distance.
;;;             : * Deal with possible rounding errors to avoid complex numbers
;;;             :   in thrust-time.
;;; 2016.09.06 Dan
;;;             : * Added a declare to avoid an unused variable warning.
;;;             : * Added separate functions for the travel time & distance to
;;;             :   the hexes and a raw distance calculation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Using the data from the game compute a metric for turning and thrusting
;;; action evaluation.  The current approach is to return a time in seconds 
;;; before the next action would be necessary.  For thrusting that is the time
;;; until either the ship would hit one of the hexagons with its current velocity
;;; or until the fortress would shoot if that's sooner (always 1 second based on
;;; whether the ship is traveling at an angular velocity of less than 10 degrees
;;; per second).  For turning it is the time at which the ship will no longer
;;; be aiming at the fortress given its current velocity minus the time it takes
;;; to actually be aimed at the fortress from its current point.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; thrust-time: dist vdir speed sx sy vx vy &key (ohr 200) (inr 40) (fx 355) (fy 315)
;;; angle-time: sx sy vx vy orientation &key (fx 355) (fy 315)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Depends on the deg->rad and rad->deg functions from the ACT-R code.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defun thrust-time (dist vdir speed sx sy vx vy &key (ohr 200) (ihr 40) (fx 355) (fy 315))
  (if (zerop speed)
      1.0
    ;; use law of sins and law of cosines to compute
    ;; the angular velocity in degrees per second
    ;; could also use the velocity deltas along with ship and
    ;; fortress positions (which might be faster but I'm not
    ;; going to work out the math on that unless this code
    ;; really seems to drag things down).
    
    ;; Using sides a,b,c and angles aA, bB, and cC to keep
    ;; the equations straight with a = distance, b = travel
    ;; distance over 1 second, and cC being vdir (the rest
    ;; have to be computed):
    ;; 
    ;; c^2 = a^2 + b^2 - 2ab*cos(cC)
    ;; b/sin(bB) = c/sin(cC)
    ;;
    ;; and bB is what we need
    ;;
    ;; 
    (let* ((vy (- vy)) ;; need to flip it since it's not "right"
                       ;; because things are in screen coords but ship movement
                       ;; is in cartesian...
           (speed-per-sec (* speed 31)) ;; convert speed to pix/sec
           (b speed-per-sec) ;; distance traveled in 1 second
           (a dist)  
           (cC (deg->rad vdir))
           (c (sqrt (+ (* a a) (* b b) (* -2 a b (cos cC)))))
           (bB (asin (min 1.0 (/ (* b (sin cC)) c))))
           (shoot-time (if (<= (rad->deg bB) 10.0) 1.0 9999))
      
           ;; Now need to determine time to hit hexagons
           
      ;; Can only intersect one big hexagon since on the inside
           (outer-time (let ((h1x (- fx ohr))
                             (h2x (- fx (/ ohr 2)))
                             (h3x (+ fx (/ ohr 2)))
                             (h4x (+ fx ohr))
                             (h1y (- fy (* ohr (/ (sqrt 3) 2))))
                             (h2y (+ fy (* ohr (/ (sqrt 3) 2)))))
                         (dolist (x (list (list h1x fy h2x h1y)
                                          (list h2x h1y h3x h1y)
                                          (list h3x h1y h4x fy)
                                          (list h4x fy h3x h2y)
                                          (list h3x h2y h2x h2y)
                                          (list h2x h2y h1x fy))
                                    0 ;; it could be "outside" because of rounding/float
                                    ;; issues in which case none would intersect so
                                    ;; consider that a bad situation
                                    )
                           (awhen (intersecting-segments sx sy (+ sx (* 10000 vx)) (+ sy (* 10000 vy))
                                                         (first x) (second x) (third x) (fourth x))
                                  (return (/ (* (dist (list sx sy) it)) speed-per-sec))))))
           ;; may hit more than one inner or none
           (inner-time (let ((h1x (- fx ihr))
                             (h2x (- fx (/ ihr 2)))
                             (h3x (+ fx (/ ihr 2)))
                             (h4x (+ fx ihr))
                             (h1y (- fy (* ihr (/ (sqrt 3) 2))))
                             (h2y (+ fy (* ihr (/ (sqrt 3) 2)))))
                         (apply 'min (mapcar (lambda (x)
                                               (aif (intersecting-segments sx sy (+ sx (* 10000 vx)) (+ sy (* 10000 vy))
                                                                           (first x) (second x) (third x) (fourth x))
                                                    (/ (dist (list sx sy) it) speed-per-sec)
                                                    9999))
                                       
                                       (list (list h1x fy h2x h1y)
                                             (list h2x h1y h3x h1y)
                                             (list h3x h1y h4x fy)
                                             (list h4x fy h3x h2y)
                                             (list h3x h2y h2x h2y)
                                             (list h2x h2y h1x fy)))))))
      
      (min shoot-time inner-time outer-time))))


(defun travel-dist-to-hex (speed sx sy vx vy &key radius (fx 355) (fy 315))
  (if (zerop speed)
      9999
    (let ((vy (- vy))
          (h1x (- fx radius))
          (h2x (- fx (/ radius 2)))
          (h3x (+ fx (/ radius 2)))
          (h4x (+ fx radius))
          (h1y (- fy (* radius (/ (sqrt 3) 2))))
          (h2y (+ fy (* radius (/ (sqrt 3) 2)))))
      ;; could hit more than 1 (moving toward inner or somehow outside of outer)
      ;; or possibly none (outside moving away or rounding issues when very close)
      ;; provide the 9999 value when that happens
      (apply 'min (mapcar (lambda (x)
                            (aif (intersecting-segments sx sy (+ sx (* 10000 vx)) (+ sy (* 10000 vy))
                                                        (first x) (second x) (third x) (fourth x))
                                 (dist (list sx sy) it)
                                 9999))
                    (list (list h1x fy h2x h1y)
                          (list h2x h1y h3x h1y)
                          (list h3x h1y h4x fy)
                          (list h4x fy h3x h2y)
                          (list h3x h2y h2x h2y)
                          (list h2x h2y h1x fy)))))))


;; Equation from https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line

(defun distance-point-to-2-point-line (x0 y0 p1 p2)
  (let ((denom (dist p1 p2))
        (x1 (first p1))
        (y1 (second p1))
        (x2 (first p2))
        (y2 (second p2)))
    (/ (abs (+ (- (* x0 (- y2 y1)) (* y0 (- x2 x1))) (- (* x2 y1) (* y2 x1))))
       denom)))


(defun dist-to-hex (sx sy &key radius (fx 355) (fy 315))
  (let* ((angle (atan (- fy sy) (- fx sx)))
         (ship-dist (dist (list sx sy) (list fx fy)))
         (h1x (- fx radius))
         (h2x (- fx (/ radius 2)))
         (h3x (+ fx (/ radius 2)))
         (h4x (+ fx radius))
         (h1y (- fy (* radius (/ (sqrt 3) 2))))
         (h2y (+ fy (* radius (/ (sqrt 3) 2))))
         (segment (cond ((<= 0 angle (/ pi 3)) (list h1x fy h2x h1y))
                        ((<= (/ pi 3) angle (* pi 2/3)) (list h2x h1y h3x h1y))
                        ((<= (* pi 2/3) angle pi) (list h3x h1y h4x fy))
                        ((>= 0 angle (/ pi -3)) (list h1x fy h2x h2y))
                        ((>= (/ pi -3) angle (* pi -2/3)) (list h2x h2y h3x h2y))
                        (t (list h3x h2y h4x fy)))))
    
    (if (<= ship-dist radius)
        ;; inside, or at least not outside the corners
        ;; can compute perpendicular to the corresponding segment
        (distance-point-to-2-point-line sx sy (list (first segment) (second segment))
                                        (list (third segment) (fourth segment)))
      
      ;; treat the hex like a circle to approximate the
      ;; distance...
      (- ship-dist radius))))
      


(defun travel-time-to-hex (speed sx sy vx vy &key radius (fx 355) (fy 315))
  (if (zerop speed)
      9999
    (let ((d (travel-dist-to-hex speed sx sy vx vy :radius radius :fx fx :fy fy))
          (pixels-per-sec (* speed (/ 1.0 .033))))
      (if (= d 9999) d (/ d pixels-per-sec)))))




;;; Function for determining the intersection of two line segments
;;; adapted from code found here:
;;; http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
;;; instead of deriving it myself.
;;; If they intersect return the list of x,y where it happens 
;;; otherwise return nil.
;;; If the lines are overlapping return the first point on p0->p1
;;; which overlaps q0,q1.

(defun intersecting-segments (p0x p0y p1x p1y q0x q0y q1x q1y)
  (let* ((rx (- p1x p0x))
         (ry (- p1y p0y))
         (sx (- q1x q0x))
         (sy (- q1y q0y))
         (denom (- (* rx sy) (* sx ry)))
         
         (t-num (- (* sy (- q0x p0x)) (* sx (- q0y p0y))))
         (u-num (- (* ry (- q0x p0x)) (* rx (- q0y p0y)))))
    
    (cond ((and (zerop denom)
                (zerop u-num))
           (cond ((and (or (<= q0x p0x q1x)   ;; p0 is between q0 and q1
                           (>= q0x p0x q1x))
                       (or (<= q0y p0y q1y)
                           (>= q0y p0y q1y)))
                  (list p0x p0y))
                 ((and (or (<= q0x p1x q1x)   ;; p1 is between q0 and q1
                           (>= q0x p1x q1x))
                       (or (<= q0y p1y q1y)
                           (>= q0y p1y q1y)))
                  (if (or (and (<= p0x q0x p1x)   ;; determine which end of q between
                               (<= p0y q0y p1y))  ;; p0 and p1
                          (and (>= p0x q0x p1x)
                               (>= p0y q0y p1y)))
                      (list q0x q0y)
                    (list q1x q1y)))
                 ((and (= (signum (- p0x q0x)) (signum (- p1x q0x)))  ;; p0 and p1 on same
                       (= (signum (- p0y q0y)) (signum (- p1y q0y)))) ;; side of q0
                  nil)
                 (t ;; q isa subset of p
                  (if (and (<= (abs (- p0x q0x)) (abs (- p0x q1x)))
                           (<= (abs (- p0y q0y)) (abs (- p0y q1y))))
                      (list q0x q0y)
                    (list q1x q1y)))))
          ((zerop denom)
           nil)
          ((and (<= 0 (/ t-num denom) 1) (<= 0 (/ u-num denom) 1))
           (let ((t-val (/ t-num denom)))
             (list (+ p0x (* t-val rx)) (+ p0y (* t-val ry)))))
          (t nil))))

(defun angle-time (sx sy vx vy orientation &key (fx 355) (fy 315) (fwidth 18))
  ;; remember that need to invert the y calculations for angles
  ;; since it's backwards with respect to coordinates
  ;; and the vy value needs to be inverted too...
  (let ((aim-count 0)
        (delayed-aim 0))
  (multiple-value-bind (initial-hit start-angle) (will-it-shoot-fortress sx sy orientation fx fy fwidth)
    (if initial-hit
        (do ((nx (+ sx vx) (+ nx vx))
             (ny (- sy vy) (- ny vy)))
            ((null (will-it-shoot-fortress nx ny orientation fx fy fwidth)) (* .033 aim-count))
          (incf aim-count))
      (multiple-value-bind (second-hit second-angle) (will-it-shoot-fortress (+ sx vx) (- sy vy) orientation fx fy fwidth)
        (declare (ignore second-hit))
        (if (< second-angle start-angle)
            (progn ;; will hit it at some point
              ;; compute time until hitting
              (do ((nx (+ sx vx) (+ nx vx))
                   (ny (- sy vy) (- ny vy)))
                  ((will-it-shoot-fortress nx ny orientation fx fy fwidth) (setf sx nx) (setf sy ny))
                (incf delayed-aim))
              ;; compute time until not hitting
              (do ((nx (+ sx vx) (+ nx vx))
                   (ny (- sy vy) (- ny vy)))
                  ((null (will-it-shoot-fortress nx ny orientation fx fy fwidth)))
                (incf aim-count))
              (* .033 (- aim-count delayed-aim)))
          0))))))  
  
(defun will-it-shoot-fortress (sx sy orientation fx fy fwidth &key (ihw 40))  
  (let* ((angle-from-ship-to-fortress (rad->deg (atan (- sy fy) (- fx sx))))
         (aim-angle-to-fort-center (- orientation angle-from-ship-to-fortress))
         (adjusted-angle (if (> aim-angle-to-fort-center 180)
                             (- aim-angle-to-fort-center 360)
                           (if (< aim-angle-to-fort-center -180)
                               (+ aim-angle-to-fort-center 360)
                             aim-angle-to-fort-center)))
         (dist-to-fort (dist (list sx sy) (list fx fy)))
         (hit-angle (rad->deg (asin (/ fwidth dist-to-fort)))))
    (values (and (> dist-to-fort ihw) (<= (abs adjusted-angle) hit-angle))
            (abs adjusted-angle))))
         


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#