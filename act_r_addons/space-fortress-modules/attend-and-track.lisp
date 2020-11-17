;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : attend-and-track.lisp
;;; Version     : 2.0
;;; 
;;; Description : Add a new request to the vision module to allow a model to
;;;             : attend to something and start tracking it as a single action.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2015.08.11 Dan [1.0a1]
;;;             : * Added the header info when converting it to ACT-R 7.
;;; 2015.08.18 Dan
;;;             : * Changed randomize-time to randomize-time-ms.
;;; 2015.08.26 Dan
;;;             : * Need to schedule the start-tracking with a priority of 9
;;;             :   because it has to be after the visual chunk gets set, but
;;;             :   before a proc-display sneaks in because that could mess things up.
;;; 2015.09.24 Dan
;;;             : * Add the code to store the request and complete it for the
;;;             :   new request.
;;; 2016.07.20 Dan
;;;             : * Update the code that sets the current marker to use set-current-marker
;;;             :   instead of directly setting the slot.
;;;             : * Updated the code because encoding-complete needs the clof as
;;;             :   a parameter now in addition to the chunk.
;;; 2016.07.21 Dan
;;;             : * Added the clearing of clof to the reset method.
;;; 2019.04.29 Dan [2.0]
;;;             : * Updated to work with 7.13.
;;;             : * Don't redefine reset-vision-module since all it needs to do 
;;;             :   is add a chunk-type.  Instead create a dummy module with a 
;;;             :   secondary reset function to do that.
;;;             : * Don't need the special attend and encode methods either.
;;;             :   If the dummy module records when the special request is 
;;;             :   made an after method on attend-to-object can be used to
;;;             :   send the tracking request.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Adds the attend-and-track request to vision which works like move-attention
;;; except that the attended item (if there is one) is also then tracked like
;;; a start-tracking request would initiate.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; +visual>
;;;  cmd attend-and-track
;;;  screen-pos <visual-location>
;;;  {scale <scale>}
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; There isn't an easy way to "extend" the vision system so use an around method
;;; on the request method and an after on attend-to-object to catch the situation
;;; and set a flag which can signals the extra action.
;;;
;;; Makes some assumptions about vision module methods, but at some level that's
;;; going to be necessary to extend the module...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defstruct attend-and-track request)

(defun make-attend-and-track-module (name)
  (declare (ignore name))
  (make-attend-and-track))

(defun reset-attend-and-track (m)
  (declare (ignore m))
  (chunk-type (attend-and-track (:include move-attention)) (cmd attend-and-track)))

(define-module :attend-and-track nil nil
  :version "2.0"
  :documentation "Support the attend-and-track visual request."
  :reset (nil reset-attend-and-track)
  :creation make-attend-and-track-module)
  

(defmethod attend-to-object :after ((vis-mod vision-module) obj &key (requested t))
  (declare (ignorable vis-mod obj))
  (when requested
    (let ((m (get-module :attend-and-track)))
      (when (attend-and-track-request m)
        (schedule-event-relative 0 'start-tracking :module :vision :output 'medium :destination :vision :priority 9)))))
        

(defmethod pm-module-request :around ((vis-mod vision-module) buffer-name chunk-spec)
  (let ((m (get-module :attend-and-track)))
    (if (and (eq buffer-name 'visual)
           (let ((command (chunk-spec-slot-spec chunk-spec 'cmd)))
             (and (= (length command) 1)
                  (eq (spec-slot-value (first command)) 'attend-and-track))))
      
        
        (let ((sp (verify-single-explicit-value chunk-spec 'screen-pos :vision 'attend-and-track))
              (scale (if (slot-in-chunk-spec-p chunk-spec 'scale)
                         (verify-single-explicit-value chunk-spec 'scale :vision 'attend-and-track)
                       nil)))
          
          (when (visual-lock vis-mod)
            (setf (visual-lock vis-mod) nil)
            (schedule-event-now 'unlock-vision :module :vision :destination :vision
                                :priority :min :output nil :maintenance t))
          
          (if (valid-vis-loc-chunk sp vis-mod)
              (progn
                (setf (attend-and-track-request m) t)
                (schedule-event-now 'move-attention 
                                    :params (list vis-mod :scale scale :location sp)
                                    :details (concatenate 'string "Attend-and-track " (symbol-name sp)" " (symbol-name scale))
                                    :module :vision))
            (progn
              (setf (attend-and-track-request m) nil)
              (print-warning "screen-pos value ~s in an attend-and-track request was not a valid chunk" sp))))
    
    ;; Only call the original if we're not handling it here
    ;; which means that the visual-lock needs to be handled above.
    
      (progn
        (setf (attend-and-track-request m) nil)
        (call-next-method)))))

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
