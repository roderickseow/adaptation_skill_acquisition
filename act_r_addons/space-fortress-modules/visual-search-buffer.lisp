;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2010 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : visual-search-buffer.lisp
;;; Version     : 1.0a2
;;; 
;;; Description : Adds a module with a searchable buffer that has the chunks
;;;             : in the visicon as its buffer set.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Make it faster since now it has to recreate the buffer
;;;             :     set every conflict resolution.  Probably want to add a
;;;             :     time stamp to the vision module on changes or maybe a hook
;;;             :     or two in the proc-display/add/remove/modify code so something
;;;             :     like this knows when there's a change.
;;; 
;;; ----- History -----
;;; 2010.01.15 Dan [1.0a1]
;;;             : * Initial creation.
;;; 2010.03.25 Dan
;;;             : * Made it a no-copy buffer now too because of an issue with
;;;             :   garbage chunks created in conflict-resolution that I need
;;;             :   to address there.  
;;;             : * Also added a dummy chunk (vs-dummy) which can be used
;;;             :   in productions to overwrite the "found" chunk so that
;;;             :   things don't end up in DM and since the dummy is in the
;;;             :   buffer-set it won't be copied either.
;;; 2015.08.12 Dan [1.0a2]
;;;             : * Updates to work with ACT-R 7.
;;;             : * Actually switch to a copy buffer because the garbage chunk
;;;             :   issue has been addressed which means the vs-dummy chunk is
;;;             :   no longer needed to overwrite the chunk in the buffer to avoid
;;;             :   problems with invalid buffer set chunks.
;;; 2016.09.06 Dan
;;;             : * Added a declare to avoid a warning about an unused variable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Adds a module with a searchable buffer called visual-search.  The module
;;; will make the chunks from the visicon available to be searched by the 
;;; productions which test the visual-search buffer.  The module doesn't explicitly 
;;; put any chunks into the buffer (the production matching will implicitly put one 
;;; there when appropriate) and it will always respond to queries as being free and 
;;; without error.  It takes one request which can be used to set an optional
;;; filter (as described below).
;;;
;;; Because the search is done in the LHS production matching only the normal chunk
;;; matching applies i.e. the request parameters of :attended and :nearest are not
;;; available, the special values of current, lowest, and highest are not 
;;; valid slot values, and the value slot can only be tested for the actual value
;;; that it will have not the "original" value as a visual-location request will
;;; perform (i.e. for text a visual-location request which specifies value "a" will
;;; find the location of a letter A but the visual-search buffer would only allow
;;; a match to the value text which is what the chunk will actually contain).  
;;; If those request parameters or special values are desired then there is an 
;;; optional filter which can be applied using the set-visual-search-filter 
;;; command or through a request to the module.  If such a filter is given, then 
;;; only those chunks in the visicon which are a match to the filter using the 
;;; standard visual-location request testing are made available for the search.
;;;
;;; The request to set a filter would look like a typical request to the visual-location
;;; buffer in that it can only specify the :attended and :nearest slots at most once 
;;; each and the special slot values are usable.  All requests to this module are to
;;; set a filter -- there's no distinction like the visual-location requests have
;;; between setting the stuffing filter and making a request for a location.  If the 
;;; request is valid then that specification will be used as the filter until either 
;;; the model is reset or a new filter is provided.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; set-visual-search-filter{-fct}
;;;
;;; Works like set-visloc-default to provide a specification for visicon chunks.
;;; For this function that determines which chunks from the visicon are put into
;;; the buffer set to be searched.  The production matching searches over only
;;; those chunks which matched the filter.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Quick and dirty implementation for now as a test.  It recreates the whole
;;; buffer set every time.  The "to do" above has some thoughts on how to fix
;;; that.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; Using the goal-style-query function so make sure it's available

(require-compiled "GOAL-STYLE-MODULE" "ACT-R-support;goal-style-module")

(defstruct visual-search-module vision filter)

;; no particular constraints on the ordering

(defun visual-search-buffer-search (instance buffer)
  ;; Assume that buffer is visual-search since that's the module's only buffer.
  (let ((res nil))
    (remove-all-m-buffer-chunks buffer)
    ;; (store-m-buffer-chunk buffer 'vs-dummy)
    (dolist (x (aif (visual-search-module-filter instance)
                    (find-current-locs-with-spec (visual-search-module-vision instance) it)
                  (visicon-chunks (visual-search-module-vision instance)))
               res)
      (store-m-buffer-chunk buffer x)
      (push x res))))


;; Consider all chunks equally valid for the searching so
;; don't need an offset function, but putting this here
;; in case I want to change things later.
#|
(defun visual-search-buffer-offset (instance buffer chunks)
  (declare (ignore instance buffer chunks))
  nil)
|#

(defun visual-search-create (name)
  (declare (ignore name))
  (make-visual-search-module))


(defun visual-search-reset (instance)
  (setf (visual-search-module-filter instance) nil)
  (setf (visual-search-module-vision instance) (get-module :vision)))

(defun visual-search-request (instance buffer chunk-spec)
  (declare (ignore buffer))
  ;; any request sets the spec as long as it's only got at most one nearest and one attended
  (let ((nearest (if (slot-in-chunk-spec-p chunk-spec :nearest) 
                     (verify-single-explicit-value chunk-spec :nearest 
                                                   :visual-search 'visual-search)
                   :none))  
        (attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                      (multiple-value-bind (val valid)
                          (verify-single-explicit-value chunk-spec :attended 
                                                        :visual-search 'visual-search)
                        (declare (ignore val))

                        valid)
                    :none)))
              
              (if (or (null nearest) (null attended))
                  (print-warning "Invalid value in a request to the visual-search buffer")
                (setf (visual-search-module-filter instance) chunk-spec))))

(define-module :visual-search ((visual-search nil (:attended :nearest) nil nil :search-copy))
  nil
  :reset visual-search-reset
  :creation visual-search-create
  :query goal-style-query
  :request visual-search-request
  :search visual-search-buffer-search
  :version "1.0a2"
  :documentation "A buffer to perform limited search of the visicon on the LHS of a production."
  )

(defmacro set-visual-search-filter (&rest params)
  "Macro to set the filter specification for visual-search chunks."
  `(set-visual-search-filter-fct ',params))

(defun set-visual-search-filter-fct (params)
  "Function to set the filter specification for visual-search chunks."
  (aif (get-module :visual-search)
       (let ((spec (funcall 'define-chunk-spec-fct params)))
         (if spec
             (let ((attended (when (slot-in-chunk-spec-p spec :attended)
                               (chunk-spec-slot-spec spec :attended)))
                   (nearest (when (slot-in-chunk-spec-p spec :nearest)
                              (chunk-spec-slot-spec spec :nearest))))
               (if (or (> (length attended) 1)
                       (> (length nearest) 1))
                   (progn
                     (print-warning "The :attended and :nearest specification for a visual-search filter can only be specified at most once each.")
                     (print-warning "Visual-search filter not set."))
                 (progn 
                   (setf (visual-search-module-filter it) spec)
                   t)))
           (print-warning "Invalid chunk specification.  Visual search filter not changed.")))
       
       (print-warning "No visual-search module found.  Cannot set search filter.")))


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
                                    
