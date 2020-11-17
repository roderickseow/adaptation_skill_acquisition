
#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GOAL-STYLE-MODULE" "ACT-R-support:goal-style-module")

(defun game-state-reset (module)
  (declare (ignore module))
  (sgp :do-not-harvest game-state)
  (specify-compilation-buffer-type game-state perceptual))

(define-module game-state
  (game-state)
  nil
  :version "1.1"
  :documentation "Simple game state module"
  :query goal-style-query
  :request goal-style-request
  :reset (nil game-state-reset)
  :buffer-mod goal-style-mod-request) 
