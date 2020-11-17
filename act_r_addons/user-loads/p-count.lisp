(defstruct p-count items on)

(defun reset-p-count (m)
  (setf (p-count-items m) nil)
  (setf (p-count-on m) nil))

(defun create-p-count (m)
  (declare (ignore m))
  (make-p-count))

(defun params-p-count (m p)
  (if (consp p)
      (if (cdr p)
          (if (p-count-on m)
            t
            (no-output 
             (defun pcount-hook (s) 
               (when (car s)
                 (if (assoc (car s) (p-count-items m))
                     (incf (cdr (assoc (car s) (p-count-items m))))
                   (push (cons (car s) 1) (p-count-items m))))
               nil)
             (sgp :conflict-set-hook pcount-hook)
             (setf (p-count-on m) t)))
        (progn
          (setf (p-count-items m) nil)
          (when (p-count-on m)
            (no-output (sgp :conflict-set-hook (:remove pcount-hook))))
          (setf (p-count-on m) nil)))
    (p-count-on m)))

(define-module-fct :p-count nil (list (define-parameter :p-count :owner t :valid-test 'tornil :default-value nil
                                   :warning "t or nil" :documentation "record production selection count accessible using p-count"))
  :creation 'create-p-count
  :reset 'reset-p-count
  :params 'params-p-count
  :version ".6b"
  :documentation "Count production selections and report from p-count function.")

(defun p-count (p)
  (let ((m (get-module :p-count)))
    (if (p-count-p m)
        (aif (assoc p (p-count-items m))
             (cdr it)
             0)
      (print-warning "No p-count module available."))))
        
  