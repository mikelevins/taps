(in-package :taps)

(defun %general-tails-by (sequence step)
  (let* ((len (length sequence))
         (index-series (take-until (lambda (i)(>= i len))
                                   (tap-integers :by step))))
    (map-fn t (lambda (i)(subseq sequence i))
            index-series)))

(defmethod tails ((sequence cl:null) &key by) 
  (declare (ignore sequence by))
  nil)

(defmethod tails ((sequence cl:cons) &key (by 1)) 
  (collect 'list (%general-tails-by sequence by)))

(defmethod tails ((sequence cl:vector) &key (by 1)) 
  (collect 'vector (%general-tails-by sequence by)))

(defmethod tails ((series series::foundation-series) &key (by 1)) 
  (let ((indexes (tap-integers :by by)))
    (map-fn t (lambda (i)(subseries series i))
            indexes)))

