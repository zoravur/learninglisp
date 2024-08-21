(defun range (lo hi)
  (let ((i lo))
    (lambda ()
      (if (< i hi)
      (values (prog1 i (setf i (1+ i))) nil)
      (values nil t)))))

(defun take (iterator n)
  (let ((next (range 0 n)))
    (lambda () 
      (multiple-value-bind (value stopiter) (funcall iterator)
        (values value (or (nth-value 1 (funcall next)) stopiter))))))

(defun forever (val)
  (lambda () (values val nil)))

(defun repeat (val n)
  (take (forever val) n))

;;; stop-p should be a predicate either returning t or nil depending 
;;; on whether it is time to stop the iterator.
(defun take-while (iterator stop-p)
  (lambda () (multiple-value-bind (value stopiter) (funcall iterator)
               (values value (or (funcall stop-p value) stopiter)))))

;;;
(defun iter-read ()
  (take-while (lambda () (read *standard-input* nil :eof)) (lambda (result) (equal result :eof))))

(defun iter-read-simple ()
  (lambda ()
    (let ((result (read *standard-input* nil :eof)))
      (values result (equal result :eof)))))

(defun iter-to-list (iterator)
  (multiple-value-bind (val stopiter) (funcall iterator)
    (if stopiter
      nil
      (cons val (iter-to-list iterator)))))

(defun empty (iterator)
  (loop while (not (nth-value 1 (funcall iterator)))))

(defun list-to-iter (lst)
  (let ((cur lst))
    (lambda ()
      (if cur
        (prog1
          (values (car cur) nil) 
          (setf cur (cdr cur)))
        (values nil t)))))

(defun map-iter (f iterator)
  (lambda () 
    (multiple-value-bind (val stopiter) (funcall iterator)
      (if stopiter
        (values nil t)
        (values (funcall f val) nil)))))

(defun iter-empty ()
  (lambda () (values nil t)))

;;; Take a list of iterators and yield from them in sequence.
(defun flatten (iter-list)
  (let ((lst iter-list))
    (labels 
      ((next ()
        (if lst 
          (multiple-value-bind (value stop) (funcall (car lst)) 
            (if stop 
              (progn (pop lst) 
                     (next)) 
              (values value stop)))
          (values nil t))))
      (lambda () (next)))))

;;; Flatten an iterator of iterators into a single iterator.
(defun chain (iterator)
  (let ((current (iter-empty)))
    (labels ((next ()
               (multiple-value-bind (value stop) (funcall current)
                 (if stop
                   ; call next on the iterator to see if it is empty.
                   ; if it's not, set the current iterator equal to
                   ; the result, and then call (next) again.
                   ; Otherwise, if the iterator IS empty, 
                   ; signal the end of chain2
                   (multiple-value-bind (cur stopglobal) (funcall iterator)
                     (if stopglobal (values nil t)
                       (progn (setf current cur)
                              (next))))
                   (values value stop)))))
      (lambda () (next)))))


;(defvar *results* (list nil))

(defun split-cons (cons-cell) (values (car cons-cell) (cdr cons-cell)))

(defun make-cloner (iterator)
  (let ((results-for-iterator (list nil)))
    (labels ((clone ()
      (let ((cp iterator)
            (results results-for-iterator))
        (lambda ()
          (let ((elemlist (cdr results)))
            (if elemlist 
              (multiple-value-prog1 (split-cons (car elemlist)) (setf results elemlist))
              (progn (nconc results (list (multiple-value-call #'cons (funcall cp)))) 
                     (setf results (cdr results)) 
                     (split-cons (car results)))))))))
      #'clone)))

;(defun clone (iterator)
;  (let ((cp iterator)
;        (results (list nil)))
;    (lambda ()
;      (let ((elemlist (cdr results)))
;        (if elemlist (multiple-value-prog1 (split-cons (car elemlist)) (setf results elemlist))
;          (progn (nconc results (list (multiple-value-call #'cons (funcall cp)))) (setf results (cdr results)) (split-cons (car results))))))))

;;; this is just for testing, make it as big as 
;;; necessary for all practical purposes
(defun counter ()
  (map-iter (lambda (i) (print i) i) (range 0 5)))

;;; This is a shared counter. It takes reference to some iterator object as another iterator,
;;; and increments that iterator and returns the value
(defun shared-counter (other)
  (lambda () (funcall other)))



;(defun comb-2-way (i j)
;  (let ((r *results*)) (chain (map-iter (lambda (x) (map-iter (lambda (y) (list x y)) (clone j r))) i))))

(defun comb-2-way (i j)
  (let ((clone-j (make-cloner j))) (chain (map-iter (lambda (x) (map-iter (lambda (y) (cons x y)) (funcall clone-j))) i))))

(defun comb-n-way (list-of-iters)
  (reduce #'comb-2-way list-of-iters :from-end t :initial-value (repeat nil 1)))

