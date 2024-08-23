(defun range (lo hi)
  (let ((i lo))
    (lambda ()
      (if (or (< i hi) (equal hi nil))
          (values (prog1 i (setf i (1+ i))) nil)
          (values nil t)))))

(defun counter (start)
  (let ((i start))
    (lambda ()
      (values (prog1 i (setf i (1+ i))) nil))))

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

(defun empty-iter (iterator)
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

;(defmacro make-cloner (iterator-call)
;  `(lambda () (iterator-call))) 

;(defun clone (iterator)
;  (let ((cp iterator)
;        (results (list nil)))
;    (lambda ()
;      (let ((elemlist (cdr results)))
;        (if elemlist (multiple-value-prog1 (split-cons (car elemlist)) (setf results elemlist))
;          (progn (nconc results (list (multiple-value-call #'cons (funcall cp)))) (setf results (cdr results)) (split-cons (car results))))))))

;;; this is just for testing, make it as big as 
;;; necessary for all practical purposes
;(defun counter ()
;  (map-iter (lambda (i) (print i) i) (range 0 5)))

;;; This is a shared counter. It takes reference to some iterator object as another iterator,
;;; and increments that iterator and returns the value
(defun shared-counter (other)
  (lambda () (funcall other)))

(defun comb-2-way (i j)
  (let ((clone-j (make-cloner j))) (chain (map-iter (lambda (x) (map-iter (lambda (y) (cons x y)) (funcall clone-j))) i))))

(defun comb-n-way (list-of-iters)
  (reduce #'comb-2-way list-of-iters :from-end t :initial-value (repeat nil 1)))

(defun zip (i j)
  (lambda () 
    (multiple-value-bind (x stopi) (funcall i)
      (multiple-value-bind (y stopj) (funcall j)
        (values (cons x y) (or stopi stopj))))))

(defun accumulate (iterator)
  (let ((clone-it (make-cloner iterator)))
    (map-iter (lambda (n) (take (funcall clone-it) n)) (counter 0))))

(defun accumulate-rev (iterator)
  (let ((results nil))
    (map-iter (lambda (x) (push x results) (list-to-iter results)) iterator)))

#|

Idea: Do the triangle trick in order to read off every element in an infinite cartesian product,
like, (1, 1) (2, 1) (1, 2) (3, 1), (2, 2), (1, 3), and so on.

In order to do this, we need to iterate x values in reverse, going from highest down to lowest. 
We also need to iterate the y values forward, going from lowest to highest.

The latter is easy, (chain (accumulate y))

Iterating backwards is interesting -- all we need to do though is just create a list, and push to it.
Then call (iter-to-list), which will iterate the list forward. Once we have the two iterators, we zip
them, and we're done.

Let's look at the backward iterator in more detail -- I don't want to use a generic reversed function,
as then it will be O(x^2) each x.

Okay, I wrapped the functionality in accumulate-rev. The rest should be easy.

|#

(defun comb-2-way-infinite (i j)
  (zip
    (chain (accumulate-rev i))
    (chain (accumulate j))))

(defun comb-n-way-infinite (iterators)
  (reduce #'comb-2-way-infinite iterators :from-end t :initial-value (repeat nil 1)))

(defun comb-2-way-inf-fast (i j limit)
  (let ((i-rev (list (funcall i)))
        (j-fwd (list (funcall j)))
        (j-ref nil)
        (i-ref nil)
        (cnt 0))
    (loop for n from 1
          do (progn (setf i-ref i-rev) 
                    (setf j-ref j-fwd)
                    (push (funcall i) i-rev)
                    (loop for k from 0 to n do
                          (when (= k (1- n)) (nconc j-ref (list (funcall j))))
                          (cons (car i-ref) (car j-ref))
                          (incf cnt)
                          (when (>= cnt limit) (return))
                          (pop i-ref) (pop j-ref))))))



