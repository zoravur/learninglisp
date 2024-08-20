(defun range (lo hi)
  (let ((i lo))
    (lambda ()
      (if (< i hi)
      (values (prog1 i (setf i (1+ i))) nil)
      (values nil t)))))

(defun take (iterator n)
  (let ((next (range 0 n)))
    (lambda () 
      (values (funcall iterator) (nth-value 1 (funcall next))))))

(defun forever (val)
  (lambda () (values val nil)))

(defun repeat (val n)
  (take (forever val) n))

(defun iter-to-list (iterator)
  (multiple-value-bind (val stopiter) (funcall iterator)
    (if stopiter
      nil
      (cons val (iter-to-list iterator)))))

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

;(defun clone (iterator)
;  (let ((cp iterator)
;        (results '())
;        (len 0)

;;; this is just for testing, make it as big as 
;;; necessary for all practical purposes
(defun counter ()
  (lambda () (range 0 1000000)))

(defun shared-counter (counter)
  (lambda ()



(defun comb-2-way (i j)
  (chain (map-iter (lambda (x) (map-iter (lambda (y) (list x y)) j)) i)))
; (chain (map-iter (lambda (x) (map-iter (lambda (y) (list x y)) it2)) it1)))

; (chain (map-iter (lambda (x) (map-iter (lambda (y) (list x y)) it2)) it1)))

;(defvar it1 (range 0 5))
;(defvar it2 (range 5 10))
;(defvar b (chain (map-iter (lambda (x) (map-iter (lambda (y) (list x y)) it1)) it2)))
;(defvar c (chain (map-iter (lambda (x) (map-iter (lambda (y) (list x y)) (forever it1))) it2)))
;(defun iter-cells ()
;  (let* ((letter #\a)
;         (num 0) 
;         (letters (list)) 
;         (nums (list))
;         (l-stub letters)
;         (n-stub nums))
;    (lambda ()
;      (if (not (equal (car letters) #\a))
;        (progn (push #\a letters) (nums 



