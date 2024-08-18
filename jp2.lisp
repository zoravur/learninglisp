;;; JSON (JavaScript Object Notation) parser. The grammar and more info can be found here.
;;; https://datatracker.ietf.org/doc/html/rfc8259
;;; Usage: (parse (mk-scanner (uiop:read-file-string "johndoe.json")))

;;; Scanner class.
(defun mk-scanner (s) (list :stream s :cursor 0))

(defun scanner-peek (scanner) 
  (if (scanner-finished? scanner) 
    nil
    (char (getf scanner :stream) (getf scanner :cursor))))

(defun scanner-pop (scanner) 
  (let ((result (scanner-peek scanner)))
    (incf (getf scanner :cursor))
    result))

(defun scanner-finished? (scanner)
  (>= (getf scanner :cursor) (length (getf scanner :stream))))

(defun scanner-advance (n scanner)
  (let ((cur (getf scanner :cursor)))
    (setf (getf scanner :cursor) (+ cur n))
    (subseq (getf scanner :stream) cur (min (+ cur n) (length (getf scanner :stream))))))

;;; Can maybe support newlines like row:col, e.g. 5:60
(defun scanner-position (scanner)
  (getf scanner :cursor))




;;; Constants for parsing.
(defvar *onenine* (coerce "123456789" 'string))
(defvar *digit* (cons #\0 *one-to-nine*))


;;; c is either a string or a char.
(defun match (c scanner)
  (let ((s (string c)))
    (let ((s-stream (scanner-advance (length s) scanner)))
      (if (equal s s-stream) t
        (error "No match at char ~a" (scanner-position scanner))))))


(defun skip-ws (scanner)
  (loop while (member (scanner-peek scanner) (mapcar #'code-char '(#x20 #x09 #x0A #x0D)))
        do (scanner-pop scanner)))


;;; Idea: the parse-<construct> functions should return nil if the parse fails.

(defun parse-string (scanner)
  (match #\" scanner)
  (prog1 
    (coerce (loop 
              while (not (equal (scanner-peek scanner) #\"))
              collect
              (let ((chr (scanner-pop scanner)))
                (if (equal chr #\\)
                  (list chr (scanner-pop scanner))
                  chr))) 'string)
    (match #\" scanner)))

(defun parse-int (scanner)
  (parse-integer (coerce (loop while (member (scanner-peek scanner) (coerce "-1234567890" 'list)) collect (scanner-pop scanner)) 'string)))

(defun parse-array (scanner)
  (prog2
    (match "[" scanner)
    (loop
      collect (prog2 
                (skip-ws scanner)
                (parse-value scanner)
                (skip-ws scanner))
      while (and (equal (scanner-peek scanner) #\,) (match #\, scanner)))
    (skip-ws scanner)
    (match "]" scanner)))

(defun parse-object (scanner)
  (prog2
    (match "{" scanner)
    (loop
      while (progn (skip-ws scanner) (not (equal (scanner-peek scanner) #\})))
      collect (parse-kvp scanner))
    (match "}" scanner)))

(defun parse-value (scanner)
  (skip-ws scanner)
  (let ((c (scanner-peek scanner)))
    (cond
      ((equal c #\n) (progn (match "null" scanner) 'jsnull))
      ((equal c #\t) (progn (match "true" scanner) 'jstrue))
      ((equal c #\f) (progn (match "false" scanner) 'jsfalse))
      ((equal c #\[) (parse-array scanner))
      ((equal c #\") (parse-string scanner))
      ((equal c #\{) (parse-object scanner))
      ((member c (coerce "-1234567890" 'list)) (parse-int scanner)))))

(defun parse-kvp (scanner)
  (let (key val)
    (skip-ws scanner)
    (setq key (parse-string scanner))
    (skip-ws scanner)
    (match #\: scanner)
    (skip-ws scanner)
    (setq val (parse-value scanner))
    (skip-ws scanner)
    (when (equal (scanner-peek scanner) #\,) (match #\, scanner))
    (cons key val)))

(defun parse (str)
  (parse-value (mk-scanner str)))
