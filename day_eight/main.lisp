(ql:quickload "str")
(ql:quickload "split-sequence")
(ql:quickload "alexandria")

(defun get-lines (input) 
  (str:lines (str:from-file input)))

(defun get-split-ends (lines)
  (loop for line in lines
        collect (split-sequence:SPLIT-SEQUENCE 
                  #\SPACE 
                  (first (last (split-sequence:SPLIT-SEQUENCE #\| line)))
                  :remove-empty-subseqs T
                  )))
(defun get-split-lines (lines)
  (loop for line in lines
        collect (mapcar #'(lambda (value) (split-sequence:SPLIT-SEQUENCE 
                   #\SPACE  value :remove-empty-subseqs T))
                   (split-sequence:SPLIT-SEQUENCE #\| line))))

(defun count-char-in-str (short-str big-str)
  (loop for x across short-str
        sum (if (find x big-str) 1 0)))                 

(defun star-one (input)
    (let* ((segment-lengths `(2 3 4 7))
        (lines (get-split-ends (get-lines input))))
   (loop for line in lines
         sum (count-if #'(lambda (st) (find (length st) segment-lengths)) line))))

(star-one "./data.txt")

(defun get-count (num str hash) 
  (let ((hash-val (gethash num hash)))
    (if (> (length str) (length hash-val)) 
        (count-char-in-str hash-val str)
        (count-char-in-str str hash-val))))

; 1 length 2
; 7 length 3
; 4 length 4
; 8 length 7
; 9 length 6 and matches all of 4
; 3 length 5 and matches all of 7
; 6 length 6 and matches 1 of 1
; 0 length 6 and matches all of 1
; 5 length 5 and matches all of 6
; 2 length 5 and matches 4 of 9last one!
(defparameter *RULES* (list (lambda (str ignore) (when (= (length str) 2) 1))
       (lambda (str ignore) (when (= (length str) 3) 7)) 
       (lambda (str ignore) (when (= (length str) 4) 4)) 
       (lambda (str ignore) (when (= (length str) 7) 8)) 
       (lambda (str hash) (when (and (= (length str) 5) (= (get-count 7 str hash) 3)) 3)) 
       (lambda (str hash) (when (and (= (length str) 6) (= (get-count 3 str hash) 5)) 9)) 
       (lambda (str hash) (when (and (= (length str) 6) (= (get-count 1 str hash) 1)) 6)) 
       (lambda (str hash) (when (and (= (length str) 6) (= (get-count 1 str hash) 2)) 0)) 
       (lambda (str hash) (when (and (= (length str) 5) (= (get-count 6 str hash) 5)) 5)) 
       (lambda (str hash) (when (and (= (length str) 5) (= (get-count 9 str hash) 4)) 2))))

(defun make-decode-hash (raw-values)
      (let ((hash (make-hash-table))
        (lazy (make-hash-table :test #'equal)))
        (loop for rule in *RULES*
              do (loop for raw-str in raw-values
                       when (funcall rule raw-str hash) 
                       do (let 
                              ((res (funcall rule raw-str hash)))
                            ; only do all the stuff if string is not used
                            (when (not (gethash (sort raw-str #'char-lessp) lazy))
                            (setf (gethash res hash) raw-str)
                            (setf (gethash (sort raw-str #'char-lessp) lazy) res)
                            (print res)
                            (return))))) lazy))

(defun decode (key-vals vals)
  (let ((decoded (make-decode-hash key-vals)))
    (loop for to-decode in vals
        for x from 3 downto 0 
        sum (* (expt 10 x) (gethash (sort to-decode #'char-lessp) decoded)))))

; star-two
(let ((vals (get-split-lines (get-lines "./data.txt"))))
  (loop for (keys-to-decode values-to-decode) in vals
        do (print (decode keys-to-decode values-to-decode))
        do (print values-to-decode)
        sum (decode keys-to-decode values-to-decode)))

