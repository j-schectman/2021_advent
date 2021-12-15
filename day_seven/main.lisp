(ql:quickload "str")
(ql:quickload "split-sequence")
(ql:quickload "alexandria")

(defun get-lines (input) 
  (str:lines (str:from-file input)))

(defun get-crabs (crab-str)
  (mapcar #'parse-integer (split-sequence:SPLIT-SEQUENCE #\, crab-str)))

(defun get-diff (crab num)
  (let ((diff (- num crab)))
    (if (< diff 0) (* diff -1)
        diff)))

(defun get-diff-list (crabs num)
  (mapcar #'(lambda (crab) (get-diff crab num)) crabs))

(defun average (numlist)
  (reduce #'+ numlist))

(defun get-smallest (crab1 crab2)
  (if (< (car crab1) (car crab2)) crab1 crab2))

(defun get-inc-rate (steps)
  (loop for x to steps
        sum x))

; star-one
(let ((crabs (get-crabs (first (get-lines "./data.lisp")))))
  (reduce #'get-smallest (loop for crab in crabs
         collect (cons (average (get-diff-list crabs crab)) crab))))

; star-two
(let ((crabs (get-crabs (first (get-lines "./data.lisp")))))
  (reduce 
    #'get-smallest 
    (loop for crab to (reduce #'max crabs)
          collect 
          (cons 
            (average (mapcar #'get-inc-rate (get-diff-list crabs crab))) crab))))


