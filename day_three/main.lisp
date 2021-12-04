(ql:quickload "str")

(defun get-lines (input) 
  (str:lines (str:from-file input)))

(defun convert-to-binary (num common?) 
  (if common? 
      (if (>= num 0) 1 0)
      (if (<= num 0) 1 0)))

(defun convert-to-dec (num)
  (reduce (lambda (accume val) 
            (+ (* 2 accume) val)) num
  :initial-value 0))

(defun make-empty-list (size)
  (loop for x from 1 to size
        collect 0))

(defun convert-gamma (num)
  (mapcar #'(lambda (value) (convert-to-binary value T)) num))

(defun convert-epsilon (num)
  (mapcar #'(lambda (value) (convert-to-binary value nil)) num))

(defun get-commons (lines)
  (let* ((bin-size (length (nth 0 lines)))
         (col (make-empty-list bin-size)))
    (loop for bin in lines
          if bin do 
          (loop for i in bin
                for x from 0 to bin-size
                do (let ((curval (nth x col)))
                     (setf (nth x col) (if (= i 1) (+ curval 1) (- curval 1))
                           ))))
    (print col)
    (cons (convert-epsilon col) 
          (cons (convert-gamma col) nil))))

(defun convert-to-list (string-binary)
  (map 'list #'(lambda (c) (digit-char-p c)) string-binary))

(defun star-one (input)
  (reduce #'* (mapcar
      #'convert-to-dec 
      (get-commons (mapcar #'convert-to-list (get-lines input))))))

(defun partition (fun lst)
  (loop for item in lst
        if (funcall fun item) collect item into right
        else collect item into left
        finally (return (list right left))))

(defun partition-most-common (lst comparer &optional (index 0))
  (let* ((part (partition #'(lambda (v) (= (nth index v) 0)) lst))
         (right (first part))
         (left (car (last part)))
         (common (if (funcall comparer (length left) (length right)) left right)))
    (if (<= (length common) 2) common
        (partition-most-common common comparer (+ 1 index)))))

(defun star-two (input)
  (let* ((lines (mapcar #'convert-to-list (get-lines input)))
         (most-common (reduce #'max 
                              (mapcar #'convert-to-dec 
                                      (partition-most-common lines #'>))))
         (least-common (reduce #'min 
                               (mapcar #'convert-to-dec 
                                       (partition-most-common lines #'<)))))
    (* most-common least-common)))
    

(star-one "./data.lisp")
(star-two "data.lisp")
