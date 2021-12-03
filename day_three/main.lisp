(ql:quickload "str")

(defun convert_to_binary (num common?) 
  (if common? 
      (if (> num 0) 1 0)
      (if (< num 0) 1 0)))

(defun convert_to_dec (num)
  (reduce (lambda (accume val) 
            (+ (* 2 accume) val)) num
  :initial-value 0))

(defun convert_gamma (num)
  (mapcar #'(lambda (value) (convert_to_binary value T)) num))

(defun convert_epsilon (num)
  (mapcar #'(lambda (value) (convert_to_binary value nil)) num))
(convert_gamma `(1 -1 2 1))

(defun make_empty_list (size)
  (loop for x from 1 to size
        collect 0))

(defun get_lines (input)
  (str:lines (str:from-file input)))

(defun get_commons (lines)
  (let* ((bin_size (length (nth 0 lines)))
         (col (make_empty_list bin_size)))
    (loop for bin in lines
          if bin do 
          (loop for i across bin
                for x from 0 to bin_size
                do (let ((curval (nth x col)))
                     (setf (nth x col) (if (char= i #\1) (+ curval 1) (- curval 1))
                           ))))
    (cons (convert_epsilon col) 
          (cons (convert_gamma col) nil))))


(defun star_one (input)
  (reduce #'* (mapcar #'convert_to_dec (get_commons (get_lines input)))))

(star_one "./data.lisp")

(defun star_two (input)
  (let* ((lines (get_lines input))
         (epsi (get_lines input))
         (commons (get_commons lines))
         (epsi (nth 0 commons))
         (gamma (nth 1 commons)))
  (loop while (> (length lines) 1)
        do ())))
