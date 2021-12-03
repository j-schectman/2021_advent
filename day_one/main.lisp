(load "data.lisp")
(defun star_one (the_data)
  (let ((last nil))
    (loop for x in the_data
          if (and last (> x last)) sum 1
          do (setf last x))))

(defun star_two (the_data range)
  (star_one (let ((the_list (list)))
             (loop for x in the_data
                   if (= (length the_list) range) collect (reduce '+ the_list)
                   do (if (= (length the_list) range) (setf the_list (butlast the_list)))
                   do (setf the_list (cons x the_list))))))

(star_two data 3)
