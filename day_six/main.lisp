(load "./test.lisp")
(load "./data.lisp")

(defun advance (fishes) 
  (loop for fish in fishes
        if (= fish 0) collect 6
        and collect 8
        else collect (- fish 1)))

(defun advance-till (fishes n)
  (if (> n 0) (advance-till (advance fishes) (- n 1)) fishes))

; Star-One answer
(length (advance-till data-fish 80))

(defun make-fish-array (fishes)
  (let ((arr (make-array 9)))
    (loop for fish in fishes
          do (setf (aref arr fish) (+ 1 (aref arr fish))))
    arr))

(defun list-simulate (fish-list generations)
  (if (> generations 1) 
      (let ((next-list (make-array 9)))
        (labels 
            ((update-fish-count (index fish-count)
               (setf (aref next-list index) (+ fish-count (aref next-list index))))
             (add-to-next-list (index fish-count)
               (if (= index 0) 
                   (progn
                     (update-fish-count 8 fish-count)
                     (update-fish-count 6 fish-count))
                   ; else
                   (update-fish-count (- index 1) fish-count))))
          (loop for fish across fish-list
                for i from 0 to 8
                do (add-to-next-list i fish))
          (list-simulate next-list (- generations 1))))
      fish-list))

(defun simulate-fish (fishes generations)
  (reduce #'+ (list-simulate (make-fish-array fishes) generations)))

(simulate-fish test-fish 80)
(simulate-fish test-fish 256)
; Star-Two answer
(simulate-fish data-fish 256)
