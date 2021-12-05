(ql:quickload "str")
(ql:quickload "split-sequence")
(ql:quickload "alexandria")

(defun get-lines (input) 
  (str:lines (str:from-file input)))

(defun parse-coords (str)
  (mapcar #'parse-integer (split-sequence:SPLIT-SEQUENCE #\, str)))

(defun straight-line? (coords)
  (destructuring-bind ((x1 y1) (x2 y2)) coords
    (or (= x1 x2) (= y1 y2))))

(defun get-coords (input)
  (let ((lines (get-lines input)))
    (loop for line in lines
          collect (mapcar 
                    #'parse-coords
                    (remove-if 
                      #'(lambda (l) (string= l "->"))
                      (split-sequence:SPLIT-SEQUENCE #\SPACE line))))))

(defun get-stright-lines (coords)
  (remove-if-not #'straight-line? coords))

(defun star-one (input)
    (let ((coords (get-stright-lines (get-coords input)))
       (hash (make-hash-table :test #'equal)))
   (labels ((add-coord-to-hash (coord)
              (if (gethash coord hash)
                  (setf (gethash coord hash) 2)
                  (setf (gethash coord hash) 1)))
            (add-y-line (x y1 y2)
              (let ((larger (if (> y1 y2) y1 y2))
                    (smaller (if (< y1 y2) y1 y2)))
                (loop for i from smaller upto larger
                      do (add-coord-to-hash (list x i)))))
            (add-x-line (y x1 x2)
              (let ((larger (if (> x1 x2) x1 x2))
                    (smaller (if (< x1 x2) x1 x2)))
                (loop for i from smaller upto larger
                      do (add-coord-to-hash (list i y))))))
     (loop for ((x1 y1) (x2 y2)) in coords 
           if (= x1 x2) do (add-y-line x1 y1 y2)
           else do (add-x-line y1 x1 x2)))
   (count 2 (alexandria:hash-table-values hash))))

(star-one "./test.lisp")
(star-one "./data.lisp")
(defun star-two (input)
    (let 
      ((coords (get-coords input))
       (hash (make-hash-table :test #'equal)))
      (flet 
          ((add-coord-to-hash (coord)
              (if (gethash coord hash)
                  (setf (gethash coord hash) 2)
                  (setf (gethash coord hash) 1)
                  )))
       (loop for ((x1 y1) (x2 y2)) in coords 
            do (let 
                   ((x x1)
                    (y y1))
                (loop while (not (and (= x x2) (= y y2)))
                      do (add-coord-to-hash (list x y))
                      if (not (= x2 x)) do (setf x (if (> x2 x) (+ x 1) (- x 1)))
                      if (not (= y2 y)) do (setf y (if (> y2 y) (+ y 1) (- y 1)))
                      finally (add-coord-to-hash (list x y))))))
             (count 2 (alexandria:hash-table-values hash))))
(star-two "./test.lisp")
(star-two "./data.lisp")
