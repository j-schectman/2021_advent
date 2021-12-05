(ql:quickload "str")
(ql:quickload "split-sequence")
(ql:quickload "alexandria")

(defun get-lines (input) 
  (str:lines (str:from-file input)))

(defun partition (lines num) 
  (let ((current (list)))
    (loop for l in lines
          do (setf current (append current (list l)))
          if (= (length current) num) collect current
          and do (setf current (list))
          )))

(defun get-matri (lines)
  (partition (loop for v in lines 
         if (not (equal v "")) 
         collect (mapcar #'parse-integer (remove-if #'(lambda (x) (equal x "")) (split-sequence:SPLIT-SEQUENCE #\SPACE v)))
         ) 5))
(defun make-test-hash (lstwners)
  (let ((hash (make-hash-table)))
    (loop for i in lstwners
          do (setf (gethash i hash) T))
    hash))

(defun run-bingo (matri winning-numbers size)
  (let ((hash (make-hash-table)))
    (labels ((is-winning-cell (matrix cell)
               (let ((x (car cell)) (y (cdr cell)))
                 (gethash (nth x (nth y matrix)) hash)))
             (is-winning-column (matrix col size)
               (let ((y 0))
                 (loop while (and (< y size) (is-winning-cell matrix (cons col y)))
                       do (setf y (+ y 1)))
                 (and (= y size) (is-winning-cell matrix (cons col (- y 1))))))
             (is-winning-row (matrix row size)
               (let ((x 0))
                 (loop while (and (< x size) (is-winning-cell matrix (cons x row)))
                       do (setf x (+ x 1)))
                 (progn
                   (and (= x size) (is-winning-cell matrix (cons (- x 1) row)))))))
             (cons hash (loop named outer for i in winning-numbers 
               do (setf (gethash i hash) T)
               do (loop for mat in matri
                        do (loop for x to size
                                 if (is-winning-column mat x size) do (return-from outer (cons i mat)))
                        do (loop for y to size
                                 if (is-winning-row mat y size) do (return-from outer (cons i mat)))))))))

(let* ((vals (get-lines "./data.lisp"))
       (matri (get-matri (cdr vals)))
       (winning-numbers (mapcar #'parse-integer (split-sequence:SPLIT-SEQUENCE #\, (car vals))))
       (results (run-bingo matri winning-numbers 5))
       (hash (car results))
       (win (alexandria:flatten (cdr (cdr results))))
       (final (remove-if (lambda (value) (gethash value hash)) win)))
  (* (reduce #'+ final)(car (cdr results))))

(defun run-bingo-2 (matri winning-numbers size)
  (let ((hash (make-hash-table))
        (last-win nil)
        (mil-met (list)))
    (labels ((is-winning-cell (matrix cell)
               (let ((x (car cell)) (y (cdr cell)))
                 (gethash (nth x (nth y matrix)) hash)))

             (is-winning-column (matrix col size)
               (let ((y 0))
                 (loop while (and (< y size) (is-winning-cell matrix (cons col y)))
                       do (setf y (+ y 1)))
                 (progn
                  (and (= y size) (is-winning-cell matrix (cons col (- y 1)))))))

             (is-winning-row (matrix row size)
               (let ((x 0))
                 (loop while (and (< x size) (is-winning-cell matrix (cons x row)))
                       do (setf x (+ x 1)))
                 (progn
                   (and (= x size) (is-winning-cell matrix (cons (- x 1) row)))))))
      (loop for i in winning-numbers 
            do (setf (gethash i hash) T)
            do (loop for mat in matri
                     for mat-i upto (length matri)
                     if (not (find mat-i mil-met))
                     do (loop for x to size
                              if (is-winning-column mat x size) 
                              do (progn 
                                   (setf mil-met (append mil-met (list mat-i)))
                                   (setf last-win (cons (alexandria:hash-table-keys hash) (cons i mat)))
                                   (return)
                                   ))
                     and
                     do (loop for y to size
                              if (is-winning-row mat y size)
                              do (progn
                                   (setf mil-met (append mil-met (list mat-i)))
                                   (setf last-win (cons (alexandria:hash-table-keys hash) (cons i mat)))
                                   (return)
                                   )))))
    last-win))

(let* ((vals (get-lines "./data.lisp"))
       (matri (get-matri (cdr vals)))
       (winning-numbers (mapcar #'parse-integer (split-sequence:SPLIT-SEQUENCE #\, (car vals))))
       (results (run-bingo-2 matri winning-numbers 5))
       (hash (car results))
       (win (alexandria:flatten (cdr (cdr results))))
       (final (remove-if (lambda (value) (find value hash)) win)))
  (* (reduce #'+ final) (car (cdr results))))
