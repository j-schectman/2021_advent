(ql:quickload "SPLIT-SEQUENCE")

(defun star_one ()
      (with-open-file (stream "./data.lisp")
        (do ((line (read-line stream nil)
                   (read-line stream nil)))
            ((null line))
          (eval 
            (mapcar #'read-from-string 
                    (split-sequence:SPLIT-SEQUENCE #\SPACE line))))))
(defun run_star_one ()
  (let ((forw 0)
        (vert 0))
    (defun forward (num)
      (setf forw (+ forw num)))

    (defun down (num)
      (setf vert (+ vert num)))

    (defun up (num)
      (setf vert (- vert num)))

    (star_one)
    (* vert forw)))
(run_star_one)
(defun run_star_two ()
  (let ((forw 0)
        (vert 0)
        (aim 0))
    (defun forward (num)
      (setf forw (+ forw num))
      (setf vert (+ vert (* aim num))))

    (defun down (num)
      (setf aim (+ aim num)))

    (defun up (num)
      (setf aim (- aim num)))
    (star_one)
    (* vert forw)
    ))
(run_star_two)
