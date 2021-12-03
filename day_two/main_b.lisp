(defun read_file()
  (let ((rez (list)))
      (with-open-file (stream "./02.data.lisp")
        (do ((line (read-line stream nil)
                   (read-line stream nil)))
            ((null line))
                    (setf rez 
                          (append rez (split-sequence:SPLIT-SEQUENCE #\SPACE line)))))
    rez))

(defun 

(flet ((fn1 () (print "here")))
  (funcall (intern "fn1")))
