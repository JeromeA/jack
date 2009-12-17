
;(defun test-let-1 (args)
;  (let (x)
;    (setf x 42)
;    (write-int x)))
;
;(make-executable 'test-let-1)
;
;(defun test-let-2 (args)
;  (let ((x))
;    (setf x 42)
;    (write-int x)))
;
;(make-executable 'test-let-2)

(defun test-let-3 (args)
  (let ((x 42))
    (write-int x)
    (write-line "")))

(make-executable 'test-let-3)

