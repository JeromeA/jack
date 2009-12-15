
(defun fact (n)
  (if (> n 1)
    (* n (fact (1- n)))
    1))

(defun test-recurse-fact (args)
  (write-int (fact (parse-integer (second args))))
  (write-line ""))

(make-executable 'test-recurse-fact)

