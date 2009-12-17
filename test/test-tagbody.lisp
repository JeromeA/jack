
(defun test-tagbody (args)
  (tagbody
    (go 42)
    (write-line "41")
    42
    (write-line "42")))

(make-executable 'test-tagbody)

