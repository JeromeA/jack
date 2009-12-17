

(defpackage :jack
  (:use cl)
  (:shadow defun))

(in-package :jack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C indent

(cl:defun indent-c (code)
  (cond
    ((stringp code) (concatenate 'string code '(#\newline)))
    ((consp code) (concatenate 'string (indent-c (car code)) (indent-c (cdr code))))
    (t "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Type bindings
;

(defparameter *bindings* ())

(cl:defun binding-get (symbol)
  (cdr (assoc symbol *bindings*)))

(cl:defun binding-add (symbol type)
  (setq *bindings* (acons symbol type *bindings*)))

(defmacro with-local-binding (&rest codes)
  `(let ((*bindings* *bindings*))
     ,@codes))

(defmacro with-binding (symbol type &rest codes)
  `(let ((*bindings* (acons ,symbol ,type *bindings*)))
     ,@codes))

(defmacro with-return-type (type &rest codes)
  `(with-binding 'return ,type ,@codes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; expresion to c translation
;

(declaim (special *declarations* *codes*))
(defparameter *defuns* ())

(cl:defun add-declaration (decl)
  (pushnew decl *declarations* :test #'equal))

(cl:defun add-code (decl)
  (push decl *codes*))

(cl:defun lispname-to-c (name)
  (substitute #\_ #\- (string-downcase (symbol-name name))))

(cl:defun defuns-get (symbol)
  (cdr (assoc symbol *defuns*)))

(cl:defun defuns-add (symbol definition)
  (setq *defuns* (acons symbol definition *defuns*)))

(cl:defun get-converter (name)
  (intern (concatenate 'string "CODE-" (symbol-name name) "-TO-C")))

(cl:defun existing-function-to-c (code)
  (let ((types (loop for expr in (cdr code)
                    collect (expr-type (to-c expr)))))
    (build-function (car code) types))
  (to-c code))
;(trace existing-function-to-c)

(cl:defun symbol-to-c (code)
  (list (binding-get code)
        (lispname-to-c code)))

(cl:defun to-c (code)
  (setf code (macroexpand code))
  (cond ((integerp code) (list 'int code))
        ((stringp code) (list 'zstring (format nil "\"~A\"" code)))
        ((symbolp code) (symbol-to-c code))
        ((listp code) (let ((conv (get-converter (car code))))
                        (unless (fboundp conv)
                          (existing-function-to-c code))
                        (apply conv (cdr code))))
        (t (error "No to-c for ~S" code))))
(trace to-c)

(cl:defun type-to-c (type)
  (cond
    ((null type) "void")
    ((eq type 'int) "int")
    ((eq type 'zstring)
     (add-declaration "typedef char *zstring;")
     "zstring")
    ((eq 'arraylen (car type))
     (let ((ctype (format nil "arraylen_of_~A" (type-to-c (second type)))))
       (add-declaration (format nil "typedef struct{int len;~A *data;} ~A;" (type-to-c (second type)) ctype))
       ctype))
    (t error "unsupported type ~A" type)))

(cl:defun expr-type (expr)
  (first expr))

(cl:defun expr-value (expr)
  (second expr))

(cl:defun expr-ctype (expr)
  (type-to-c (expr-type expr)))

(cl:defun funcall-to-c (fname return-type args)
  (list return-type
        (format nil "~A(~{~A~^, ~})"
                (lispname-to-c fname)
                (loop for arg in args
                      for expr = (to-c arg)
                      collect (expr-value expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; to-c implementations
;

(cl:defun code-block-to-c (name &rest code)
  (to-c (cons 'progn code)))

(cl:defun code-let-to-c (names &rest codes)
  (let ((decl (loop for (name code) in names
                    for expr = (to-c code)
                    collect (expr-ctype expr)
                    collect (lispname-to-c name)
                    collect (expr-value expr)))
        (expr (to-c (cons 'progn codes))))
    (list (expr-type expr)
          (format nil "({~{~A ~A = ~A;~}~A;})" decl (expr-value expr)))))

(cl:defun code-tagbody-to-c (&rest codes)
  (loop for code in codes
        collect (if (atom code)
                  (format nil "label_~A:" code)
                  (second (to-c code))) into exprs
        finally (return (list nil (format nil "({~{~A;~}})" exprs)))))

(cl:defun code-go-to-c (label)
  (list nil (format nil "goto label_~A" label)))

(cl:defun code-*-to-c (arg1 arg2)
  (let ((expr1 (to-c arg1))
        (expr2 (to-c arg2)))
      (list 'int (format nil "~A * ~A" (expr-value expr1) (expr-value expr2)))))

(cl:defun code-if-to-c (cond yes no)
  (let ((expr (to-c cond))
        (expr1 (to-c yes))
        (expr2 (to-c no)))
    (list 'int (format nil "~A ? ~A : ~A" (expr-value expr) (expr-value expr1) (expr-value expr2)))))

(cl:defun code->-to-c (arg1 arg2)
  (let ((expr1 (to-c arg1))
        (expr2 (to-c arg2)))
      (list 'bool (format nil "~A > ~A" (expr-value expr1) (expr-value expr2)))))

(cl:defun code-length-to-c (arg)
  (let ((expr (to-c arg)))
    (list 'int (format nil "~A.len" (expr-value expr)))))

(cl:defun code-write-line-to-c (arg)
  (let ((expr (to-c arg)))
    (add-declaration "#include <stdio.h>")
    (list 'int (format nil "printf(\"%s\\n\", ~A)" (expr-value expr)))))

(cl:defun code-write-int-to-c (arg)
  (let ((expr (to-c arg)))
    (add-declaration "#include <stdio.h>")
    (list 'int (format nil "printf(\"%d\", ~A)" (expr-value expr)))))

(cl:defun code-progn-to-c (&rest codes)
  (loop for code in codes
        for (type expr) = (to-c code)
        collect expr into exprs
        finally (return (list type (format nil "({~{~A;~}})" exprs)))))

(cl:defun code-parse-integer-to-c (str)
  (let ((expr (to-c str)))
    (add-declaration "#include <stdlib.h>")
    (list 'int (format nil "atoi(~A)" (expr-value expr)))))

(cl:defun code-car-to-c (arg)
  (let ((expr (to-c arg)))
    (list (second (expr-type expr)) (format nil "~A.data[0]" (expr-value expr)))))

(cl:defun code-second-to-c (arg)
  (let ((expr (to-c arg)))
    (list (second (expr-type expr)) (format nil "~A.data[1]" (expr-value expr)))))

(cl:defun code-1--to-c (arg)
  (let ((expr (to-c arg)))
    (list 'int (format nil "~A-1" (expr-value expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; build function
;

(cl:defun build-function (fname arg-types &optional return-type)
  (assert (defuns-get fname) (fname) "Can't find code for fname ~S" fname)
  (let ((code (defuns-get fname))
        (conv (get-converter fname)))
    (setf (fdefinition conv) (lambda (&rest args) (funcall-to-c fname 'int args)))
    (let ((expr (with-local-binding
                  (loop for var in (second code)
                        for type in arg-types
                        do (binding-add var type))
                  (to-c (cons 'progn (cddr code))))))
      (let ((signature (format nil "~A ~A(~{~A ~A~^, ~})" (expr-ctype expr) (lispname-to-c fname)
                               (loop for var in (second code)
                                     for type in arg-types
                                     collect (type-to-c type)
                                     collect (lispname-to-c var)))))
        (add-declaration (format nil "~A;" signature))
        (add-code (list (format nil "~A {" signature)
                        (format nil "~A~A;" (if (expr-type expr) "return " "") (expr-value expr))
                        "}"))))))
;(trace build-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; build executable
;

(cl:defun make-executable (mainfunction)
  (let* ((filename-bin (lispname-to-c mainfunction))
         (filename-source (format nil "~A.c" filename-bin))
         *declarations* *codes*)
    (build-function mainfunction '((arraylen zstring)))
    (with-open-file (f filename-source :direction :output)
      (format f "#define ~A main~%" (lispname-to-c mainfunction))
      (dolist (code (reverse *declarations*))
        (write-line code f))
      (dolist (code *codes*)
        (write-string (indent-c code) f)))
    (ext:run-program "gcc" :arguments (list "-g" "-Wall" "-Wno-main" filename-source "-o" filename-bin))))

(defmacro defun (fname lambda-list &rest body)
  (defuns-add fname `(lambda ,lambda-list ,@body))
  `(cl:defun ,fname ,lambda-list ,@body))

(cl:defun args ()
  #+SBCL (cdr *posix-argv)
  #+CLISP ext:*args*)

(cl:defun main ()
  (load (car (args)))
  (ext:exit))

(defun build-jack ()
  (format t "Building jack:")
  (ext:saveinitmem "jack" :init-function #'main :executable t))

(if (car (args))
  (main)
  (build-jack))

