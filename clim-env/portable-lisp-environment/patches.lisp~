(in-package :clim-env)

;;;; in file utilities

;;; buff 7/21/99 added [source|object]-extensions - oversight by swm?
(defclass file-type ()
  ((name :accessor file-type-name :initarg :name)
   (object-types :accessor file-type-object-types :initarg :object-types)
   (compiler :accessor file-type-compiler :initarg :compiler)
   (source-loader :accessor file-type-source-loader :initarg :source-loader)
   (object-loader :accessor file-type-object-loader :initarg :object-loader)
   (source-extensions :accessor file-type-source-extensions :initarg :source-extensions)
   (object-extensions :accessor file-type-object-extensions :initarg :object-extensions)))

(defmacro rebinding ((&rest rest) &body body)
  `(let* ,(loop for x in rest collect `(,x ',(gensym (string x))))
     (declare (special ,@rest))
     `(let* ,(loop for x in ',rest collect (list (symbol-value x) x))
        ,(macroexpand-1 ,.body))))

(defmacro rebinding2 ((&rest rest) &body body)
  `(let* ,(loop for x in rest collect `(,x ',(gensym (string x))))
     (declare (special ,@rest))
     (macroexpand ,.body)))

(defmacro rebinding2 ((&rest vars) &body body)
  `(let* ,(loop for v in vars collect `(,v ',(gensym (string v))))
     (macroexpand ,.body)))

(defmacro rebinding-1 (vars syms &body body)
  `(let* ,(loop for v in vars 
                for s in syms 
                collect `(,v ,s))
     (macroexpand ,.body)))

(defmacro rebinding ((&rest vars) &body body)
  (let ((gensyms (loop for v in vars collect (gensym (string v)))))
    ``(let* ,(loop for v in ',vars
                   for g in ',gensyms
                   collect `(,g ,v))
        (rebinding-1 ,',vars ,',gensyms ,',.body))))

(defmacro rebindit ((&rest vars) &body body &environment env)
  (loop for v in vars do (setf v (gensym (string v))))
  (let ((new-body (macroexpand-1 body)))
    new-body))

(defmacro one (x &environment env) `(macroexpand-1 (setf ,x y)))
(defmacro two (y) (one i))
(defmacro three (x) ``(setf ,',x ,y))
(defmacro four (y) (three i))


(defmacro rebinding-1 (vars syms &body body)
  `(let* ,(loop for v in vars 
                for s in syms 
                collect `(,v ',s))
     (macroexpand ,.body)))

(defmacro rebinding ((&rest vars) &body body)
  `(let* ((gensyms (loop for v in ',vars collect (gensym (string v)))))
     `(let* ,(loop for v in ',vars
		   for g in gensyms
		   collect `(,g ,v))
        ,(rebinding-1 ,vars gensyms ,.body))))
