(in-package :cl-user)

#|

(rebinding varlist &body body)
-- prevents variable capture and multiple evaluation inside macro bodies

; rewriting the following macro (vulnerable to variable capture and
multiple evals etc):

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

; as:

(defmacro for ((var start stop) &body body)
  (rebinding (start stop) ; don't rebind var, because the do ... body needs
                          ; to capture it
    `(do ((,var ,start (1+ ,var)))
         ((> ,var ,stop))
       ,@body)))

; results in a macro that is equivalent to:

(defmacro for ((var start stop) &body body)
  (let ((gstart (gensym))
        (gstop (gensym))
        )
    `(destructuring-bind (,gstart ,gstop)
                         (list ,start ,stop)
       (do ((,var ,gstart (1+ ,var)))
           ((> ,var ,gstop))
         ,@body))))

; and which is thus free of variable capture problems and multiple-evaluation
; problems. Also note that gvar, gstart, and gstop are merely used here for
; readability, in fact they are also gensymmed so nested calls to rebinding
; will not break

|#

; standing comment throughout this macro is: this is wierd as hell but correct
(defmacro rebinding (varlist &body body)
  (labels (; explode takes a list and list*-ifies it, so we can compare the results
           ; of what rebinding creates with what back-quote and comma would create
           (explode (l) (if (atom l) l (%explode l)))
           (%explode (l) (if (null (cdr l))
                           `(list ,(car l))
                           `(list* ,(explode (car l)) ,(%explode (cdr l)))))
           ; given a form and an assoc-list, replaces all the items in the form
           ; that appear as keys in the assoc-list with their associtated item
           (alist-replace (f al)
             (cond ((null f) f) ; don't want to replace nils, that would be bad
                   ((atom f) (or (cdr (assoc f al)) f))
                   (t (cons (alist-replace (car f) al) (alist-replace (cdr f) al))))))
    (let (; make an assoc-list of the gensyms that will be in the macro body
          ; (this is done once per expantion of rebinding so that variables won't
          ; be captured in nested calls to rebinding, god knows why you'd want to)
          (new-varlist (mapcar #'(lambda (var) (cons var (gensym))) varlist)))
      `(let
         ; let-bind all the gensyms to a gensym for use in the macro body
         ,(mapcar #'(lambda (var) (list (cdr (assoc var new-varlist)) '(gensym))) 
                  varlist)
         `(destructuring-bind
            ; let-bind all the gensyms in the gensyms to the evaluated varlist
            ,,(explode (mapcar #'(lambda (var) (cdr (assoc var
                                                           new-varlist))) varlist))
            (list ,@,(explode varlist))
            ;
            ,@(list ,@(alist-replace body new-varlist)))))))



