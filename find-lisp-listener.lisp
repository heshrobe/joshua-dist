(defun find-lisp-listener ()
  (loop for p in mp:*all-processes* 
      for name = (mp:process-name p)
      when (search "lisp-listener" name :test #'string-equal)
      collect (list name p)))

(defun interrupt-lisp-listener ()
  (loop for (name p) in (find-lisp-listener)
      do (format t "~%Interrupting ~a ~a" name p)
	 (mp:process-interrupt p 'break)))

(eval-when (:load-toplevel)
  (export '(find-lisp-listener interrupt-lisp-listener)))

