;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :cl-user)


;;; Make the CLIM completer more friendly

(define-gesture-name :complete :keyboard (:i :control) :unique nil)


;;; Important patches to LispWorks 3.2

(defparameter *lispworks-3-2-patches*
    '("caps-lock"
      "copy-file"
      "delete-process-window"
      "gp-stipple-xy"
      "rename"))

(when (member ':lispworks3.2 *features*)
  (loop for patch in *lispworks-3-2-patches*
        for pathname = (merge-pathnames 
		         patch (pathname-location (current-pathname "~ext/beta-patches/")))
        do (when (probe-file pathname)
	     (sys::without-warning-on-redefinition
	       (compiler::compile-file-if-needed pathname :load t)))))


;;; Stuff we need in order to compile the CLIM environment

;;--- From ncomp;defs
(in-package :compiler)

(defmacro once-only (var-list &rest body)
  (do ((var-list var-list (cdr var-list))
       (name-bindings ())
       (variable-bindings ()))
      ((null var-list)
       `(let ,(nreverse name-bindings)
	  (list 'let (nconc ,@(nreverse variable-bindings)) ,@body)))
      (let ((var-name (caar var-list))
	    (var (cadar var-list)))
	(push `(,var-name (if (trivialp ,var) ,var (new-internal-variable)))
	      name-bindings)
	(push `(if  (not (eq ,var-name ,var)) (list (list ,var-name ,var)))
	      variable-bindings))))


;;--- From condition-system;debug
(in-package :dbg)

(defmacro with-interesting-frames ((&key (hidden-packages nil hidden-packages-p)) 
                                   &body body)
  `(let (,@(and hidden-packages-p `((*hidden-packages* ,hidden-packages))))
     ,@body))


;;--- From debug;nstack-defs
(in-package :dbg)

(system::without-warning-on-redefinition

(defmacro with-debugger-stack (stack &body body)
  (compiler::once-only ((stack-name stack))
	     `(let ((*debugger-stack* ,stack-name)
		    (*debug-level* (debugger-stack-level ,stack-name))
		    (*debug-condition* (debugger-stack-condition ,stack-name))
		    (*debug-abort* (debugger-stack-abort ,stack-name))
		    (*debug-continue* (debugger-stack-continue ,stack-name))
		    (*debug-restarts* (debugger-stack-restarts ,stack-name))
		    (*debug-number-of-debug-restarts*
		     (debugger-stack-no-restarts ,stack-name)))
		,@body)))

)

(defmacro stack-current-frame (x)
  `(first (debugger-stack-next ,x)))


(defmacro for-next-frames (frame frames &body body)
  `(do ((,frame ,frames (frame-next ,frame)))
       ((null ,frame))
     ,@body))

(defmacro for-prev-frames (frame frames &body body)
  `(do ((,frame ,frames (frame-prev ,frame)))
       ((null ,frame))
     ,@body))


;;; Patches to the disassembler to support disassembly in a given range

#+harp::sparc
(eval-when (compile load eval)
  (sys::do-demand-pre-loads :disassemble))

#+harp::sparc
(in-package :dis)

#+harp::sparc
(defvar *disassemble-min-pc* nil)
#+harp::sparc
(defvar *disassemble-max-pc* nil)

#+harp::sparc
;;--- It's a kludge, but it'll do the trick for the time being
(defun format-t (str &rest args)
  (when (or (null *disassemble-min-pc*)
	    (null *disassemble-max-pc*)
	    (and (< *disassemble-min-pc* 
	            (* *current-pc* 4))
                 (< (* *current-pc* 4)
	            *disassemble-max-pc*)))
    (apply #'format t str args)))

#+harp::sparc
(defun format-out (opcode &key label dest s1 s2 base offset asi )
  (when (or (null *disassemble-min-pc*)
	    (null *disassemble-max-pc*)
	    (and (< *disassemble-min-pc* 
	            (* *current-pc* 4))
                 (< (* *current-pc* 4)
	            *disassemble-max-pc*)))
    (let ((*print-base* 16)
	  (label-here (if *doing-fixups*
			  *fixup-label*
		        (svref *labels* *current-pc*))))
      (if label-here
	  (format t "~%.L~d:~8t" label-here)
        (format t "~%~3x~8t" (* *current-pc* 4)))
      (format t "~(~a~)~16t" opcode)
      (when label
        (format t ".L~d" label))
      (when s1
        (if s2
	    (format t "~(~a, ~a~)" s1 s2)
	  (format t "~(~a~)" s1)))
      (when base 
        (when s1
	  (format t ", "))
        (if (and offset
	         (not (eq offset 0))
	         (not (eq offset *null-register*)))
	    (if (and (numberp offset)
		     (minusp offset))
	        (format t "~([~a - ~a]~)" base (- offset))
	      (format t "~([~a + ~a]~)" base offset))
	  (format t "~([~a]~)" base))
        (when asi
	  (format t " ~a" asi)))
      (when dest
        (when (or s1 base)
	  (format t ", "))
        (format t "~(~a~)" dest)))))

#+harp::sparc
(defun dis-jmpl (word other-op3)
     (declare (ignore other-op3))
     (let ((link (dis-rd word))
	   (base  (dis-rs1 word))
	   (offs  (dis-simm13/rs2 word)))
	  (if (eq link *null-register*)
		    (dis-out 'jmp
			     :base (dis-rs1 word)
			     :offset (dis-simm13/rs2 word))
	       (dis-out 'jmpl 
			:dest link
			:base base
			:offset offs))
; new stuff for printing nil vector calls
	  (when (and (eq base '%g1)
		     (sys::fixnump offs)
		     (= (mod offs 4) 2))
	       ; (format-t "found an offset ~X from nil" offs)
	       (let  ((possible-nil-symbol (sys::raw-+ nil 
						       (sys::fixnum-raw-int (- offs 38))))
		      (possible-nil-stub   (sys::raw-+ nil
						       (sys::fixnum-raw-int (- offs 5)))))
		    (if (symbolp possible-nil-symbol)
			      (progn (setq possible-nil-stub 0)
				     (format-t "    ;;  global fun: ~S" possible-nil-symbol))
			 (progn (setq possible-nil-symbol 0)
				(if (sys::i-vectorp possible-nil-stub)
					  (examine-potential-code-stub possible-nil-stub)
				     (setq possible-nil-stub 0))))
		    )
	       )
; end new stuff
	  )
     )

#+harp::sparc
(defun  examine-potential-code-stub (thing-from-nil)
     (cond ((symbolp thing-from-nil)	  (format-t "    ;;  global: ~S" thing-from-nil))

	   ((sys::i-vectorp thing-from-nil)
	    (if  (and (= (length thing-from-nil) 5)
		      (= #xD820684A (aref thing-from-nil 0)) ; check is a simple stub
		      (= #x98006000 (logand #xFFFFE000 (aref thing-from-nil 1))))
		      (let* ((offset (logand #x00001FFF (aref thing-from-nil 1)))
			     (thing  (sys::raw-+ nil (sys::fixnum-raw-int offset))))
			   (if (symbolp thing)
				     (format-t "    ;;  stub for ~S" thing)
				(format-t "    ;;  some stub")))
		 (format-t "    ;;  some complex stub")))				    
	   ))

#+harp::sparc
(defun dis-f3-ld-st (word)
  (let* ((op3 (logand (ash word -19) #b111111))
	 (ld-st (if (eql (logand op3 #b1100) #b0100)
		    :s1
		  :dest)))
    (case (ash op3 -4)
      (0 (let  ((base (dis-rs1 word))
		(offs (dis-simm13/rs2 word)))
	      (dis-out (svref *format-3-codes-3* op3)   ;; includes normal LD
		       ld-st   (dis-rd word)
		       :base   base
		       :offset offs)
;; new stuff, markt, show constants
	      (when (and (= op3 4)  ; ST WORD instruction
			 (sys::fixnump offs)
			 (eq base '%g1)   ; NIL register
			 (<= sys::*%multiple-value-offset-in-bytes offs
			     (+ sys::*%multiple-value-offset-in-bytes (* 4 48))))
		   (format-t "    ;;  ~:R multiple value"
			   (+ 2 (/ (- offs sys::*%multiple-value-offset-in-bytes) 4))))

	      (when (and (= op3 0)  ; LD WORD instruction
			 (sys::fixnump offs))   ; with an immediate offset
		   (when (and (eq base '%g1) ; NIL register
			      (= (mod offs 4) 2)  ; tagged correctly for access via NIL
			      )
			(if  (<= sys::*%multiple-value-offset-in-bytes offs
				 (+ sys::*%multiple-value-offset-in-bytes (* 4 48)))
				  (format-t "    ;;  ~:R multiple value" 
					  (+ 2 (/ (- offs sys::*%multiple-value-offset-in-bytes) 4)))
			     (let  ((thing-from-nil (sys::raw-load nil (sys::fixnum-raw-int offs))))
				  (when (or (symbolp thing-from-nil)
					    (sys::i-vectorp thing-from-nil))
				       (examine-potential-code-stub thing-from-nil))
				  )))
		   (when (and *constants*      ; have a constants vector
			      (eq base '%i4)   ; number for i4, constants reg
			      (= (mod offs 4) 1))
			(if  (= #x1d offs)
				  (format-t "    ;;  call counter")
			     (let  ((*print-length* 3)
				    (*print-level*  2))
				  (format-t "     ;;  ~S" (svref *constants* (truncate (- offs 5) 4)))))))
;; end new stuff
	      ))
      (1 (dis-out (svref *format-3-codes-3* op3)
		  ld-st (dis-rd word)
		  :base (dis-rs1 word)
		  :offset (dis-rs2 word)
		  :asi (dis-asi word)))
      (2 (dis-out (svref *format-3-codes-3* op3)
		  ld-st (case (- op3 32)
			  ((1 5) '%fsr)
			  (6 '%fq)
			  (t (dis-rd word 1)))
		  :base (dis-rs1 word)
		  :offset (dis-simm13/rs2 word)))
      (3 (dis-out (svref *format-3-codes-3* op3)
		  ld-st (case (- op3 48)
			  ((1 5) '%csr)
			  (6 '%cq)
			  (t (dis-rd word 1)))
		  :base (dis-rs1 word)
		  :offset (dis-simm13/rs2 word))))))



;;; Patches to DESCRIBE to support creation of presentations

(eval-when (compile load eval)
  (sys::do-demand-pre-loads :describe))

(in-package :ccl)

;;--- Create presentations when doing output on a CLIM stream
(sys::without-warning-on-redefinition
#+LispWorks3.2
(defmethod describe-object (x stream)
  (setq *descr-continuation* nil)
  (let* ((descr-stream-p (descr-stream-p stream))
         (clim-stream (if descr-stream-p
			  (and (typep (indenting-stream-stream stream) 'clim:clim-stream-pane)
                               (indenting-stream-stream stream))
			  (and (typep stream 'clim:clim-stream-pane)
                               stream)))
	 (*describe-object-label* (if (boundp '*describe-object-label*)
				      *describe-object-label*
				      *default-describe-object-label*)))
    (multiple-value-bind
	(attributes values getter setter name)
      (get-inspector-values x
			    (and descr-stream-p
				 (descr-stream-mode stream)))
      (declare (ignore getter))
      (when descr-stream-p
	    (setf (descr-stream-attributes stream) attributes
		  (descr-stream-values stream) values
		  (descr-stream-setter stream) setter))
      (let* ((attributes attributes)
	     (max-attr-width 0)
	     (count 0)
	     (attribute-cells nil)
	     (too-many-attributes
	      (when attributes
		    (loop
		     (let* ((attribute (pop attributes))
			    (attribute-cell
			     (might-abbreviate (princ-to-string attribute)
					       *describe-attribute-width*))
			    (width (length attribute-cell)))
		       (push attribute-cell attribute-cells)
		       (when (> width max-attr-width)
			     (setq max-attr-width width))
		       (when (null attributes)
			     (return nil))
		       (when (and *describe-length*
				  (= (incf count) *describe-length*))
			     (return attributes)))))))
	(setf attribute-cells (nreverse attribute-cells))
	(when name
	      (format stream
		      "~s ~a"
		      x
		      (is-a (if (stringp name)
				name
			      (prin1-to-string name)))));; nick 03.08.93
	(incf max-attr-width 5)
	(do ((attribute-cells attribute-cells (cdr attribute-cells))
	     (values values (cdr values)))
	    ((null attribute-cells)
	     (when too-many-attributes
		  (setq *descr-continuation*
			(list* *describe-object-label*
			       too-many-attributes values))
		  (format stream
			  (if (and descr-stream-p
				   (descr-stream-interactive-p stream))
			      " ........ (:dm for more)~%~%"
			    " ........~%~%"))))
	    (format stream
		    *describe-object-label*
		    (car attribute-cells)
		    max-attr-width)
            (if clim-stream
                (clim:with-output-as-presentation
                    (clim-stream (car values) 'clim:expression)
                  (if descr-stream-p
		      (describe (car values) stream)
	              (format-with-attributes stream "~s" (car values))))
                (if descr-stream-p
		    (describe (car values) stream)
	            (format-with-attributes stream "~s" (car values)))))))))

#-LispWorks3.2
(defmethod describe-object (x stream)
  (setq *descr-continuation* nil)
  (let* ((descr-stream-p (descr-stream-p stream))
         (clim-stream (if descr-stream-p
                          (and (typep (indenting-stream-stream stream) 'clim:clim-stream-pane)
                               (indenting-stream-stream stream))
                          (and (typep stream 'clim:clim-stream-pane)
                               stream)))
	 (*describe-object-label* (if (boundp '*describe-object-label*)
				      *describe-object-label*
				      *default-describe-object-label*)))
    (multiple-value-bind
	(attributes values getter setter name)
      (get-inspector-values x
			    (and descr-stream-p
				 (descr-stream-mode stream)))
      (when descr-stream-p
	    (setf (descr-stream-info stream) (list attributes values setter getter)))
      (let* ((attributes attributes)
	     (max-attr-width 0)
	     (count -1)
	     (attribute-cells nil)
	     (too-many-attributes
	      (when attributes
		    (loop
		     (when (and *describe-length*
				(= (incf count) *describe-length*))
		       (return attributes))
		     (let* ((attribute (pop attributes))
			    (attribute-cell
			     (might-abbreviate (format nil *describe-attribute-formatter* attribute)
					       *describe-attribute-width*))
			    (width (length attribute-cell)))
		       (push attribute-cell attribute-cells)
		       (when (> width max-attr-width)
			 (setq max-attr-width width))
		       (when (null attributes)
			 (return nil)))))))
	(setf attribute-cells (nreverse attribute-cells))
	(when name
	      (format stream
		      "~s ~a"
		      x
		      (is-a (if (stringp name)
				name
			      (prin1-to-string name)))));; nick 03.08.93
	(incf max-attr-width 5)
	(do ((attribute-cells attribute-cells (cdr attribute-cells))
	     (values values (cdr values)))
	    ((null attribute-cells)
	     (when too-many-attributes
		  (setq *descr-continuation*
			(list* *describe-object-label*
			       too-many-attributes values))
		  (format stream " ........~:[~; (:dm for more)~]~2%"
			  (and descr-stream-p
                               (descr-stream-interactive-p stream)))))
	    (format stream
		    *describe-object-label*
		    (car attribute-cells)
		    max-attr-width)
            (if clim-stream
                (clim:with-output-as-presentation (clim-stream (car values) 'clim:expression)
                  (if descr-stream-p
		      (describe (car values) stream)
	              (prin1 (car values) stream)))
                (if descr-stream-p
		    (describe (car values) stream)
	            (prin1 (car values) stream))))))))

)	;sys::without-warning-on-redefinition


;;; Patches to the editor to integrate with CLIM's kill ring

(in-package :editor)

;;--- From lw;editor;macros
(defmacro with-point-and-mark( &rest body)
  `(let ((%point% (current-point))
	(%mark% (current-mark )))
    (if (point> %point% %mark%)
	(rotatef %point% %mark%))
    ,@body))

;;--- From lw;editor;killcoms
(sys::without-warning-on-redefinition
(defcommand "Save Region" (p)
     "Insert the region into the kill ring.
   If the region is not active nor the last command a yank, signal an error."
     "Insert the region into the kill ring."
  (declare (ignore p))
  (with-point-and-mark 
   (ring-push (points-to-string %point% %mark%) *kill-ring*)
   (when (boundp 'clim:*kill-ring*)
     ;; Put it on the CLIM kill ring, too
     (clim:push-history-element 
       clim:*kill-ring* (points-to-string %point% %mark%)))))
)	;sys::without-warning-on-redefinition

;;--- From lw:editor;killcoms
(defun %kill-region (start end &optional (current-type :kill-forward))
  (let ((type (last-command-type))
	(new (delete-points-and-save start end)))
    (cond ((zerop (ring-length *kill-ring*))
	   (ring-push new *kill-ring*))
	  ((or (eq type :kill-forward) (eq type :kill-backward))
	   (let ((r (ring-pop *kill-ring* )))
	     (ring-push (if (eq current-type :kill-forward)
			    (concatenate 'simple-string r new)
			  (concatenate 'simple-string new r)) 
			*kill-ring*)))	   
	  (t
	   (ring-push new *kill-ring*)))
    (when (boundp 'clim:*kill-ring*)
      ;; Put it on the CLIM kill ring, too
      (if (and (member type '(:kill-forward ':kill-backward))
               (plusp (clim:history-length clim:*kill-ring*)))
          ;; Merge with last kill
	  (let ((last (clim::pop-history-element clim:*kill-ring*)))
            (clim:push-history-element 
	      clim:*kill-ring*
	      (if (eq current-type :kill-forward)
		  (concatenate 'simple-string new new)
		  (concatenate 'simple-string new last))))
          ;; Else just push the new element
          (clim:push-history-element clim:*kill-ring* new)))
    (setf (last-command-type) current-type)
    new))


;;; Remove overzealous error-checking from wildcard merging

(in-package :system)

(sys::without-warning-on-redefinition
(defun translate-pathname (source from-wildname to-wildname &key (case :local))
  (setq source (pathname source)
	from-wildname (pathname from-wildname)
	to-wildname (pathname to-wildname))
  #+ignore
  (assert (pathname-match-p source from-wildname)
	  nil
	  "Source pathname ~s does not match from-wildname ~s."
	  source
	  from-wildname)
  (let ((case-translation
	 (if (eq case :local)
	     (cond ((and (not (logical-pathname-p to-wildname))
			 (logical-pathname-p source))
		    :downcase))
	   case)))
    (make-pathname-of-same-type
     to-wildname
     (or (%pathname-host to-wildname)
	 (and ;;MJS 06/02/92: zap the host from phyical PC pathnames
	      ;; #+harlequin-pc-lisp   MJS 27May93: for unix too since default host is NIL now
	      (logical-pathname-p to-wildname)
	      (%pathname-host source)))
     (or (%pathname-device to-wildname)
	 (%pathname-device source))
     (translate-pathname-directory
      (%pathname-directory source)
      (%pathname-directory from-wildname)
      (%pathname-directory to-wildname)
      case-translation)
     (translate-pathname-component-string
      (%pathname-name source)
      (%pathname-name from-wildname)
      (%pathname-name to-wildname)
      case-translation)
     (translate-pathname-component-string
      (%pathname-type source)
      (%pathname-type from-wildname)
      (%pathname-type to-wildname)
      case-translation)
     (translate-pathname-component-string
      (%pathname-version source)
      (%pathname-version from-wildname)
      (%pathname-version to-wildname)
      case-translation))))

(defun translate-pathname-directory (source from to case-translation)
  (cond ((or (null to) (eq to :wild)) source)
	((null source) 
	 (if (equal from '(:absolute :wild-inferiors))
	     (replace-wildcards-in-directory to 
					     '((:wild-inferiors))
					     case-translation)
;	     (perform-case-translation to case-translation)
	   (error "frob-dir")))
	(t
	 (multiple-value-bind
	  (winp matches)
	  (directories-match-p source from)
	  #+ignore (assert winp)
	  (replace-wildcards-in-directory to matches case-translation)))))
)	;sys::without-warning-on-redefinition
