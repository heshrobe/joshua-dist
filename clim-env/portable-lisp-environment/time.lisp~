;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Time parser, heavily cribbed from a KMP version

(defconstant +whitespace+ '(#\Space #\Tab))

(declaim (inline whitespace-p))
(defun whitespace-p (x) (member x +whitespace+ :test #'char-equal))

(define-condition time-parser-error (parse-error)
  ((format-string :initarg :format-string
		  :reader parse-error-format-string )
   (format-arguments :initarg :format-arguments :initform nil
		     :reader parse-error-format-arguments))
  (:report (lambda (condition stream)
	     (apply #'format stream (parse-error-format-string condition)
				    (parse-error-format-arguments condition)))))

(defun time-parser-error (format-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (error 'time-parser-error
	 :format-string format-string
	 :format-arguments (copy-list format-arguments)))

(defmacro parse-string-tokens (kind (string items) &body forms)
  (clim-utils:with-gensyms (char token pos len type)
;    (lispworks:rebinding (kind string)
     (let ((gkind (gensym "KIND"))
           (gstring (gensym "STRING")))
      `(let* ((,gkind ,kind)
              (,gstring ,string)
              (,pos 0)
              ,token
              (,len (loop for i downfrom (length ,gstring)
                          while (> i 0)
                          for j downfrom (1- i)
                          when (not (whitespace-p (char ,gstring j)))
		            do (return i)
                          finally (return 0)))
              (,char (when (> ,len 0) (aref ,gstring 0))))
         (macrolet ((next-char ()
                      `(progn
                         (incf ,',pos)
			 (setq ,',char (when (< ,',pos ,',len)
                                        (aref ,',gstring ,',pos)))))
                    (pop-char ()
                      `(prog1 ,',char (next-char)))
                    (peek-next-char ()
                      `(let ((,',pos ,',pos) (,',char ,',char))
			 (next-char)))
                    (with-string (kind (var) &body forms)
                      `(values (with-output-to-string (,var) ,@forms)
                               ,kind)))
           (labels ((fail (&optional format-string &rest format-args)
                      (if format-string
			(apply #'time-parser-error format-string format-args)
			(time-parser-error "Unrecognized notation for ~A: ~A~@[~
					    ~%  Problem detected at or near token ~S.~]"
					   ,gkind ,gstring ,token)))
                    (gobble-whitespace ()
                      (loop until (or (not ,char) (not (whitespace-p ,char)))
			    do (next-char)))
                    (get-token ()
                      (when ,char
			(cond ((whitespace-p ,char)
			       (gobble-whitespace)
			       (values " " :whitespace))
			      ((alpha-char-p ,char) 
			       (with-string :alpha (str)
		                 (loop until (or (not ,char)
                                                 (not (alpha-char-p ,char)))
				       do (write-char (pop-char) str))))
			      ((digit-char-p ,char) 
			       (with-string :digits (str)
			         (loop until (or (not ,char)
                                                 (not (digit-char-p ,char)))
				       do (write-char (pop-char) str))))
			      (t
			       (with-string :literal (str)
                                 (write-char (pop-char) str)))))))
             (gobble-whitespace)
             (let ((,items (loop for (,token ,type) = (multiple-value-list
                                                       (get-token))
                                 while ,token
                                 collect (list ,token ,type (length ,token)))))
               (macrolet ((assign (&rest forms)
                            `(progn (when (or ,@(loop for f on forms by #'cddr
                                                      collect (car f)))
                                      (fail))
                               (setq ,@forms)))
                          (skip (n kind)
			    (declare (ignore kind))
                            `(progn
                               (setq ,',items (nthcdr ,n ,',items))
                               (setq ,',token (car (car ,',items))))))
                 ,@forms)
               (when ,items (fail)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun get-pattern-element-vars (element)
  (cond ((not (consp element)) '())
        ((eq (car element) 'or)
         (mapcan #'get-pattern-element-vars (cdr element)))
        ((and (consp (cdr element)) (symbolp (cadr element)) (cadr element))
         (list (cadr element)))))

)	;eval-when

(defmacro match-pattern-element (var element)
  `(let ((.item. (pop ,var)))
     ,(labels ((process-pattern-element (element)
                 (when (atom element) (setq element `(:literal nil ,element)))
                 (let ((len (length element)) (op (car element)))
                   (ecase op
                     (or
                       `(or ,@(mapcar #'process-pattern-element (cdr element))))
                     (:whitespace
                       (unless (= len 1) (error "Usage is (:whitespace)."))
                       `(eq (cadr .item.) :whitespace))
                     (:digits
                       (unless (<= 2 len 4)
                         (error "Usage is (:DIGITS var), (:DIGITS var len), or (:DIGITS var lo-len hi-len)."))
                       `(and (eq (cadr .item.) :digits)
                             ,(ecase len
                                ((2) t)
                                ((3) `(= (length (car .item.)) ,(third element)))
                                ((4) `(<= ,(third element)
                                          (length (car .item.))
                                          ,(fourth element))))
                             (setq ,(cadr element) (parse-integer (car .item.)))))
                     (:alpha
                       (unless (<= 2 len 4)
                         (error "Usage is (:ALPHA var), (:ALPHA var len), or (:ALPHA var lo-len hi-len)."))
                       `(and (eq (cadr .item.) :alpha)
                             ,(ecase len
                                ((2) t)
                                ((3) `(= (length (car .item.)) ,(third element)))
                                ((4) `(<= ,(third element)
                                          (length (car .item.))
                                          ,(fourth element))))
                             (setq ,(cadr element) (car .item.))))
                     (:predicate
                       (unless (= len 3)
                         (error "Usage is (:PREDICATE var pred-fn)."))
                       `(when (,(third element) (car .item.))
                          (setq ,(second element) (car .item.))))
                     ((member assoc find position)
                      (unless (and (<= 3 len 4)
                                   (symbolp (second element))
                                   (symbolp (third element))
                                   (char= (char (string (third element)) 0) #\*))
                        (error "Usage is (~S local-var special-var [#'key-fn])." op))
                      `(setq ,(second element)
                             (,op (car .item.) ,(third element)
                              :test #'string-equal
                              ,@(when (= len 4) `(:key ,(fourth element))))))
		     (:test
		       (unless (= len 2) (error "Usage is (:TEST test)"))
		       `(progn (push .item. ,var) ,(second element)))
                     (:literal
                       (let (var literal)
                         (unless (and (= len 3)
                                      (symbolp (setq var (second element)))
                                      (stringp (setq literal (third element))))
                           (error "Usage is (:LITERAL var string)."))
                         `(string-equal ,(if var
                                           `(setq ,var (car .item.))
					   `(car .item.))
                                        ,literal)))))))
        (process-pattern-element element))))

(defmacro match-pattern (var pattern)
  `(and ,@(mapcar #'(lambda (p)
                      (if (equal p '(*))
                        `t
			`(and ,var (match-pattern-element ,var ,p))))
                  pattern)
        ,(if (equal (last pattern) '((*)))
           `t
	   `(not ,var))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun get-pattern-vars (pattern)
  (cond ((not (consp pattern)) nil)
        ((eq (car pattern) 'or)
         (mapcan #'get-pattern-vars (cdr pattern)))
        (t
         (mapcan #'get-pattern-element-vars pattern))))

)	;eval-when

(defmacro match (form &body clauses)
  (let ((temp (gensym)) (temp1 (gensym))
        (vars (delete-duplicates
                (mapcan #'(lambda (clause)
                            (get-pattern-vars (car clause)))
                        clauses))))
    (labels ((process-pattern (pattern)
               `(progn
                  (setq ,temp1 ,temp
                        ,@(mapcan #'(lambda (var) (list var nil)) vars))
                  (match-pattern ,temp1 ,pattern)))
             (process-clause (clause)
               (let ((pattern (car clause))
                     (forms   (cdr clause)))
                 (cond ((eq pattern 'otherwise)
                        `(t ,@forms))
                       ((atom pattern)
                        (error "Invalid pattern: ~S" pattern))
                       ((eq (car pattern) 'or)
                        `((or ,@(mapcar #'process-pattern (cdr pattern)))
                          ,@forms))
                       (t
                        `(,(process-pattern pattern) ,@forms))))))
      `(let ((,temp ,form) ,temp1 ,@vars)
         (cond ,@(mapcar #'process-clause clauses))))))


(defvar *days-of-the-week*
        '#(("Monday"    "Mon")
           ("Tuesday"   "Tue")
           ("Wednesday" "Wed")
           ("Thursday"  "Thu")
           ("Friday"    "Fri")
           ("Saturday"  "Sat")
           ("Sunday"    "Sun")))

(defun day-of-the-week-p (string)
  (dotimes (i 7 nil)
    (when (member string (aref *days-of-the-week* i))
      (return i))))

(defun day-name (day-number &key (format :long))
  (check-type day-number (integer 0 6))
  (let ((entry (aref *days-of-the-week* (the fixnum day-number))))
    (ecase format
      (:long  (first  entry))
      (:short (second entry)))))

(defvar *relative-days* '("yesterday" "today" "tomorrow"))

(defvar *months*
        '#(("January"   "Jan")
           ("February"  "Feb")
           ("March"     "Mar")
           ("April"     "Apr")
           ("May"       "May")
           ("June"      "Jun")
           ("July"      "Jul")
           ("August"    "Aug")
           ("September" "Sep")
           ("October"   "Oct")
           ("November"  "Nov")
           ("December"  "Dec")))

(defun month-p (string)
  (let ((pos (position-if #'(lambda (names) (member string names :test #'string-equal))
                          *months*)))
    (and pos (1+ pos))))

(defun month-name (month-number &key (format :long))
  (check-type month-number (integer 1 12))
  (let ((entry (aref *months* (1- (the fixnum month-number)))))
    (ecase format
      (:long  (first  entry))
      (:short (second entry)))))

;;--- We need a more complete table of these. -kmp 8-Mar-94
(defparameter *timezones*
              '(("GMT" 0 nil) ("BST" 0 t)
                ("EST" 5 nil) ("EDT" 5 t)
                ("CST" 6 nil) ("CDT" 6 t)
                ("MST" 7 nil) ("MDT" 7 t)
                ("PST" 8 nil) ("PDT" 8 t)
		("JST" -9 nil)))

(defun timezone-name (n daylight-p)
  (dolist (entry *timezones*)
    (when (and (= (second entry) n) (if daylight-p (third entry) (not (third entry))))
      (return (first entry)))))

(defvar *n1/n2/n3-meaning* :month-date-year)
(defvar *n1.n2.n3-meaning* :date-month-year)
(defvar *n1-n2-n3-meaning* :year-month-date)

;;--- Signal an error when any components are seen more than once
(defun parse-universal-time (string)
  (when (string-equal string "now")
    (return-from parse-universal-time
      (get-universal-time)))
  (when (string-equal string " ago" :start1 (- (length string) 4))
    (multiple-value-bind (duration components)
        (parse-duration (subseq string 0 (- (length string) 4)))
      (return-from parse-universal-time
        (values
          (- (get-universal-time) duration)
          components))))
  (let (day year month date hour minute second tz daylight state)
    (parse-string-tokens "time" (string items)
      (loop
	(unless items (return))
	(match items
	  ((or ((position dy *days-of-the-week* #'first)  (*))
	       ((position dy *days-of-the-week* #'second) (*)))
	   (setq state :day)
	   (assign day dy)
	   (skip 1 "day"))
	  ((or ((:digits h 1 2) ":" (:digits m 2) ":" (:digits s 2) (*))
	       ((:digits h 1 2) ":" (:digits m 2) (*)))                
	   (when (or (> h 23) (> m 59) (and s (> s 59))) (fail))
	   (setq state :time)
	   (assign hour h minute m second (or s 0))
	   (if s (skip 5 "hh:mm:ss") (skip 3 "hh:mm")))
	  ((or ((:digits d 1 2) "-" (:predicate m month-p) "-" (:digits y 2) (*))
	       ((:digits d 1 2) "-" (:predicate m month-p) "-" (:digits y 4) (*)))
	   (setq state :date)
	   (setq m (or (month-p m) (fail)))
	   (when (<= 0 y 99) (incf y 1900))	;--- not actually right
	   (assign year y month m date d)
	   (skip 5 "dd-mmm-yy"))
	  ((or ((:predicate m month-p) (:whitespace) (:digits d 1 2) (*))
	       ((:digits d 1 2) (:whitespace) (:predicate m month-p) (*)))
	   (setq state :date)
	   (setq m (or (month-p m) (fail)))
	   (assign month m date d)
	   (skip 3 "mmm dd"))
	  (((:digits y 4) "-" (:digits m 1 2) "-" (:digits d 1 2) (*))
	   (setq state :date)
	   (assign year y month m date d)
	   (skip 5 "yyyy-mm-dd"))
	  (((:digits q1 1 2) "/" (:digits q2 1 2) "/"
	    (or (:digits q3 2) (:digits q3 4)) (*))
	   (setq state :date)
	   (when (<= 0 q3 99) (incf q3 1900))	;--- not actually right
	   (cond ((> q1 12)
		  (assign year q3 month q2 date q1))
		 ((> q2 12)
		  (assign year q3 month q1 date q2))
		 (t
		  (ecase *n1/n2/n3-meaning*
		    (:month-date-year
		     (assign year q3 month q1 date q2))
		    (:date-month-year
		     (assign year q3 month q2 date q1)))))
	   (skip 5 "xx/yy/zz"))
	  (((:digits q1 1 2) "/" (:digits q2 1 2) (*))
	   (setq state :date)
	   (assign year (nth-value 5 (get-decoded-time)))
	   (cond ((> q1 12)
		  (assign month q2 date q1))
		 ((> q2 12)
		  (assign month q1 date q2))
		 (t
		  (ecase *n1/n2/n3-meaning*
		    (:month-date-year
		     (assign month q1 date q2))
		    (:date-month-year
		     (assign month q2 date q1)))))
	   (skip 3 "xx/yy"))
	  (((:digits q1 1 2) "." (:digits q2 1 2) "."
	    (or (:digits q3 2) (:digits q3 4)) (*))
	   (setq state :date)
	   (when (<= 0 q3 99) (incf q3 1900))	;--- not actually right
	   (cond ((> q1 12)
		  (assign year q3 month q2 date q1))
		 ((> q2 12)
		  (assign year q3 month q1 date q2))
		 (t
		  (ecase *n1.n2.n3-meaning*
		    (:month-date-year
		     (assign year q3 month q1 date q2))
		    (:date-month-year
		     (assign year q3 month q2 date q1)))))
	   (skip 5 "xx/yy/zz"))
	  (((position dy *relative-days*)  (*))
	   (setq state :date)
	   (let ((utime (+ (get-universal-time) (* (- dy 1) (* 24 60 60)))))
	     (multiple-value-bind (sec min hour dd mm yy)
		 (decode-universal-time utime)
	       (declare (ignore sec min hour))
	       (assign year yy month mm date dd)
	       (skip 1 "day"))))
	  (((:test (and month (not year))) (or (:digits y 2) (:digits y 4)) (*))
	   (setq state :date)
	   (when (<= 0 y 99) (incf y 1900))	;--- not actually right
	   (assign year y)
	   (skip 1 "yyyy"))
	  (((:test (not hour)) (:digits h 1 2) (or "am" "pm") (*))
	   (setq state :time)
	   (setq hour h minute 0 second 0)
	   (skip 1 "nn [am/pm]"))		;don't skip the AM/PM--it's processed later
	  (("(" (*))
	   (skip 1 "paren")
	   (let ((level 1))
	     (loop 
	       (match items
		 (("(") (incf level))
		 ((")") (decf level)))
	       (skip 1 "comment token")
	       (when (or (zerop level) (not items)) (return)))))
	  (("," (*))
	   (skip 1 "comma"))
	  (((:whitespace) (*))
	   (skip 1 "whitespace"))
	  (otherwise
           (fail)))
	(when (eq state :time)
	  (let ((adjustment nil))
	    (match items
	      (("pm" (*)) (setq adjustment :pm) (skip 1 "am/pm"))
	      (("am" (*)) (setq adjustment :am) (skip 1 "am/pm")))
	    (when adjustment
	      (when (or (not hour) (< hour 1) (> hour 12)) (fail))
	      (when (= hour 12) (setq hour 0))
	      (when (eq adjustment :pm) (incf hour 12))))
	  (let ((tz? nil) (minusp nil))
	    (match items
	      (("+" (:digits n 4) (*)) (setq tz? n minusp nil) (skip 2 "+nnnn"))
	      (("-" (:digits n 4) (*)) (setq tz? n minusp t)   (skip 2 "-nnnn"))
	      (("-" (assoc entry *timezones*) (*))
	       (setq tz (second entry) daylight (third entry))
	       (skip 2 "-timezone"))
	      (((assoc entry *timezones*) (*))
	       (setq tz (second entry) daylight (third entry))
	       (skip 1 "timezone")))
	    (when tz?
	      (multiple-value-bind (q r)
		  (truncate tz? 100)
		(when (or (> q 24) (> r 59)) (fail))
		(setq tz (+ q (/ r 60)))
		(when (not minusp) (setq tz (- tz)))))))))
    (multiple-value-bind (current-second current-minute current-hour
		          current-date current-month current-year)
	(get-decoded-time)
      (declare (ignore current-second current-minute current-hour))
      (values
	(apply #'encode-universal-time
	       (or second 0)
	       (or minute 0)
	       (or hour 0)
	       (or date current-date)
	       (or month current-month)
	       (or year current-year)
	       (when tz (list (if daylight (1- tz) tz))))
	(list :year year :month month :date date
	      :hour hour :minute minute :second second
	      :timezone tz :daylight daylight :day day)))))

(defun print-universal-time (universal-time 
                             &key (newline nil)
			          (show-seconds t)
				  (day-delimiter nil)
				  (date-format :short)
				  (day-format nil)
				  (show-timezone nil)
				  (stream *standard-output*)
				  (short-month-style :numeric)
				  (show-am/pm nil)
				  (padded t)
				  (short-year t)
				  (short-separator :slash))
  (multiple-value-bind (secs mins hrs date mon yr dow daylight-p timezone)
      (decode-universal-time universal-time)
    (when (and short-year (> yr 1900) (< yr 2000)) (decf yr 1900))
    (let ((tz (timezone-name timezone daylight-p))
	  (pad (if padded 2 nil))
	  n1 n2 n3 short-style (am/pm "am")
	  (short-month
	    (ecase short-month-style
	      (:numeric mon)
	      (:symbolic (month-name mon :format :short)))))
      (multiple-value-setq (short-style short-separator)
	(ecase short-separator
	  (:slash  (values *n1/n2/n3-meaning* #\/))
	  (:dot    (values *n1.n2.n3-meaning* #\.))
	  (:hyphen (values *n1-n2-n3-meaning* #\-))))
      (ecase short-style
	(:month-date-year (setq n1 short-month n2 date n3 yr))
	(:date-month-year (setq n1 date n2 short-month n3 yr))
	(:year-month-date (setq n1 yr n2 short-month n3 date)))
      (format stream "~:[~;~%~]~:[~*~;~:*~A~@[~C~] ~]"
	newline
	(when day-format (day-name dow :format day-format))
	(and day-format day-delimiter))
      (ecase date-format
	(:short (format stream "~V,'0D~C~V,'0D~C~2,'0D"
			pad n1 short-separator pad n2 short-separator n3))
	(:long (format stream "~A ~D, ~D" (month-name mon) date yr)))
      (when show-am/pm
	(when (> hrs 11)
	  (setq hrs (- hrs 12))
	  (setq am/pm "pm"))
	(when (= hrs 0) (setq hrs 12)))
      (format stream " ~V,'0D:~2,'0D~:[~*~;:~2,'0D~]~:[~*~;~A~]~@[ ~A~]"
	pad hrs mins show-seconds secs
	show-am/pm am/pm
	(when show-timezone tz)))))

;;--- Signal an error when any components are seen more than once
(defun parse-duration (string)
  (let (days weeks months years hours minutes seconds)
    (parse-string-tokens "duration" (string items)
      (loop
	(unless items (return))
	(match items
	  ((or ((:digits h 1 2) ":" (:digits m 2) ":" (:digits s 2) (*))
	       ((:digits h 1 2) ":" (:digits m 2) (*)))                
	   (when (or (> h 23) (> m 59) (and s (> s 59))) (fail))
	   (assign hours h minutes m seconds (or s 0))
	   (if s (skip 5 "hh:mm:ss") (skip 3 "hh:mm")))
	  ((or ((:digits d) (:whitespace) "hour" (*))
	       ((:digits d) (:whitespace) "hours" (*)))
	   (assign hours d)
	   (skip 3 "dd hours"))
	  ((or ((:digits d) (:whitespace) "minute" (*))
	       ((:digits d) (:whitespace) "minutes" (*)))
	   (assign minutes d)
	   (skip 3 "dd minutes"))
	  ((or ((:digits d) (:whitespace) "second" (*))
	       ((:digits d) (:whitespace) "seconds" (*)))
	   (assign seconds d)
	   (skip 3 "dd second"))
	  ((or ((:digits d) (:whitespace) "day" (*))
	       ((:digits d) (:whitespace) "days" (*)))
	   (assign days d)
	   (skip 3 "dd days"))
	  ((or ((:digits d) (:whitespace) "week" (*))
	       ((:digits d) (:whitespace) "weeks" (*)))
	   (assign weeks d)
	   (skip 3 "dd weeks"))
	  ((or ((:digits d) (:whitespace) "month" (*))
	       ((:digits d) (:whitespace) "months" (*)))
	   (assign months d )
	   (skip 3 "dd months"))
	  ((or ((:digits d) (:whitespace) "year" (*))
	       ((:digits d) (:whitespace) "years" (*)))
	   (assign years d)
	   (skip 3 "dd years"))
	  (((:whitespace) (*))
	   (skip 1 "whitespace"))
          (otherwise
	   (fail)))))
    (values 
      (+ (or seconds 0)
	 (* (or minutes 0)         60)
	 (* (or hours 0)        60 60)
	 (* (or days 0)      24 60 60)
	 (* (or weeks 0)   7 24 60 60)
	 (* (or months 0) 30 24 60 60)
	 (* (or years 0) 365 24 60 60))
      (list :years years :months months :weeks weeks :days days
	    :hours hours :minutes minutes :seconds seconds))))

