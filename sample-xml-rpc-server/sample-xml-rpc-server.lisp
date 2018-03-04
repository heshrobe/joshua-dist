;;; -*- mode:common-lisp; package: user -*-

(eval-when (:load-toplevel :compile-toplevel :execute) (require :xml-rpc))

(in-package :user)

(defparameter *default-port* 8082)

;;; This used to use a utility called osasubr (third party)
;;; The advance of osasubr over osascript (apple) is that until
;;; 10.4 or so, you couldn't pass arguments to osascript.
;;; But now you can, and osasubr seems to be available only in 
;;; PowerPC binary which won't run in Lion (10.7).
;;; So I've diked out the osasubr versions.

;;;
;;; This is the server side
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create the server
;;; It receives commmand then dispatches to the appropriate thing
;;; to execute on the server
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *script-server* nil)

(defun create-ascript-server (&optional (port *default-port*))
  (when *script-server*
    (net.xml-rpc:disable-xml-rpc-server *script-server*))
  (let ((s (net.xml-rpc:make-xml-rpc-server
	    :start nil 
	    :introspect t
	    :enable t
	    :publish `(:path "/ACL-XML-RPC2"))))
    ;; (net.xml-rpc:export-xml-rpc-method s '("runosasubr" run-osasubr t) :string :string)
    ;; At the moment the server does exactly one thing: execute a script remotely
    (net.xml-rpc:export-xml-rpc-method
     s
     '("runascript" run-ascript t)
     :string				;result spec
     :string :string :array		;rest arg for calling spec
     )
    (setq *script-server* s)
    (net.aserve:start :port port)
    (net.xml-rpc:enable-xml-rpc-server s)
    s))

;;; Command interface to the above
(clim-env:define-lisp-listener-command (com-create-script-server :name t)
    (&key (port 'integer :default *default-port*))
  (create-ascript-server port))

(clim-env:define-lisp-listener-command (com-disable-script-server :name t)
    ()
  (when *script-server*
    (net.xml-rpc:disable-xml-rpc-server *script-server*)))

; (defun run-osasubr (script-arguments)
;   (let ((command-line (concatenate 'string "osasubr " script-arguments)))
;     (format t "~%~s~%" command-line)
;     (excl:shell command-line)
;     command-line))

;;; Decode the command to run a script
;;; and then exec it on the server
(defun run-ascript (script-pathname function args)
  (let ((command (format nil "osascript ~a ~a ~{\"~a\"~^ ~}" script-pathname function args)))
    ;; (format t "~%~a" command)
    (excl:shell command)
    command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Client side
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *url-trailing-portion* "/ACL-XML-RPC2")

(defparameter *url-alist* nil)

(defun build-full-url (host &optional (port *default-port*))
  (format nil "http://~a:~d~a" host port *url-trailing-portion*))

(defun get-host-url (host &optional (port *default-port*))
  (let ((entry (assoc (list host port) *url-alist* :test #'equal)))
    (when (null entry)
      (setq entry (cons (list host port) (build-full-url host port)))
      (push entry *url-alist*))
    (cdr entry)))

;;; Makes the RPC call to the script server
(defun call-ascript-server (host port script function &rest args)
  (if (string-equal host "localhost")
      (run-ascript script function args)
    (net.xml-rpc:xml-rpc-call
     (net.xml-rpc:encode-xml-rpc-call "runascript" script function args)
     :url (get-host-url host port))))

; (defun call-osasubr-server (&key host (port *default-port*)
; 				 script
; 				 function
; 				 args)
;   (let* ((string-of-args (loop with answer = ""
; 			     for arg in args
; 			     do (setq answer (concatenate 'string answer " "
; 							  (string arg) ))
; 			     finally (return answer)))
; 	 (answer (concatenate 'string 
; 		   script " "
; 		   function " " string-of-args)))
;     (net.xml-rpc:xml-rpc-call
;      (net.xml-rpc:encode-xml-rpc-call "runosasubr" answer)
;      :url (get-host-url host port))))

(defun test-ascript-server (host &optional (port *default-port*))
  (net.xml-rpc:xml-rpc-call 
   (net.xml-rpc:encode-xml-rpc-call "system.listMethods")
   :url (get-host-url host port)))

(defparameter *host-alist* '(("OG5" . "og5.csail.mit.edu")
			     ("TIARA" "tiara.csail.mit.edu")
			     ("Local" . "localhost")
			     ("AIRE" . "mac-aire.csail.mit.edu")))

(defparameter *remote-scripts* '(("Itunes" . "itunes")
				 ("Powerpoint". "ppt-view-show")))

(defparameter *station-alist*
    `(("WBUR" . "http://wbur-sc.streamguys.com:80/")
      ("WUMB" . "http://wumb.streamguys1.com/wumb919fast")
      ("WGBH" . "http://64.71.145.107:8000")))

(clim-env:define-lisp-listener-command (com-play-radio :name t)
    ((station `(clim:member-alist ,*station-alist*))
     &key
     (host `(clim:member-alist ,*host-alist* :test string-equal)
	   :default "localhost" :provide-default t)
     (port 'integer :default *default-port* :provide-default t))
  (let ((file-name "~/scripts/itunes.scpt"))
    ;; (format t "~%Invoking script ~a on host ~a with arg ~a" file-name host "play")
    (call-ascript-server host port file-name "play" station)))

(clim-env:define-lisp-listener-command (com-stop-radio :name t)
    (
     &key
     (host `(clim:member-alist ,*host-alist* :test string-equal)
	   :default "localhost" :provide-default t)
     (port 'integer :default *default-port* :provide-default t))
  (let ((file-name "~/scripts/itunes.scpt"))
    ;; (format t "~%Invoking script ~a on host ~a port ~a with arg ~a" file-name host port "stop")
    (call-ascript-server host port file-name "stop" "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; More general interface to remote script server
;;; (does this still work?)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;z
  
(defparameter *script-to-function-and-args*
    '(("itunes" ("invoke") (member "play" "stop"))
      ("ppt-view-show" ("load_file") (clim-env::pathname) pathname-as-mac-namestring)
      ("WBUR" ("invoke") (member "play" "stop"))
      ))

; (clim-env::define-lisp-listener-command (com-run-remote-script :name t)
;     ((host `(clim:member-alist ,*host-alist* :test string-equal)
; 	   :default "localhost" :provide-default t)
;      (file-name `(clim:member-alist ,*remote-scripts* :test string-equal)
; 		:default "itunes" :provide-default t)
;      (function `(clim:member-sequence ,(second (assoc file-name *script-to-function-and-args* :test #'string-equal))
; 				      :test string-equal)
; 	       :default (first (second (assoc file-name *script-to-function-and-args* :test #'string-equal)))
; 	       :provide-default t)
;      (operands `(clim:sequence ,(third (assoc file-name *script-to-function-and-args* :test #'string-equal)))
; 	       )
;      &key
;      (port 'integer :default *default-port* :provide-default t)
;      (in-scripts-folder 'clim:boolean :default t))
;   (let* ((full-file-name
; 	  (if in-scripts-folder 
; 	      (concatenate 'string "/scripts/" file-name ".scpt")
; 	    file-name))
; 	 (coercer (fourth (assoc file-name *script-to-function-and-args* :test #'string-equal)))
; 	 (ops (loop for thing in operands
; 		  if coercer
; 		  collect (funcall coercer thing)
; 		  else collect (string thing))))
;     (format t "~%Invoking ~a in script ~a on host ~a port ~a with arg ~a"
; 	    function full-file-name host port ops)
;     (call-osasubr-server :host host :port port
; 			 :script full-file-name
; 			 :function function
; 			 :args ops)))

(defun pathname-as-mac-namestring (pathname)
  (subseq (substitute #\: #\/ (namestring pathname) :test #'char-equal) 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphical Radio Command Buttons
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun graphical-play-radio (pixmap stream &optional (station "wumb") x y)
  (let ((url (cdr (assoc station *station-alist* :test #'string-equal))))
    (clim:with-output-as-presentation 
	(stream `(com-play-radio ,url) 'clim:command :single-box t)
      (clim:display-clipped-pixmap pixmap 0 0 
				   :move-cursor? nil
				   :stream stream
				   :window-x x
				   :window-y y)
      (clim:draw-text* stream (string-upcase station)
		       (+ x (round (clim:pixmap-width pixmap) 2))
		       (+ y (round (clim:pixmap-height pixmap) 2))
		       :ink clim:+white+
		       :text-size :very-large
		       :text-face :bold
		       :align-x :center :align-y :center))))

(defun graphical-stop-radio (pixmap stream x y)
  (clim:with-output-as-presentation (stream 
				     '(com-stop-radio)
				     'clim:command
				     :single-box t)
    (multiple-value-bind (x y width height) 
	(clim:display-clipped-pixmap pixmap 0 0
				     :move-cursor? nil
				     :window-x x
				     :window-y y
				     :stream stream)
      (clim:draw-line* stream x y (+ x width) (+ y height)
		       :ink clim:+red+)
      (clim:draw-line* stream  (+ x width) y x (+ y height)
		       :ink clim:+red+))))

(defun graphical-radio-commands (pixmap stream)
  (multiple-value-bind (x y) (clim:stream-cursor-position stream)
    (graphical-play-radio pixmap stream "wumb" (+ x 10) y)
    (graphical-play-radio pixmap stream "wbur" (+ x (clim:pixmap-width pixmap) 30) y)
    (graphical-play-radio pixmap stream "wgbh" (+ x 10) (+ y (clim:pixmap-height pixmap) 20))
    (graphical-stop-radio pixmap stream (+ x (clim:pixmap-width pixmap) 30) (+ y (clim:pixmap-height pixmap) 20))
    ))

(defparameter *radio-icon* nil)
(defparameter *radio-pattern* nil)
(defparameter *radio-icon-pathname* "~/josh-dist/clim-fixes/radio.xpm")
(defparameter *radio-frame* nil)
(defparameter *radio-window* nil)


(defun radio-commands ()
  (multiple-value-setq (*radio-icon* *radio-pattern*)
    (clim:make-pixmap-and-pattern-from-file
     :pathname *radio-icon-pathname*
     :format :pixmap-3))
  (let ((graft (clim:find-graft)))
    (multiple-value-bind (sheet-width sheet-height) (clim:rectangle-size (clim:sheet-region graft))
      (declare (ignore sheet-width))
      (let ((window-width (+ 30 (* 2 (clim:pixmap-width *radio-icon*))))
            (window-height (+ 40 (* 2 (clim:pixmap-height *radio-icon*)))))
        (setq *radio-window* 
          (clim:open-window-stream 
           :label "Radio Controls"
           :left window-width
           :top (- sheet-height window-height 30)
           :width window-width
           :height window-height
           ;; Note that :Invisible is different than :off
           ;; and is necessary to avoid having a black box for the cursor
           :initial-cursor-visibility :invisible
           ;; :input-buffer (clim:stream-input-buffer *standard-input*)
           )
          *radio-frame* (clim:pane-frame *radio-window*))))
    (mp:process-run-function "radio-commands"
      #'(lambda (frame)
          ;; This is necessary before you can draw apparently
          (clim:window-expose *radio-window*)
          (graphical-radio-commands *radio-icon* *radio-window*)
          (clim:run-frame-top-level frame))
      *radio-frame*)
    ))

(defun kill-radio-commands ()
  (clim:destroy-frame *radio-frame*))

(clim-env::define-lisp-listener-command (com-show-radio-commands :name t)
    ()
  (radio-commands))


#||

Some notes about window exiting:
The way this works is that it starts another frame that has it's own process (using open-window-stream) and run-frame-top-level.
Then the exit box is handled in the normal way.

I think the commands work since they're presented as clim:command, not (clim:command :command-table 'clim-env::listener))


Another route would be for a minor change to the listener's top level handler as follows:

((frame-exit
  #'(lambda (condition)
      (let ((exit-frame (frame-exit-frame condition))
	    (options (frame-exit-options condition)))
	(setq destroy (getf options :destroy nil))
	(if (eq frame exit-frame)
	    (return-from run-frame-top-level nil)
	  (destroy-frame exit-frame)
	  )))))

||#