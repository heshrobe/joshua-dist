;;; -*- Mode: Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

(defmethod filling-stream-handle-line-break ((filling-stream filling-stream))
  (with-slots (current-width on-fresh-line) filling-stream
    (let ((stream (slot-value filling-stream 'stream))
          (prefix (slot-value filling-stream 'prefix))
          (prefix-width (slot-value filling-stream 'prefix-width)))
      ;; The "after line break" stuff is not a candidate for filling,
      ;; but it does contribute to the width of the line
      (when prefix
        (stream-write-string stream prefix))
      ;; This fixes the problem where a short string, terpri, long string sequence  <<<
      ;; causes a spurious extra blank line after the short string
      (setq on-fresh-line t) 
      (setq current-width (+ prefix-width
                             (text-size stream (filling-stream-buffer-to-string filling-stream)))))))

(defmethod stream-terpri ((filling-stream filling-stream))
  (let ((stream (slot-value filling-stream 'stream)))
    (filling-stream-write-buffer filling-stream t)
    (stream-terpri stream)
    (filling-stream-handle-line-break filling-stream)))

(defmethod stream-write-char ((filling-stream filling-stream) char)
  (with-slots (current-width) filling-stream
     (let* ((stream (slot-value filling-stream 'stream))
            (fill-width (slot-value filling-stream 'fill-width))
            (text-size (text-size stream char))
            (break-chars (slot-value filling-stream 'break-characters))
            (buffer (slot-value filling-stream 'buffer)))
       (cond ((or (eql char #\Newline)
                  (eql char #\Return))
              (stream-terpri filling-stream))
             ((and (> (+ current-width text-size) fill-width)
                   (member char break-chars))
	      ;; This fixes the problem where a break character
	      ;; never get output when there is a string longer than the fill-width
	      ;; delimited by a break which is also past the fill-width
	      (vector-push-extend char buffer);; <<<<
	      (incf current-width text-size)
              (stream-terpri filling-stream)
              )
             (t
              (vector-push-extend char buffer)
              (incf current-width text-size)
              ;; FILLING-STREAM-WRITE-BUFFER will only do something if we
              ;; are beyond the fill-width, so optimize calls to it
              (when (> current-width fill-width)
                (filling-stream-write-buffer filling-stream)))))))

(defmethod save-buffer-and-continue ((filling-stream filling-stream) original-stream continuation &rest continuation-args)
  ;; FixMe: maybe.  The only drawing option this handles is ink.
  ;; Note that "text-style" as a drawing option turns into "with-text-style" and is handled.
  ;; The other drawing options have to do with line size, shape, etc.  
  ;; It's not clear that there is an intereaction between those and filling-output.
  (let ((original-text-style (medium-merged-text-style original-stream))
        (recording-p (clim:stream-recording-p original-stream))
        (drawing-p (clim:stream-drawing-p original-stream))
	(ink (clim:medium-ink original-stream)))
    (labels ((flush-continuation (stream buffer start end)
	       ;; Note that "with-text-style" merges with but doesn't
	       ;; replace the text style with the original-text-style.  This would show up with sizes
	       ;; like :larger/:smaller and with faces like :bold and :italic mergind to (:bold :italic)
	       ;; We need instead to use left-globally.
               (letf-globally (((medium-merged-text-style stream) original-text-style))
		 (with-output-recording-options (stream :record recording-p :draw drawing-p)
		   (with-drawing-options (stream :ink ink) 
		     (flush-buffer-to-stream/filling stream buffer start end))))))
      (with-slots (buffer old-buffers stream) filling-stream
	(push (list buffer stream #'flush-continuation) old-buffers)
	(setf buffer (make-filling-stream-buffer))
	(apply continuation stream continuation-args)))))

(defmethod invoke-with-drawing-options ((filling-stream filling-stream) continuation &rest options)
  (let ((original-stream (slot-value filling-stream 'stream)))
    (labels ((filling-continuation ()
	       (multiple-value-prog1
		   (funcall continuation)
		 (stream-close-text-output-record filling-stream))))
      ;; Save-buffer-and-continue will call invoke-with-drawing-options
      ;; on the original-stream with filling-continuation as the continuation
      ;; and the original options as the arguments.  Filling continuation
      ;; then just calls the real body of the with-drawing-options and then
      ;; closes the text output record.
      (apply #'save-buffer-and-continue 
	     filling-stream original-stream
	     #'invoke-with-drawing-options
	     #'filling-continuation
	     options))))

(defmethod invoke-with-text-style ((filling-stream filling-stream)
                                   continuation style other-stream)
  (declare (ignore other-stream))
  (let ((original-stream (slot-value filling-stream 'stream)))
    (labels ((filling-continuation (stream)
	       (multiple-value-prog1
		   (funcall continuation stream)
		 (stream-close-text-output-record filling-stream))))
      (declare (dynamic-extent #'filling-continuation))
      (save-buffer-and-continue filling-stream original-stream 
				#'invoke-with-text-style
				#'filling-continuation style original-stream))))