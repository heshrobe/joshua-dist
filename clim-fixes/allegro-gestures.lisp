;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gestures.lisp,v 1.20 92/12/03 10:26:44 cer Exp $

(in-package :clim-internals)

;; Returns T only for key press events
(defun-inline key-press-event-p (x)
  (or (characterp x)
      (typep x 'key-press-event)))

(defun-inline key-release-event-p (x)
  (typep x 'key-release-event))

#+allegro
(define-input-editor-gestures
  (:ie-show-arglist            :a   :control :shift)
  (:ie-show-value            :v   :control :shift)
  (:ie-show-documentation   :d   :control :shift))

;;; this one below is a fix to the problem that allegro freezes up
;;; when you type the : to begin a keyword arg to a clim command
;;; say in the lisp listener.
;;; The patch is the lines:
;;;	       (IF (NUMBERP TIMEOUT)
;;;		   (RETURN-FROM STREAM-READ-GESTURE
;;;		     (VALUES THING TYPE))
;;;                 NIL
;;; Which has to do with bizarre behavior in the event loop of the windows
;;; version of acl on PC's.  I don't really understand this, but it prevents
;;; an infinite loop in this method where if you get a timeout you would have
;;; always just eval'd to nil above rather than return but you'd keep getting the same thing
;;; back and keep looping forever.  Non PC versions of Allegro don't have this problem
;;; so they just always do the return-from.  On the pc version, always returning will 
;;; cause a bug in completion code, so this test seems to work at the moment.
#+(and allegro mswindows)
(defmethod stream-read-gesture ((istream input-editing-stream-mixin)
                                &key timeout peek-p
                                     (input-wait-test *input-wait-test*)
                                     (input-wait-handler *input-wait-handler*)
                                     (pointer-button-press-handler
                                       *pointer-button-press-handler*))
  (rescan-if-necessary istream t)
  (with-slots (stream input-buffer scan-pointer insertion-pointer
               activation-gesture rescanning-p
               numeric-argument previous-history) istream
    (declare (type fixnum scan-pointer insertion-pointer))
    (loop        ;until a real gesture is read or we throw out
      ;; First look in the input-buffer of the input-editor and see
      ;; if there's something there.
      (loop
        (cond ((= scan-pointer insertion-pointer)
               (return))
              ((< scan-pointer insertion-pointer)
               (let ((gesture (aref input-buffer scan-pointer)))
                 (cond ((characterp gesture)
                        (unless peek-p (incf scan-pointer))
                        (return-from stream-read-gesture (values gesture)))
                       (t (incf scan-pointer)))))
              (t (return)
                 ;; If the scan pointer is greater than the insertion pointer
                 ;; then we'll definitely have to rescan if a character is typed
                 ;; at this point.
                 )))

      ;; If we're about to go to the stream but there's an activation
      ;; character buffered, return it instead.
      (when activation-gesture
        (return-from stream-read-gesture
          (prog1 activation-gesture
                 (unless peek-p
                   (setf activation-gesture nil)))))

      ;;--- This is presumably much slower than necessary.
      ;;--- Perhaps there is a better way to keep track of where the cursor should be.
      (multiple-value-bind (x-pos y-pos)
          (input-buffer-input-position->cursor-position istream insertion-pointer)
        (declare (type coordinate x-pos y-pos))
        (multiple-value-bind (cx cy)
            (stream-cursor-position stream)
          (declare (type coordinate cx cy))
          ;; Don't set the cursor position if it's already right.
          ;; This prevents the input editor from scrolling the window after
          ;; the user has scrolled it back until the cursor position actually changes.
          (unless (and (= cx x-pos) (= cy y-pos))
            (ie-set-cursor-position istream x-pos y-pos))))

      (setf rescanning-p nil)
      (multiple-value-bind (thing type)
          (let* ((*input-buffer-empty* (zerop (fill-pointer input-buffer)))
                 (*accelerator-numeric-argument* (or numeric-argument 1))
                 (*accelerator-gestures*
                   ;; If there's anything in the input buffer, disallow accelerators
                   (and *input-buffer-empty* *accelerator-gestures*)))
            (stream-read-gesture stream
                                 :timeout timeout :peek-p peek-p
                                 :input-wait-test input-wait-test
                                 :input-wait-handler input-wait-handler
                                 :pointer-button-press-handler
                                 pointer-button-press-handler))
        (cond ((eq type ':timeout)
               #+(or aclpc acl86win32 mswindows)
	       (IF (NUMBERP TIMEOUT)
		   (RETURN-FROM STREAM-READ-GESTURE
		     (VALUES THING TYPE))
		 nil)
               #-(or aclpc acl86win32 mswindows)
               (return-from stream-read-gesture
                 (values thing type)))
              (peek-p
               (return-from stream-read-gesture
                 (values thing type)))
              (t
               (multiple-value-bind (new-thing new-type)
                   ;; This can throw out in order to do rescans.
                   ;; NEW-THING is a character, a presentation "blip", or NIL
                   (stream-process-gesture istream thing type)
                 (when (and (characterp new-thing)
                            ;; Don't put things in the buffer that we can't echo later
                            (or (ordinary-char-p new-thing)
                                (diacritic-char-p new-thing))
                            (not (activation-gesture-p new-thing)))
                   ;; If we are inserting multiple copies of this character
                   ;; we'll need to do a rescan in order to keep user-level
                   ;; buffers up-to-date
                   (let* ((count (or numeric-argument 1))
                          (immediate-rescan (> count 1)))
                     (dotimes (i count)
                       (cond ((< insertion-pointer (fill-pointer input-buffer))
                              (when (= i (1- count))        ;optimization
                                (erase-input-buffer istream insertion-pointer))
                              (setq input-buffer (shift-buffer-portion
                                                   input-buffer
                                                   insertion-pointer (1+ insertion-pointer)))
                              (setf (aref input-buffer insertion-pointer) new-thing)
                              (when (= i (1- count))        ;optimization
                                (redraw-input-buffer istream insertion-pointer))
                              (let ((rescan (> scan-pointer insertion-pointer)))
                                (incf insertion-pointer)
                                (if rescan
                                    (setq immediate-rescan t)
                                    (setf scan-pointer insertion-pointer))))
                             (t (vector-push-extend new-thing input-buffer)
                                (incf scan-pointer)
                                (incf insertion-pointer)
                                (write-char new-thing istream))))
                     (setq numeric-argument nil
                           previous-history nil)
                     (if immediate-rescan
                         (immediate-rescan istream)
                         (rescan-if-necessary istream t))
                     (return-from stream-read-gesture
                       (values new-thing new-type))))
                 (when new-thing
                   (setq numeric-argument nil
                         previous-history nil)
                   (cond ((activation-gesture-p new-thing)
                          ;; If we got an activation gesture, we must first finish
                          ;; scanning the input line, moving the insertion pointer
                          ;; to the end and finishing rescanning.  Only then can we
                          ;; return the activation gesture.
                          (cond ((= insertion-pointer (fill-pointer input-buffer))
                                 (return-from stream-read-gesture
                                   (values new-thing new-type)))
                                (t (setf insertion-pointer (fill-pointer input-buffer))
                                   (setf activation-gesture new-thing))))
                         ((or (not (characterp new-thing))
                              (ordinary-char-p new-thing)
                              (diacritic-char-p new-thing))
                          ;; There might be some queued up rescans from destructive
                          ;; input editing commands, so take care of them now
                          (rescan-if-necessary istream t)
                          (return-from stream-read-gesture
                            (values new-thing new-type)))
                         (t
                           ;; Some input editing doesn't throw, and should not
                           ;; cause us to return just yet, since IE commands don't
                           ;; count as real gestures.
                           (beep istream)))))))))))