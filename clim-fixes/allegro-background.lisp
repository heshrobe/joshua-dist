;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: acl-clim; Base: 10; Lowercase: Yes -*-

(in-package :acl-clim)


#+mswindows
(defmethod initialize-instance :after ((port acl-port) &key)
  (with-slots (silica::default-palette silica::deep-mirroring vk->keysym) port
    ;; Start the scheduler as a workaround to bug7305.  Eventually,
    ;; we probably won't need this.  JPM 10/98.
    (mp:start-scheduler)
    (setf silica::default-palette (make-palette port :color-p t))
    (setf silica::deep-mirroring t)
    (register-window-class (realize-cursor port :default))
    ;;(get-clim-icon) +++
    (let ((res (ct:callocate :long)))
      (loop for (vk . keysym) in *vk->keysym* do
	    (setf (gethash vk vk->keysym) keysym))
      (loop for code from (char-code #\!) to (char-code #\~)
	  for char = (code-char code)
	  do
	    (let ((scan (loword (win:VkKeyScan code))))
	      (push char (gethash scan vk->keysym))))
      )
    ;; Panes that have a direct mirror will get initialized from
    ;; get-sheet-resources:
    (setf (port-default-resources port)
      `(:background 
	,+white+
	:foreground
	,(wincolor->color (win:GetSysColor win:COLOR_WINDOWTEXT))))
    ;; Panes that don't have a direct mirror will use this as
    ;; the default background:
    (setq silica:*default-pane-background* +ltgray+)
    ))

#+mswindows
(setq acl-clim::*windows-system-text-style* (clim:parse-text-style '(:Fix :roman :normal)))




#|
(defmethod get-sheet-resources :around ((port acl-clim::acl-port)
					(sheet t))
  (call-next-method))

(defmethod get-sheet-resources ((port acl-clim::acl-port)
                                (sheet sheet-with-resources-mixin))
  (list :background (clim:find-named-color  "linen" port)
	:text-style (parse-text-style '(:fix :roman :normal))))

|#