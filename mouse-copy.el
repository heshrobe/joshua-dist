;;; -*- Mode: Emacs-Lisp; outline-regexp: ";;;+ [^\n]\\|(" -*-
;;;;;; redshank.el --- Common Lisp Editing Extensions
;;; Extracted from Redshank by:
;; Copyright (C) 2006, 2007, 2008  Michael Weber

;; Author: Michael Weber <michaelw@foldr.org>
;; Keywords: languages, lisp

(defun region-active-p ()
  "Returns true if `transient-mark-mode' is used and region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(defun mouse-copy-thing-at-point (event)
  "Insert at point the syntactical element clicked on with the mouse.
Clicking on an open parenthesis inserts the whole form,
clicking on a symbol, number, string, etc., inserts it,
clicking within a (line) comment, inserts the comment up to the
end of the line.

When `transient-mark-mode' is enabled, and a region is
active, it is deleted.

This should be bound to a mouse click event type."
  (interactive "*e")
  (let* ((echo-keystrokes 0)
	 (start-posn (event-start event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn)))
    (let ((contents
           (with-current-buffer (window-buffer start-window)
             (save-excursion
               (goto-char start-point)
               (cond ((paredit-in-comment-p)
                      (skip-syntax-backward "^<")
                      (skip-syntax-backward "<")
                      (let ((comment.start (point)))
                        (end-of-line)
                        (buffer-substring comment.start (point))))
                     ((and (not (paredit-in-string-p))
                           (looking-at ";"))
                      (let ((comment.start (point)))
                        (end-of-line)
                        (buffer-substring comment.start (point))))
                     (t (thing-at-point 'sexp)))))))
      (cond ((and (stringp contents)
                  (not (equal "" contents)))
             (when (region-active-p)
               (delete-region (region-beginning) (region-end)))
             (unless (or (bolp)
                         (and (minibufferp)
                              (= (point) (minibuffer-prompt-end)))
                         (save-excursion
                           (backward-char)
                           (looking-at "\\s-\\|\\s\(")))
               (insert " "))
             (let ((contents.start (point)))
               (insert contents)
               (unless (or (eolp)
                           (and (minibufferp)
                                (= (point) (minibuffer-prompt-end)))
                           (looking-at "\\s-\\|\\s\)"))
                 (insert " "))
               (save-excursion
                 (goto-char contents.start)
                 (indent-sexp))))
            (t
             (message "Don't know what to copy?"))))))


