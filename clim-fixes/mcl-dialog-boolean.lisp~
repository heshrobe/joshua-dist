(defun testx ()
  (let ((stream *standard-output*))
    (clim:accepting-values (stream :own-window t :label "What is it?")
      (clim:accept 'clim:boolean 
                   :prompt "Is it a foo or a bar or something even very much bigger?"
                   :stream stream))))

(in-package :ccl)
(defmethod view-default-size ((item check-box-dialog-item))
  (let ((size (call-next-method)))
    (make-point (+ (if (osx-p) 12 4) (point-h size))
                (+ (if (osx-p) 4 2) (point-v size)))))