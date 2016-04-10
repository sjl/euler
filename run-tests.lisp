(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "euler"))


(defvar *passed* t)

(defun test (spec)
  (let ((result (5am:run spec)))
    (5am:explain! result)
    (when (not (5am:results-status result))
      (setf *passed* nil))))

(test :euler)

(sb-ext:exit :code (if *passed* 0 1))
