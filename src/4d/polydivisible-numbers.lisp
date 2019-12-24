(defpackage :euler/4d/polydivisible-numbers #.euler:*use*)
(in-package :euler/4d/polydivisible-numbers)

(setf lparallel:*kernel* (lparallel:make-kernel 30))

(deftype radix ()
  '(integer 2 64))

(deftype digit ()
  '(integer 1 63))

(deftype digit-set ()
  '(unsigned-byte 63))

(defun make-digits (radix)
  (1- (expt 2 (1- radix))))

(defun-inline remove-digit (d digits)
  (declare (optimize speed)
           (type digit d)
           (type digit-set digits))
  (dpb 0 (byte 1 (1- d)) digits))

(defun new-candidates (radix candidate remaining)
  (declare (optimize speed)
           (type radix radix)
           (type digit-set remaining)
           (type (integer 0) candidate))
  (iterate
    (declare (iterate:declare-variables)
             (type (or (integer 0) digit) d)
             (type digit-set n more))
    (for d :from 1)
    (for n :first remaining :then more)
    (for (values more d?) = (truncate n 2))
    (unless (zerop d?)
      (collect (cons (+ (* radix candidate) d)
                     (remove-digit d remaining))))
    (until (zerop more))))

(defun find-polydivisible-numbers (radix)
  (labels ((recur (n candidate remaining)
             (when (or (zerop n) (dividesp candidate n))
               (if (= n (1- radix))
                 (list candidate)
                 (funcall (if (< n 2)
                            #'lparallel:pmapcan
                            ;; #'mapcan
                            #'mapcan
                            )
                          (lambda (next)
                            (recur (1+ n) (car next) (cdr next)))
                          (new-candidates radix candidate remaining))))))
    (cons radix (recur 0 0 (make-digits radix)))))


