(in-package #:euler)

(defun problem-1 ()
  (loop :for i :from 1 :below 1000
        :when (or (dividesp i 3)
                  (dividesp i 5))
        :sum i))

(defun problem-2 ()
  (loop :with p = 0
        :with n = 1
        :while (<= n 4000000)
        :when (evenp n) :sum n
        :do (psetf p n
                   n (+ p n))))

(defun problem-3 ()
  (apply #'max (prime-factorization 600851475143)))
