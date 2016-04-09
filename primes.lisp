(in-package #:euler)

(defun prime-factorization (n)
  "Return the prime factors of `n`."
  ;; from http://www.geeksforgeeks.org/print-all-prime-factors-of-a-given-number/
  (let ((result (list)))
    (while (evenp n) ; handle 2, the only even prime factor
      (push 2 result)
      (setf n (/ n 2)))
    (loop :for i :from 3 :to (sqrt n) :by 2 ; handle odd (prime) divisors
          :do (while (dividesp n i)
                (push i result)
                (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push n result))
    (nreverse result)))



