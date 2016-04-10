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


(defun fermat-prime-p (p &optional (tests 10))
  "Return whether `p` might be prime.

  Checks `tests` times.

  If `t` is returned, `p` might be prime.  If `nil` is returned it is definitely
  composite.

  "
  (if (or (zerop p) (= 1 p))
    nil
    (flet ((fermat-check (a)
             (= 1 (mod (expt a (1- p)) p))))
      (loop :repeat tests
            :when (not (fermat-check (random-exclusive 0 p)))
            :do (return nil)
            :finally (return t)))))

(defun brute-force-prime-p (p)
  "Return (slowly) whether `p` is prime."
  (cond
    ((or (= p 0) (= p 1)) nil)
    ((= p 2) t)
    ((evenp p) nil)
    (t (loop :for divisor :from 3 :to (sqrt p)
          :when (dividesp p divisor)
          :do (return nil)
          :finally (return t)))))


(defun primes-below (n)
  "Return (slowly) the prime numbers less than `n`."
  (cond
    ((<= n 2) (list))
    ((= n 3) (list 2))
    (t (cons 2 (loop :for i :from 3 :by 2 :below n
                     :when (brute-force-prime-p i)
                     :collect i)))))

(defun primes-upto (n)
  "Return (slowly) the prime numbers less than or equal to `n`."
  (primes-below (1+ n)))


(defun nth-prime (n)
  "Return (slowly) the `n`th prime number."
  (if (= n 1)
    2
    (loop :with seen = 1
          :for i :from 3 :by 2
          :when (brute-force-prime-p i)
          :do (progn
                (incf seen)
                (when (= seen n)
                  (return i))))))
