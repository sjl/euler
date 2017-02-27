(in-package :euler)

(define-constant carmichael-numbers ; from oeis
  '(561 1105 1729 2465 2821 6601 8911 10585 15841 29341 41041 46657 52633
    62745 63973 75361 101101 115921 126217 162401 172081 188461 252601 278545
    294409 314821 334153 340561 399001 410041 449065 488881 512461)
  :test #'equal)

(defun prime-factorization (n)
  "Return the prime factors of `n`."
  ;; from http://www.geeksforgeeks.org/print-all-prime-factors-of-a-given-number/
  (let ((result (list)))
    (iterate (while (evenp n)) ; handle 2, the only even prime factor
             (push 2 result)
             (setf n (/ n 2)))
    (loop :for i :from 3 :to (sqrt n) :by 2 ; handle odd (prime) divisors
          :do (iterate (while (dividesp n i))
                       (push i result)
                       (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push n result))
    (nreverse result)))


(defun expmod (base exp m)
  "Return base^exp % m quickly."
  ;; From SICP and
  ;; https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Common_Lisp
  ;;
  ;; We want to avoid bignums as much as possible.  This computes (base^exp % m)
  ;; without having to deal with huge numbers by taking advantage of the fact
  ;; that:
  ;;
  ;;     (x * y) % m
  ;;
  ;; is equivalent to:
  ;;
  ;;     ((x % m) * (y % m)) % m
  ;;
  ;; So for the cases where `exp` is even, we can split base^exp into an x and
  ;; y both equal to base^(exp/2) and use the above trick to handle them
  ;; separately.  Even better, we can just compute it once and square it.
  ;;
  ;; We also make it tail recursive by keeping a running accumulator:
  ;;
  ;;    base^exp * acc
  (labels
      ((recur (base exp acc)
         (cond
           ((zerop exp) acc)
           ((evenp exp)
            (recur (rem (square base) m)
                   (/ exp 2)
                   acc))
           (t
            (recur base
                   (1- exp)
                   (rem (* base acc) m))))))
    (recur base exp 1)))


(defun fermat-prime-p (n &optional (tests 10))
  "Return whether `n` might be prime.

  Checks `tests` times.

  If `t` is returned, `n` might be prime.  If `nil` is returned it is definitely
  composite.

  "
  (flet ((fermat-check (a)
           (= (expmod a n n) a)))
    (loop :repeat tests
          :when (not (fermat-check (random-range-exclusive 0 n)))
          :do (return nil)
          :finally (return t))))

(defun factor-out (n factor)
  "Factor the all the `factor`s out of `n`.

  Turns `n` into:

    factor^e * d

  where `d` is no longer divisible by `n`, and returns `e` and `d`.

  "
  (loop :for d = n :then (/ d factor)
        :for e = 0 :then (1+ e)
        :while (dividesp d factor)
        :finally (return (values e d))))

(defun miller-rabin-prime-p (n &optional (k 10))
  "Return whether `n` might be prime.

  If `t` is returned, `n` is probably prime.
  If `nil` is returned, `n` is definitely composite.

  "
  ;; https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Common_Lisp
  (cond
    ((< n 2) nil)
    ((< n 4) t)
    ((evenp n) nil)
    (t (multiple-value-bind (r d)
           (factor-out (1- n) 2)
         (flet ((strong-liar-p (a)
                  (let ((x (expmod a d n)))
                    (or (= x 1)
                        (loop :repeat r
                              :for y = x :then (expmod y 2 n)
                              :when (= y (1- n))
                              :do (return t))))))
           (loop :repeat k
                 :for a = (random-range-exclusive 1 (1- n))
                 :always (strong-liar-p a)))))))

(defun brute-force-prime-p (n)
  "Return (slowly) whether `n` is prime."
  (cond
    ((or (= n 0) (= n 1)) nil)
    ((= n 2) t)
    ((evenp n) nil)
    (t (loop :for divisor :from 3 :to (sqrt n)
          :when (dividesp n divisor)
          :do (return nil)
          :finally (return t)))))


(defun primep (n)
  "Return (less slowly) whether `n` is prime."
  (cond
    ;; short-circuit a few edge/common cases
    ((< n 2) nil)
    ((= n 2) t)
    ((evenp n) nil)
    ((< n 100000) (brute-force-prime-p n))
    (t (miller-rabin-prime-p n))))

(defun unitp (n)
  "Return whether `n` is 1."
  (= n 1))

(defun compositep (n)
  "Return whether `n` is composite."
  (and (not (unitp n))
       (not (primep n))))


(defun primes-below (n)
  "Return the prime numbers less than `n`."
  (cond
    ((<= n 2) (list))
    ((= n 3) (list 2))
    (t (cons 2 (loop :for i :from 3 :by 2 :below n
                     :when (primep i)
                     :collect i)))))

(defun primes-upto (n)
  "Return the prime numbers less than or equal to `n`."
  (primes-below (1+ n)))

(defun nth-prime (n)
  "Return the `n`th prime number."
  (if (= n 1)
    2
    (loop :with seen = 1
          :for i :from 3 :by 2
          :when (primep i)
          :do (progn
                (incf seen)
                (when (= seen n)
                  (return i))))))


(defun woodall (n)
  (1- (* n (expt 2 n))))

(defun woodall-prime-p (n)
  (primep (woodall n)))


(defun coprimep (a b)
  (= 1 (gcd a b)))

