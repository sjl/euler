(in-package :euler)

(declaim (ftype (function ((integer 0 #.array-dimension-limit))
                          (simple-array bit (*)))
                sieve%))

(eval-dammit
  (defun sieve% (limit)
    "Return a bit vector of primality for all numbers below `limit`."
    (iterate
      (with numbers = (make-array limit :initial-element 1 :element-type 'bit))
      (for bit :in-vector numbers :with-index n :from 2)
      (when (= 1 bit)
        (iterate (for composite :from (* 2 n) :by n :below limit)
                 (setf (aref numbers composite) 0)))
      (finally
        (setf (aref numbers 0) 0
              (aref numbers 1) 0)
        (return numbers)))))

(defun sieve (limit)
  "Return a vector of all primes below `limit`."
  (declare (optimize speed))
  (iterate (declare (iterate:declare-variables))
           (for bit :in-vector (sieve% limit) :with-index n :from 2)
           (when (= 1 bit)
             (collect n :result-type vector))))


(defun prime-factors (n)
  "Return the prime factors of `n`.

  The result will be a set of primes.  For example:

    (prime-factors 60)
    ; =>
    (2 3 5)

  "
  (let ((result (list)))
    (iterate (while (evenp n)) ; handle 2, the only even prime factor
             (if-first-time
               (push 2 result))
             (setf n (/ n 2)))
    (iterate
      (for i :from 3 :to (sqrt n) :by 2) ; handle odd (prime) divisors
      (iterate (while (dividesp n i))
               (if-first-time
                 (push i result))
               (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push n result))
    (nreverse result)))

(defun prime-factorization (n)
  "Return the prime factorization of `n` as a flat list.

  The result will be a list of primes, with duplicates.  For example:

    (prime-factorization 60)
    ; =>
    (2 2 3 5)

  "
  ;; from http://www.geeksforgeeks.org/print-all-prime-factors-of-a-given-number/
  (let ((result (list)))
    (iterate (while (evenp n)) ; handle 2, the only even prime factor
             (push 2 result)
             (setf n (/ n 2)))
    (iterate
      (for i :from 3 :to (sqrt n) :by 2) ; handle odd (prime) divisors
      (iterate (while (dividesp n i))
               (push i result)
               (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push n result))
    (nreverse result)))

(defun prime-factorization-pairs (n &key include-zeros)
  "Return the prime factorization of `n` as a list of prime/exponent conses.

  The result will be a list of `(prime . exponent)` conses.  If `include-zeros`
  is given, even primes whose exponent turns out to be zero will be included.

  For example:

    (prime-factorization-pairs 60)
    ; =>
    ((2 . 2) (3 . 1) (5 . 1))

  "
  (let ((result (list)))
    (iterate (while (evenp n)) ; handle 2, the only even prime factor
             (else (when include-zeros
                     (push (cons 2 0) result)))
             (if-first-time
               (push (cons 2 1) result)
               (incf (cdar result)))
             (setf n (/ n 2)))
    (iterate
      (for i :from 3 :to (sqrt n) :by 2) ; handle odd (prime) divisors
      (iterate (while (dividesp n i))
               (else (when (and include-zeros (primep i))
                       (push (cons i 0) result)))
               (if-first-time
                 (push (cons i 1) result)
                 (incf (cdar result)))
               (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push (cons n 1) result))
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


(defun factor-out (n factor)
  "Factor the all the `factor`s out of `n`.

  Turns `n` into:

    factor^e * d

  where `d` is no longer divisible by `n`, and returns `e` and `d`.

  "
  (iterate (for d :initially n :then (/ d factor))
           (for e :initially 0 :then (1+ e))
           (while (dividesp d factor))
           (finally (return (values e d)))))

(defun miller-rabin-prime-p (n &optional (k 11))
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
                        (iterate (repeat r)
                                 (for y :initially x :then (expmod y 2 n))
                                 (when (= y (1- n))
                                   (return t)))))))
           (iterate (repeat k)
                    (for a = (random-range-exclusive 1 (1- n)))
                    (always (strong-liar-p a))))))))

(defun brute-force-prime-p (n)
  "Return (slowly) whether `n` is prime."
  (cond
    ((or (= n 0) (= n 1)) nil)
    ((= n 2) t)
    ((evenp n) nil)
    (t (iterate (for divisor :from 3 :to (sqrt n))
                (when (dividesp n divisor)
                  (return nil))
                (finally (return t))))))


;;;; Precomputation -----------------------------------------------------------
;;; We precompute a bit vector of the primality of the first hundred million
;;; numbers to make checking primes faster.  It'll take up about 12 mb of memory
;;; and take a few seconds to compute, but saves a lot of brute forcing later.

(defconstant +precomputed-primality-limit+ 100000000)

(defparameter *precomputed-primality-bit-vector*
  (sieve% +precomputed-primality-limit+))

(deftype precomputed-primality-bit-vector ()
  `(simple-array bit (,+precomputed-primality-limit+)))

(deftype integer-with-precomputed-primality ()
  `(integer 0 (,+precomputed-primality-limit+)))

(defun-inline precomputed-prime-p% (n)
  (declare (optimize speed (debug 0) (safety 1))
           (type integer-with-precomputed-primality n)
           (type precomputed-primality-bit-vector *precomputed-primality-bit-vector*))
  (not (zerop (aref *precomputed-primality-bit-vector* n))))


(defun primep (n)
  "Return (less slowly) whether `n` is prime."
  (cond
    ;; short-circuit a few edge/common cases
    ((< n 2) nil)
    ((= n 2) t)
    ((evenp n) nil)
    ((< n +precomputed-primality-limit+) (precomputed-prime-p% n))
    (t (miller-rabin-prime-p n))))

(defun unitp (n)
  "Return whether `n` is 1."
  (= n 1))

(defun compositep (n)
  "Return whether `n` is composite."
  (and (not (unitp n))
       (not (primep n))))


(defun primes% (start end)
  (assert (<= start end))
  (if (= start end)
    nil
    (let ((odd-primes (iterate (for i :from (if (oddp start)
                                              start
                                              (1+ start))
                                    :by 2 :below end)
                               (when (primep i)
                                 (collect i)))))
      (if (<= start 2)
        (cons 2 odd-primes)
        odd-primes))))

(defun primes-below (n)
  "Return the prime numbers less than `n`."
  (primes% 2 n))

(defun primes-upto (n)
  "Return the prime numbers less than or equal to `n`."
  (primes% 2 (1+ n)))

(defun primes-in (min max)
  "Return the prime numbers `p` such that `min` <= `p` <= `max`."
  (primes% min (1+ max)))

(defun primes-between (min max)
  "Return the prime numbers `p` such that `min` < `p` < `max`."
  (primes% (1+ min) max))


(defun-inline next-prime% (p)
  (case p
    ((nil) 2)
    (2 3)
    (t (iterate (for i :from (+ p 2) :by 2)
                (finding i :such-that #'primep)))))

(defmacro-driver (FOR var IN-PRIMES _)
  (declare (ignore _))
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,var :next (next-prime% ,var))))


(defun nth-prime (n)
  "Return the `n`th prime number."
  (if (= n 1)
    2
    (iterate
      (with seen = 1)
      (for i :from 3 :by 2)
      (when (primep i)
        (incf seen)
        (when (= seen n)
          (return i))))))


(defun woodall (n)
  (1- (* n (expt 2 n))))

(defun woodall-prime-p (n)
  (primep (woodall n)))


(defun coprimep (a b)
  (= 1 (gcd a b)))

