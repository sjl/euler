(defpackage :euler/003 #.euler:*use*)
(in-package :euler/003)

;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143?

(define-problem (3 6857)
  (apply #'max (prime-factorization 600851475143)))

