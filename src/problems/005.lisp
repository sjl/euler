(defpackage :euler/005 #.euler:*use*)
(in-package :euler/005)


;; 2520 is the smallest number that can be divided by each of the numbers from
;; 1 to 10 without any remainder.
;;
;; What is the smallest positive number that is evenly divisible by all of the
;; numbers from 1 to 20?

(define-problem (5 232792560)
  (iterate
    ;; all numbers are divisible by 1 and we can skip checking everything <= 10
    ;; because:
    ;;
    ;; anything divisible by 12 is automatically divisible by 2
    ;; anything divisible by 12 is automatically divisible by 3
    ;; anything divisible by 12 is automatically divisible by 4
    ;; anything divisible by 15 is automatically divisible by 5
    ;; anything divisible by 12 is automatically divisible by 6
    ;; anything divisible by 14 is automatically divisible by 7
    ;; anything divisible by 16 is automatically divisible by 8
    ;; anything divisible by 18 is automatically divisible by 9
    ;; anything divisible by 20 is automatically divisible by 10
    (with divisors = (range 11 20))
    (for i :from 20 :by 20) ; it must be divisible by 20
    (finding i :such-that (every (curry #'dividesp i) divisors))))
