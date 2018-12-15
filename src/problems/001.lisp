(defpackage :euler/001 #.euler:*use*)
(in-package :euler/001)

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.

(define-problem (1 233168)
  (iterate (for i :from 1 :below 1000)
           (when (or (dividesp i 3)
                     (dividesp i 5))
             (summing i))))

