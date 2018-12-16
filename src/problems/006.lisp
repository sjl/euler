(defpackage :euler/006 #.euler:*use*)
(in-package :euler/006)

;; The sum of the squares of the first ten natural numbers is,
;;   1² + 2² + ... + 10² = 385
;;
;; The square of the sum of the first ten natural numbers is,
;;   (1 + 2 + ... + 10)² = 55² = 3025
;;
;; Hence the difference between the sum of the squares of the first ten natural
;; numbers and the square of the sum is 3025 − 385 = 2640.
;;
;; Find the difference between the sum of the squares of the first one hundred
;; natural numbers and the square of the sum.

(define-problem (6 25164150)
  (flet ((sum-of-squares (to)
           (summation (irange 1 to) :key #'square))
         (square-of-sum (to)
           (square (summation (irange 1 to)))))
    (abs (- (sum-of-squares 100) ; apparently it wants the absolute value
            (square-of-sum 100)))))



