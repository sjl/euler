(defpackage :euler/004 #.euler:*use*)
(in-package :euler/004)

;; A palindromic number reads the same both ways. The largest palindrome made
;; from the product of two 2-digit numbers is 9009 = 91 × 99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.

(define-problem (4 906609)
  (iterate
    ;; We COULD brute force this, but it's more fun to do it smartly.
    (with result = 0)
    (for i :from 999 :downto 100)
    (iterate (for j :from i :downto 100)
             (for product = (* i j))
             ;; Because we're counting down, we can break in this inner loop as
             ;; soon as our products drop below the current maximum.
             (while (>= product (or result 0)))
             (when (palindromep product)
               (setf result product)))
    (finally (return result))))


