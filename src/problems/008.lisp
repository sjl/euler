(defpackage :euler/008 #.euler:*use*)
(in-package :euler/008)

;; The four adjacent digits in the 1000-digit number that have the greatest
;; product are 9 × 9 × 8 × 9 = 5832.
;;
;; Find the thirteen adjacent digits in the 1000-digit number that have the
;; greatest product. What is the value of this product?

(define-problem (8 23514624000 )
  (let ((digits (map 'list #'digit-char-p
                     (remove #\newline
                             (read-file-into-string "data/008-number.txt")))))
    (iterate (for window :in (n-grams 13 digits))
             (maximize (apply #'* window)))))
