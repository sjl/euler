(defpackage :euler/009 #.euler:*use*)
(in-package :euler/009)

;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for
;; which:
;;
;;   a² + b² = c²
;;
;; For example, 3² + 4² = 9 + 16 = 25 = 5².
;;
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

(define-problem (9 31875000)
  (product (first (pythagorean-triplets-of-perimeter 1000))))

