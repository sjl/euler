(in-package :euler)

;;;; Utils --------------------------------------------------------------------
(defmacro-driver (FOR var ITERATING function INITIALLY value)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (f)
      `(progn
         (with ,f = ,function)
         (,kwd ,var
          :initially (funcall ,f ,value)
          :then (funcall ,f ,var))))))

(defmacro-driver (FOR var IN-LOOPING list)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (l remaining)
      `(progn
         (with ,l = ,list)
         (,kwd (,var . ,remaining)
          :next (if-first-time
                  ,l
                  (if (null ,remaining)
                    ,l
                    ,remaining)))))))

(defmacro-driver (FOR var KEY function &sequence)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (i f)
      `(progn
         (with ,f = ,function)
         (generate ,i ,@(losh::expand-iterate-sequence-keywords))
         (,kwd ,var :next (funcall ,f (next ,i)))))))


(defmacro-driver (FOR var IN-DIGITS-OF integer &optional RADIX (radix 10))
  "Iterate `var` through the digits of `integer` in base `radix`, low-order first."
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (i r remaining digit)
      `(progn
         (with ,r = ,radix)
         (with ,i = (abs ,integer))
         (,kwd ,var :next (if (zerop ,i)
                            (terminate)
                            (multiple-value-bind (,remaining ,digit)
                                (truncate ,i ,r)
                              (setf ,i ,remaining)
                              ,digit)))))))

(defun digits (n &optional (radix 10))
  "Return a fresh list of the digits of `n` in base `radix`."
  (iterate (for d :in-digits-of n :radix radix)
           (collect d :at :beginning)))

(defun digits-length (n &optional (radix 10))
  "Return how many digits `n` has in base `radix`."
  (if (zerop n)
    1
    (values (1+ (truncate (log (abs n) radix))))))


(defun digits-to-number (digits)
  (if digits
    (reduce (lambda (total digit)
              (+ (* total 10) digit))
            digits)
    0))

(defun extremely-fucking-unsafe-digits-to-number (digits)
  (declare (optimize (speed 3) (safety 0)))
  (if digits
    (iterate
      (declare (iterate:declare-variables))
      (with (the (unsigned-byte 62) result) = 0)
      (for (the (integer 0 9) d) :in digits)
      (setf result (the (unsigned-byte 64) (mod (* result 10) (expt 2 62)))
            result (the (unsigned-byte 64) (mod (+ result d) (expt 2 62))))
      (finally (return result)))
    0))


(defun palindromep (n &optional (radix 10))
  "Return whether `n` is a palindrome in base `radix`."
  (let ((s (format nil "~VR" radix n)))
    (string= s (reverse s))))


(defun sum (sequence &key key)
  (iterate (for n :in-whatever sequence)
           (sum (if key
                  (funcall key n)
                  n))))

(defun product (sequence &key key)
  (iterate (for n :in-whatever sequence)
           (multiplying (if key
                          (funcall key n)
                          n))))


(defun sort< (sequence)
  (sort sequence #'<))


(defun divisors (n)
  (sort< (iterate (for i :from 1 :to (sqrt n))
                 (when (dividesp n i)
                   (collect i)
                   (let ((j (/ n i)))
                     ;; don't collect the square root twice
                     (unless (= i j)
                       (collect j)))))))

(defun proper-divisors (n)
  (remove n (divisors n)))

(defun count-divisors (n)
  (* 2 (iterate (for i :from 1 :to (sqrt n))
                (counting (dividesp n i)))))


(defmacro-driver (FOR var IN-COLLATZ n)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd ,var :next (cond ((null ,var) ,n)
                              ((= 1 ,var) (terminate))
                              ((evenp ,var) (/ ,var 2))
                              (t (1+ (* 3 ,var))))))))

(defun collatz (n)
  (iterate (for i :in-collatz n)
           (collect i)))

(defun collatz-length (n)
  (iterate (for i :in-collatz n)
           (counting t)))


(defmacro-driver (FOR var IN-FIBONACCI _)
  (declare (ignore _))
  (with-gensyms (a b)
    (let ((kwd (if generate 'generate 'for)))
      `(progn
         (with ,a = 0)
         (with ,b = 1)
         (,kwd ,var :next (prog1 ,b
                            (psetf ,a ,b
                                   ,b (+ ,a ,b))))))))

(defun fibonacci (n)
  "Return the first `n` Fibonacci numbers as a fresh list."
  (iterate (repeat n)
           (for i :in-fibonacci t)
           (collect i)))


(defun binomial-coefficient (n k)
  "Return `n` choose `k`."
  ;; https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  (iterate (for i :from 1 :to k)
           (multiply (/ (+ n 1 (- i))
                        i))))


(defun factorial (n)
  (iterate (for i :from 1 :to n)
           (multiplying i)))


(defun perfectp (n)
  (= n (sum (proper-divisors n))))

(defun abundantp (n)
  (< n (sum (proper-divisors n))))

(defun deficientp (n)
  (> n (sum (proper-divisors n))))


(defun multiplicative-order (integer modulus)
  "Return the multiplicative order of `integer` modulo `modulus`."
  ;; https://en.wikipedia.org/wiki/Multiplicative_order
  (iterate (for i :from 1)
           (for v :first integer :then (* v integer))
           (finding i :such-that (= 1 (mod v modulus)))))


(defun number-spiral-corners (size)
  "Return a list of the corner values of a 'number spiral' of size `size`.

  `size` must be odd.  The order of the corners in the resulting list is
  unspecified.

  Note that the \"spiral\" of size one has just a single \"corner\": `1`.

  "
  (assert (oddp size))
  (if (= 1 size)
    (list 1)
    (let ((leg (1- size))
          (final (square size)))
      (list (- final (* 0 leg))
            (- final (* 1 leg))
            (- final (* 2 leg))
            (- final (* 3 leg))))))


(defun truncate-number-left (n amount &optional (radix 10))
  "Chop `amount` digits off the left side of `n` in base `radix`."
  (mod n (expt radix (- (digits-length n radix) amount))))

(defun truncate-number-right (n amount &optional (radix 10))
  "Chop `amount` digits off the right side of `n` in base `radix`."
  (truncate n (expt radix amount)))


(defun hex (n)
  (format t "~X" n)
  (values))


(defun concatenate-integers (&rest integers)
  "Concatenate each integer in `integers` and return a big ol' integer result."
  (values (parse-integer
            (format nil "~{~D~}" integers))))


(defun pandigitalp (integer &key (start 1) (end 9))
  "Return whether `integer` is `start` to `end` (inclusive) pandigital.

  Examples:

    (pandigitalp 123)     ; => nil
    (pandigitalp 123 1 3) ; => t
    (pandigitalp 123 0 3) ; => nil

  "
  (equal (irange start end)
         (sort< (digits integer))))

(defun pandigitals (&optional (start 1) (end 9))
  "Return a list of all `start` to `end` (inclusive) pandigital numbers."
  (gathering
    (map-permutations (lambda (digits)
                        ;; 0-to-n pandigitals are annoying because we don't want
                        ;; to include those with a 0 first.
                        (unless (zerop (first digits))
                          (gather (digits-to-number digits))))
                      (irange start end)
                      :copy nil)))


(defun permutations (sequence &key length)
  (gathering (map-permutations #'gather sequence :length length)))

(defun combinations (sequence &key length)
  (gathering (map-combinations #'gather sequence :length length)))


(defun-inline digits< (n digits)
  "Return whether `n` has fewer than `digits` digits."
  (< (abs n) (expt 10 (1- digits))))

(defun-inline digits<= (n digits)
  "Return whether `n` has `digits` or fewer digits."
  (< (abs n) (expt 10 digits)))


(defun adjoin% (list item &rest keyword-args)
  (apply #'adjoin item list keyword-args))

(define-modify-macro adjoinf (item &rest keyword-args) adjoin%)


(defun mv* (matrix vector)
  (iterate
    (with (rows cols) = (array-dimensions matrix))
    (initially (assert (= cols (length vector))))
    (with result = (make-array rows :initial-element 0))
    (for row :from 0 :below rows)
    (iterate (for col :from 0 :below cols)
             (for v = (aref vector col))
             (for a = (aref matrix row col))
             (incf (aref result row)
                   (* v a)))
    (finally (return result))))


(defun pythagorean-triplet-p (a b c)
  (= (+ (square a) (square b))
     (square c)))

(defun pythagorean-triplets-of-perimeter (p)
  (iterate
    (with result = '())
    (for c :from 1 :to (- p 2))
    (iterate
      (for a :from 1 :below (min c (- p c)))
      (for b = (- p c a))
      (when (pythagorean-triplet-p a b c)
        (adjoinf result (sort< (list a b c))
                 :test #'equal)))
    (finally (return result))))


(defun map-primitive-pythagorean-triplets (function stop-predicate)
  ;; http://mathworld.wolfram.com/PythagoreanTriple.html
  (let ((u #2A(( 1  2  2)
               (-2 -1 -2)
               ( 2  2  3)))
        (a #2A(( 1  2  2)
               ( 2  1  2)
               ( 2  2  3)))
        (d #2A((-1 -2 -2)
               ( 2  1  2)
               ( 2  2  3))))
    (recursively ((triple (vector 3 4 5)))
      (unless (apply stop-predicate (coerce triple 'list))
        (apply function (coerce triple 'list))
        (recur (mv* u triple))
        (recur (mv* a triple))
        (recur (mv* d triple))))))


(defun squarep (n)
  "Return whether `n` is a perfect square."
  (and (integerp n)
       (= n (square (isqrt n)))))


(defun cube (n)
  (* n n n))


(eval-dammit
  (defun build-cube-array ()
    ;; http://stackoverflow.com/a/32017647
    (iterate
      (with arr = (make-array 819 :initial-element nil))
      (for mod in '(0  125  181  818  720  811  532  755  476
                     1  216   90  307  377  694  350  567  442
                     8  343  559  629  658  351  190   91  469
                     27  512  287  252  638  118  603  161  441
                     64  729   99  701  792  378  260  468  728))
      (setf (aref arr mod) t)
      (finally (return arr)))))

(defun slow-cubep (n)
  (= n (cube (truncate (expt n 1/3)))))

(defun cubep (n)
  (and (integerp n)
       (svref #.(build-cube-array) (mod n 819))
       (slow-cubep n)))


(defun triangle (n)
  "Return the `n`th triangle number (1-indexed because mathematicians are silly)."
  (* 1/2 n (1+ n)))

(defun trianglep (n)
  "Return whether `n` is a triangle number."
  ;; http://mathforum.org/library/drmath/view/57162.html
  ;;
  ;; A number is triangular if and only if 8T + 1 is an odd perfect square.
  (let ((x (1+ (* 8 n))))
    (and (oddp x)
         (squarep x))))


(defun pentagon (n)
  (* n (- (* 3 n) 1) 1/2))

(defun pentagonp (n)
  ;; We can ignore the - branch of the quadratic equation because negative
  ;; numbers aren't indexes.
  (dividesp (+ 1 (sqrt (1+ (* 24.0d0 n)))) 6))


(defun hexagon (n)
  (* n (1- (* 2 n))))

(defun hexagonp (n)
  ;; We can ignore the - branch of the quadratic equation because negative
  ;; numbers aren't indexes.
  (dividesp (+ 1 (sqrt (1+ (* 8.0d0 n)))) 4))


(defun heptagon (n)
  (* n (- (* 5 n) 3) 1/2))

(defun octagon (n)
  (* n (- (* 3 n) 2)))


(defun parse-strings-file (filename)
  (-<> filename
    read-file-into-string
    (substitute #\Space #\, <>)
    read-all-from-string))


(defun letter-number (char)
  "Return the index of `char` in the alphabet (A being 1)."
  (1+ (- (char-code (char-upcase char)) (char-code #\A))))


(defun set-equal (list1 list2 &rest args)
  (null (apply #'set-exclusive-or list1 list2 args)))

(defun orderless-equal (list1 list2 &key (sort-predicate #'<))
  (equal (sort (copy-seq list1) sort-predicate)
         (sort (copy-seq list2) sort-predicate)))


(defun irange (start end &key (step 1) (key 'identity))
  "Inclusive `range`."
  (range start (1+ end) :step step :key key))


(defun length= (n sequence)
  (= n (length sequence)))


(defun reverse-integer (n)
  (digits-to-number (nreverse (digits n))))


(defmacro labels-memoized (definitions &body body)
  (let ((caches (mapcar #'gensym (range 0 (length definitions)))))
    (flet ((build (cache definition)
             (destructuring-bind (name lambda-list &body body) definition
               `(,name ,lambda-list
                 (values
                   (ensure-gethash (list ,@lambda-list) ,cache
                                   (progn ,@body)))))))
      `(let (,@(iterate (for cache :in caches)
                        (collect `(,cache (make-hash-table :test #'equal)))))
         (labels (,@(mapcar #'build caches definitions))
           ,@body)))))



;;;; Problems -----------------------------------------------------------------
(defun problem-1 ()
  ;; If we list all the natural numbers below 10 that are multiples of 3 or 5,
  ;; we get 3, 5, 6 and 9. The sum of these multiples is 23.
  ;;
  ;; Find the sum of all the multiples of 3 or 5 below 1000.
  (iterate (for i :from 1 :below 1000)
           (when (or (dividesp i 3)
                     (dividesp i 5))
             (sum i))))

(defun problem-2 ()
  ;; Each new term in the Fibonacci sequence is generated by adding the previous
  ;; two terms. By starting with 1 and 2, the first 10 terms will be:
  ;;
  ;;     1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
  ;;
  ;; By considering the terms in the Fibonacci sequence whose values do not
  ;; exceed four million, find the sum of the even-valued terms.
  (iterate (for n :in-fibonacci t)
           (while (<= n 4000000))
           (when (evenp n)
             (sum n))))

(defun problem-3 ()
  ;; The prime factors of 13195 are 5, 7, 13 and 29.
  ;;
  ;; What is the largest prime factor of the number 600851475143 ?
  (apply #'max (prime-factorization 600851475143)))

(defun problem-4 ()
  ;; A palindromic number reads the same both ways. The largest palindrome made
  ;; from the product of two 2-digit numbers is 9009 = 91 × 99.
  ;;
  ;; Find the largest palindrome made from the product of two 3-digit numbers.
  (iterate (for-nested ((i :from 0 :to 999)
                        (j :from 0 :to 999)))
           (for product = (* i j))
           (when (palindromep product)
             (maximize product))))

(defun problem-5 ()
  ;; 2520 is the smallest number that can be divided by each of the numbers from
  ;; 1 to 10 without any remainder.
  ;;
  ;; What is the smallest positive number that is evenly divisible by all of the
  ;; numbers from 1 to 20?
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
    (with divisors = (irange 11 20))
    (for i :from 20 :by 20) ; it must be divisible by 20
    (finding i :such-that (every (lambda (n) (dividesp i n))
                                 divisors))))

(defun problem-6 ()
  ;; The sum of the squares of the first ten natural numbers is,
  ;;   1² + 2² + ... + 10² = 385
  ;;
  ;; The square of the sum of the first ten natural numbers is,
  ;;   (1 + 2 + ... + 10)² = 55² = 3025
  ;;
  ;; Hence the difference between the sum of the squares of the first ten
  ;; natural numbers and the square of the sum is 3025 − 385 = 2640.
  ;;
  ;; Find the difference between the sum of the squares of the first one hundred
  ;; natural numbers and the square of the sum.
  (flet ((sum-of-squares (to)
           (sum (irange 1 to :key #'square)))
         (square-of-sum (to)
           (square (sum (irange 1 to)))))
    (abs (- (sum-of-squares 100) ; apparently it wants the absolute value
            (square-of-sum 100)))))

(defun problem-7 ()
  ;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
  ;; that the 6th prime is 13.
  ;;
  ;; What is the 10 001st prime number?
  (nth-prime 10001))

(defun problem-8 ()
  ;; The four adjacent digits in the 1000-digit number that have the greatest
  ;; product are 9 × 9 × 8 × 9 = 5832.
  ;;
  ;; Find the thirteen adjacent digits in the 1000-digit number that have the
  ;; greatest product. What is the value of this product?
  (let ((digits (map 'list #'digit-char-p
                     "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")))
    (iterate (for window :in (n-grams 13 digits))
             (maximize (apply #'* window)))))

(defun problem-9 ()
  ;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for
  ;; which:
  ;;
  ;;   a² + b² = c²
  ;;
  ;; For example, 3² + 4² = 9 + 16 = 25 = 5².
  ;;
  ;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  ;; Find the product abc.
  (product (first (pythagorean-triplets-of-perimeter 1000))))

(defun problem-10 ()
  ;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  ;; Find the sum of all the primes below two million.
  (sum (sieve 2000000)))

(defun problem-11 ()
  ;; In the 20×20 grid below, four numbers along a diagonal line have been marked
  ;; in red.
  ;;
  ;; The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
  ;;
  ;; What is the greatest product of four adjacent numbers in the same direction
  ;; (up, down, left, right, or diagonally) in the 20×20 grid?
  (let ((grid
          #2A((08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
              (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
              (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
              (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
              (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
              (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
              (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
              (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
              (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
              (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
              (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
              (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
              (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
              (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
              (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
              (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
              (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
              (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
              (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
              (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48))))
    (max
      ;; horizontal
      (iterate (for-nested ((row :from 0 :below 20)
                            (col :from 0 :below 16)))
               (maximize (* (aref grid row (+ 0 col))
                            (aref grid row (+ 1 col))
                            (aref grid row (+ 2 col))
                            (aref grid row (+ 3 col)))))
      ;; vertical
      (iterate (for-nested ((row :from 0 :below 16)
                            (col :from 0 :below 20)))
               (maximize (* (aref grid (+ 0 row) col)
                            (aref grid (+ 1 row) col)
                            (aref grid (+ 2 row) col)
                            (aref grid (+ 3 row) col))))
      ;; backslash \
      (iterate (for-nested ((row :from 0 :below 16)
                            (col :from 0 :below 16)))
               (maximize (* (aref grid (+ 0 row) (+ 0 col))
                            (aref grid (+ 1 row) (+ 1 col))
                            (aref grid (+ 2 row) (+ 2 col))
                            (aref grid (+ 3 row) (+ 3 col)))))
      ;; slash /
      (iterate (for-nested ((row :from 3 :below 20)
                            (col :from 0 :below 16)))
               (maximize (* (aref grid (- row 0) (+ 0 col))
                            (aref grid (- row 1) (+ 1 col))
                            (aref grid (- row 2) (+ 2 col))
                            (aref grid (- row 3) (+ 3 col))))))))

(defun problem-12 ()
  ;; The sequence of triangle numbers is generated by adding the natural
  ;; numbers. So the 7th triangle number would be
  ;; 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
  ;;
  ;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
  ;;
  ;; Let us list the factors of the first seven triangle numbers:
  ;;
  ;;  1: 1
  ;;  3: 1,3
  ;;  6: 1,2,3,6
  ;; 10: 1,2,5,10
  ;; 15: 1,3,5,15
  ;; 21: 1,3,7,21
  ;; 28: 1,2,4,7,14,28
  ;;
  ;; We can see that 28 is the first triangle number to have over five divisors.
  ;;
  ;; What is the value of the first triangle number to have over five hundred
  ;; divisors?
  (iterate
    (for tri :key #'triangle :from 1)
    (finding tri :such-that (> (count-divisors tri) 500))))

(defun problem-13 ()
  ;; Work out the first ten digits of the sum of the following one-hundred
  ;; 50-digit numbers.
  (-<> (+ 37107287533902102798797998220837590246510135740250
          46376937677490009712648124896970078050417018260538
          74324986199524741059474233309513058123726617309629
          91942213363574161572522430563301811072406154908250
          23067588207539346171171980310421047513778063246676
          89261670696623633820136378418383684178734361726757
          28112879812849979408065481931592621691275889832738
          44274228917432520321923589422876796487670272189318
          47451445736001306439091167216856844588711603153276
          70386486105843025439939619828917593665686757934951
          62176457141856560629502157223196586755079324193331
          64906352462741904929101432445813822663347944758178
          92575867718337217661963751590579239728245598838407
          58203565325359399008402633568948830189458628227828
          80181199384826282014278194139940567587151170094390
          35398664372827112653829987240784473053190104293586
          86515506006295864861532075273371959191420517255829
          71693888707715466499115593487603532921714970056938
          54370070576826684624621495650076471787294438377604
          53282654108756828443191190634694037855217779295145
          36123272525000296071075082563815656710885258350721
          45876576172410976447339110607218265236877223636045
          17423706905851860660448207621209813287860733969412
          81142660418086830619328460811191061556940512689692
          51934325451728388641918047049293215058642563049483
          62467221648435076201727918039944693004732956340691
          15732444386908125794514089057706229429197107928209
          55037687525678773091862540744969844508330393682126
          18336384825330154686196124348767681297534375946515
          80386287592878490201521685554828717201219257766954
          78182833757993103614740356856449095527097864797581
          16726320100436897842553539920931837441497806860984
          48403098129077791799088218795327364475675590848030
          87086987551392711854517078544161852424320693150332
          59959406895756536782107074926966537676326235447210
          69793950679652694742597709739166693763042633987085
          41052684708299085211399427365734116182760315001271
          65378607361501080857009149939512557028198746004375
          35829035317434717326932123578154982629742552737307
          94953759765105305946966067683156574377167401875275
          88902802571733229619176668713819931811048770190271
          25267680276078003013678680992525463401061632866526
          36270218540497705585629946580636237993140746255962
          24074486908231174977792365466257246923322810917141
          91430288197103288597806669760892938638285025333403
          34413065578016127815921815005561868836468420090470
          23053081172816430487623791969842487255036638784583
          11487696932154902810424020138335124462181441773470
          63783299490636259666498587618221225225512486764533
          67720186971698544312419572409913959008952310058822
          95548255300263520781532296796249481641953868218774
          76085327132285723110424803456124867697064507995236
          37774242535411291684276865538926205024910326572967
          23701913275725675285653248258265463092207058596522
          29798860272258331913126375147341994889534765745501
          18495701454879288984856827726077713721403798879715
          38298203783031473527721580348144513491373226651381
          34829543829199918180278916522431027392251122869539
          40957953066405232632538044100059654939159879593635
          29746152185502371307642255121183693803580388584903
          41698116222072977186158236678424689157993532961922
          62467957194401269043877107275048102390895523597457
          23189706772547915061505504953922979530901129967519
          86188088225875314529584099251203829009407770775672
          11306739708304724483816533873502340845647058077308
          82959174767140363198008187129011875491310547126581
          97623331044818386269515456334926366572897563400500
          42846280183517070527831839425882145521227251250327
          55121603546981200581762165212827652751691296897789
          32238195734329339946437501907836945765883352399886
          75506164965184775180738168837861091527357929701337
          62177842752192623401942399639168044983993173312731
          32924185707147349566916674687634660915035914677504
          99518671430235219628894890102423325116913619626622
          73267460800591547471830798392868535206946944540724
          76841822524674417161514036427982273348055556214818
          97142617910342598647204516893989422179826088076852
          87783646182799346313767754307809363333018982642090
          10848802521674670883215120185883543223812876952786
          71329612474782464538636993009049310363619763878039
          62184073572399794223406235393808339651327408011116
          66627891981488087797941876876144230030984490851411
          60661826293682836764744779239180335110989069790714
          85786944089552990653640447425576083659976645795096
          66024396409905389607120198219976047599490197230297
          64913982680032973156037120041377903785566085089252
          16730939319872750275468906903707539413042652315011
          94809377245048795150954100921645863754710598436791
          78639167021187492431995700641917969777599028300699
          15368713711936614952811305876380278410754449733078
          40789923115535562561142322423255033685442488917353
          44889911501440648020369068063960672322193204149535
          41503128880339536053299340368006977710650566631954
          81234880673210146739058568557934581403627822703280
          82616570773948327592232845941706525094512325230608
          22918802058777319719839450180888072429661980811197
          77158542502016545090413245809786882778948721859617
          72107838435069186155435662884062257473692284509516
          20849603980134001723930671666823555245252804609722
          53503534226472524250874054075591789781264330331690)
    aesthetic-string
    (subseq <> 0 10)
    parse-integer
    (nth-value 0 <>)))

(defun problem-14 ()
  ;; The following iterative sequence is defined for the set of positive
  ;; integers:
  ;;
  ;;   n → n/2 (n is even)
  ;;   n → 3n + 1 (n is odd)
  ;;
  ;; Using the rule above and starting with 13, we generate the following
  ;; sequence:
  ;;
  ;;   13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
  ;;
  ;; It can be seen that this sequence (starting at 13 and finishing at 1)
  ;; contains 10 terms. Although it has not been proved yet (Collatz Problem),
  ;; it is thought that all starting numbers finish at 1.
  ;;
  ;; Which starting number, under one million, produces the longest chain?
  ;;
  ;; NOTE: Once the chain starts the terms are allowed to go above one million.
  (iterate (for i :from 1 :below 1000000)
           (finding i :maximizing #'collatz-length)))

(defun problem-15 ()
  ;; Starting in the top left corner of a 2×2 grid, and only being able to move
  ;; to the right and down, there are exactly 6 routes to the bottom right
  ;; corner.
  ;;
  ;; How many such routes are there through a 20×20 grid?
  (binomial-coefficient 40 20))

(defun problem-16 ()
  ;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  ;;
  ;; What is the sum of the digits of the number 2^1000?
  (sum (digits (expt 2 1000))))

(defun problem-17 ()
  ;; If the numbers 1 to 5 are written out in words: one, two, three, four,
  ;; five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
  ;;
  ;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out
  ;; in words, how many letters would be used?
  ;;
  ;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
  ;; forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
  ;; 20 letters. The use of "and" when writing out numbers is in compliance with
  ;; British usage, which is awful.
  (labels ((letters (n)
             (-<> n
               (format nil "~R" <>)
               (count-if #'alpha-char-p <>)))
           (has-british-and (n)
             (or (< n 100)
                 (zerop (mod n 100))))
           (silly-british-letters (n)
             (+ (letters n)
                (if (has-british-and n) 0 3))))
    (sum (irange 1 1000)
         :key #'silly-british-letters)))

(defun problem-18 ()
  ;; By starting at the top of the triangle below and moving to adjacent numbers
  ;; on the row below, the maximum total from top to bottom is 23.
  ;;
  ;;        3
  ;;       7 4
  ;;      2 4 6
  ;;     8 5 9 3
  ;;
  ;; That is, 3 + 7 + 4 + 9 = 23.
  ;;
  ;; Find the maximum total from top to bottom of the triangle below.
  ;;
  ;; NOTE: As there are only 16384 routes, it is possible to solve this problem
  ;; by trying every route. However, Problem 67, is the same challenge with
  ;; a triangle containing one-hundred rows; it cannot be solved by brute force,
  ;; and requires a clever method! ;o)
  (let ((triangle '((75)
                    (95 64)
                    (17 47 82)
                    (18 35 87 10)
                    (20 04 82 47 65)
                    (19 01 23 75 03 34)
                    (88 02 77 73 07 63 67)
                    (99 65 04 28 06 16 70 92)
                    (41 41 26 56 83 40 80 70 33)
                    (41 48 72 33 47 32 37 16 94 29)
                    (53 71 44 65 25 43 91 52 97 51 14)
                    (70 11 33 28 77 73 17 78 39 68 17 57)
                    (91 71 52 38 17 14 91 43 58 50 27 29 48)
                    (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
                    (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23))))
    (car (reduce (lambda (prev last)
                   (mapcar #'+
                           prev
                           (mapcar #'max last (rest last))))
                 triangle
                 :from-end t))))

(defun problem-19 ()
  ;; You are given the following information, but you may prefer to do some
  ;; research for yourself.
  ;;
  ;; 1 Jan 1900 was a Monday.
  ;; Thirty days has September,
  ;; April, June and November.
  ;; All the rest have thirty-one,
  ;; Saving February alone,
  ;; Which has twenty-eight, rain or shine.
  ;; And on leap years, twenty-nine.
  ;; A leap year occurs on any year evenly divisible by 4, but not on a century
  ;; unless it is divisible by 400.
  ;;
  ;; How many Sundays fell on the first of the month during the twentieth
  ;; century (1 Jan 1901 to 31 Dec 2000)?
  (iterate
    (for-nested ((year :from 1901 :to 2000)
                 (month :from 1 :to 12)))
    (counting (-<> (local-time:encode-timestamp 0 0 0 0 1 month year)
                local-time:timestamp-day-of-week
                zerop))))

(defun problem-20 ()
  ;; n! means n × (n − 1) × ... × 3 × 2 × 1
  ;;
  ;; For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
  ;; and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
  ;;
  ;; Find the sum of the digits in the number 100!
  (sum (digits (factorial 100))))

(defun problem-21 ()
  ;; Let d(n) be defined as the sum of proper divisors of n (numbers less than
  ;; n which divide evenly into n).
  ;;
  ;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair
  ;; and each of a and b are called amicable numbers.
  ;;
  ;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
  ;; 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
  ;; 71 and 142; so d(284) = 220.
  ;;
  ;; Evaluate the sum of all the amicable numbers under 10000.
  (labels ((sum-of-divisors (n)
             (sum (proper-divisors n)))
           (amicablep (n)
             (let ((other (sum-of-divisors n)))
               (and (not= n other)
                    (= n (sum-of-divisors other))))))
    (sum (remove-if-not #'amicablep (range 1 10000)))))

(defun problem-22 ()
  ;; Using names.txt, a 46K text file containing over five-thousand first names,
  ;; begin by sorting it into alphabetical order. Then working out the
  ;; alphabetical value for each name, multiply this value by its alphabetical
  ;; position in the list to obtain a name score.
  ;;
  ;; For example, when the list is sorted into alphabetical order, COLIN, which
  ;; is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So,
  ;; COLIN would obtain a score of 938 × 53 = 49714.
  ;;
  ;; What is the total of all the name scores in the file?
  (labels ((read-names ()
             (-<> "data/22-names.txt"
               parse-strings-file
               (sort <> #'string<)))
           (name-score (name)
             (sum name :key #'letter-number)))
    (iterate (for (position . name) :in
                  (enumerate (read-names) :start 1))
             (sum (* position (name-score name))))))

(defun problem-23 ()
  ;; A perfect number is a number for which the sum of its proper divisors is
  ;; exactly equal to the number. For example, the sum of the proper divisors of
  ;; 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
  ;; number.
  ;;
  ;; A number n is called deficient if the sum of its proper divisors is less
  ;; than n and it is called abundant if this sum exceeds n.
  ;;
  ;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
  ;; number that can be written as the sum of two abundant numbers is 24. By
  ;; mathematical analysis, it can be shown that all integers greater than 28123
  ;; can be written as the sum of two abundant numbers. However, this upper
  ;; limit cannot be reduced any further by analysis even though it is known
  ;; that the greatest number that cannot be expressed as the sum of two
  ;; abundant numbers is less than this limit.
  ;;
  ;; Find the sum of all the positive integers which cannot be written as the
  ;; sum of two abundant numbers.
  (let* ((limit 28123)
         (abundant-numbers
           (make-hash-set :initial-contents
                          (remove-if-not #'abundantp (irange 1 limit)))))
    (flet ((abundant-sum-p (n)
             (iterate (for a :in-hashset abundant-numbers)
                      (when (hset-contains-p abundant-numbers (- n a))
                        (return t)))))
      (sum (remove-if #'abundant-sum-p (irange 1 limit))))))

(defun problem-24 ()
  ;; A permutation is an ordered arrangement of objects. For example, 3124 is
  ;; one possible permutation of the digits 1, 2, 3 and 4. If all of the
  ;; permutations are listed numerically or alphabetically, we call it
  ;; lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
  ;;
  ;; 012   021   102   120   201   210
  ;;
  ;; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3,
  ;; 4, 5, 6, 7, 8 and 9?
  (-<> "0123456789"
    (gathering-vector (:size (factorial (length <>)))
      (map-permutations (compose #'gather #'parse-integer) <>
                        :copy nil))
    (sort <> #'<)
    (elt <> (1- 1000000))))

(defun problem-25 ()
  ;; The Fibonacci sequence is defined by the recurrence relation:
  ;;
  ;;   Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
  ;;
  ;; Hence the first 12 terms will be:
  ;;
  ;; F1 = 1
  ;; F2 = 1
  ;; F3 = 2
  ;; F4 = 3
  ;; F5 = 5
  ;; F6 = 8
  ;; F7 = 13
  ;; F8 = 21
  ;; F9 = 34
  ;; F10 = 55
  ;; F11 = 89
  ;; F12 = 144
  ;;
  ;; The 12th term, F12, is the first term to contain three digits.
  ;;
  ;; What is the index of the first term in the Fibonacci sequence to contain
  ;; 1000 digits?
  (iterate (for f :in-fibonacci t)
           (for i :from 1)
           (finding i :such-that (= 1000 (digits-length f)))))

(defun problem-26 ()
  ;; A unit fraction contains 1 in the numerator. The decimal representation of
  ;; the unit fractions with denominators 2 to 10 are given:
  ;;
  ;; 1/2	= 	0.5
  ;; 1/3	= 	0.(3)
  ;; 1/4	= 	0.25
  ;; 1/5	= 	0.2
  ;; 1/6	= 	0.1(6)
  ;; 1/7	= 	0.(142857)
  ;; 1/8	= 	0.125
  ;; 1/9	= 	0.(1)
  ;; 1/10	= 	0.1
  ;;
  ;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can
  ;; be seen that 1/7 has a 6-digit recurring cycle.
  ;;
  ;; Find the value of d < 1000 for which 1/d contains the longest recurring
  ;; cycle in its decimal fraction part.
  (iterate
    ;; 2 and 5 are the only primes that aren't coprime to 10
    (for i :in (set-difference (primes-below 1000) '(2 5)))
    (finding i :maximizing (multiplicative-order 10 i))))

(defun problem-27 ()
  ;; Euler discovered the remarkable quadratic formula:
  ;;
  ;;     n² + n + 41
  ;;
  ;; It turns out that the formula will produce 40 primes for the consecutive
  ;; integer values 0 ≤ n ≤ 39. However, when n=40, 40² + 40 + 41 = 40(40 + 1)
  ;; + 41 is divisible by 41, and certainly when n=41, 41² + 41 + 41 is clearly
  ;; divisible by 41.
  ;;
  ;; The incredible formula n² − 79n + 1601 was discovered, which produces 80
  ;; primes for the consecutive values 0 ≤ n ≤ 79. The product of the
  ;; coefficients, −79 and 1601, is −126479.
  ;;
  ;; Considering quadratics of the form:
  ;;
  ;;     n² + an + b, where |a| < 1000 and |b| ≤ 1000
  ;;
  ;;     where |n| is the modulus/absolute value of n
  ;;     e.g. |11| = 11 and |−4| = 4
  ;;
  ;; Find the product of the coefficients, a and b, for the quadratic expression
  ;; that produces the maximum number of primes for consecutive values of n,
  ;; starting with n=0.
  (flet ((primes-produced (a b)
           (iterate (for n :from 0)
                    (while (primep (+ (square n) (* a n) b)))
                    (counting t))))
    (iterate (for-nested ((a :from -999 :to 999)
                          (b :from -1000 :to 1000)))
             (finding (* a b) :maximizing (primes-produced a b)))))

(defun problem-28 ()
  ;; Starting with the number 1 and moving to the right in a clockwise direction
  ;; a 5 by 5 spiral is formed as follows:
  ;;
  ;; 21 22 23 24 25
  ;; 20  7  8  9 10
  ;; 19  6  1  2 11
  ;; 18  5  4  3 12
  ;; 17 16 15 14 13
  ;;
  ;; It can be verified that the sum of the numbers on the diagonals is 101.
  ;;
  ;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
  ;; formed in the same way?
  (iterate (for size :from 1 :to 1001 :by 2)
           (summing (apply #'+ (number-spiral-corners size)))))

(defun problem-29 ()
  ;; Consider all integer combinations of a^b for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
  ;;
  ;; 2²=4,  2³=8,   2⁴=16,  2⁵=32
  ;; 3²=9,  3³=27,  3⁴=81,  3⁵=243
  ;; 4²=16, 4³=64,  4⁴=256, 4⁵=1024
  ;; 5²=25, 5³=125, 5⁴=625, 5⁵=3125
  ;;
  ;; If they are then placed in numerical order, with any repeats removed, we
  ;; get the following sequence of 15 distinct terms:
  ;;
  ;; 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
  ;;
  ;; How many distinct terms are in the sequence generated by a^b for
  ;; 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
  (length (iterate (for-nested ((a :from 2 :to 100)
                                (b :from 2 :to 100)))
                   (adjoining (expt a b)))))

(defun problem-30 ()
  ;; Surprisingly there are only three numbers that can be written as the sum of
  ;; fourth powers of their digits:
  ;;
  ;; 1634 = 1⁴ + 6⁴ + 3⁴ + 4⁴
  ;; 8208 = 8⁴ + 2⁴ + 0⁴ + 8⁴
  ;; 9474 = 9⁴ + 4⁴ + 7⁴ + 4⁴
  ;;
  ;; As 1 = 1⁴ is not a sum it is not included.
  ;;
  ;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.
  ;;
  ;; Find the sum of all the numbers that can be written as the sum of fifth
  ;; powers of their digits.
  (flet ((maximum-sum-for-digits (n)
           (* (expt 9 5) n))
         (digit-power-sum (n)
           (sum (mapcar (rcurry #'expt 5) (digits n)))))
    (iterate
      ;; We want to find a limit N that's bigger than the maximum possible sum
      ;; for its number of digits.
      (with limit = (iterate (for digits :from 1)
                             (for n = (expt 10 digits))
                             (while (< n (maximum-sum-for-digits digits)))
                             (finally (return n))))
      ;; Then just brute-force the thing.
      (for i :from 2 :to limit)
      (when (= i (digit-power-sum i))
        (summing i)))))

(defun problem-31 ()
  ;; In England the currency is made up of pound, £, and pence, p, and there are
  ;; eight coins in general circulation:
  ;;
  ;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
  ;;
  ;; It is possible to make £2 in the following way:
  ;;
  ;; 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
  ;;
  ;; How many different ways can £2 be made using any number of coins?
  (recursively ((amount 200)
                (coins '(200 100 50 20 10 5 2 1)))
    (cond
      ((zerop amount) 1)
      ((minusp amount) 0)
      ((null coins) 0)
      (t (+ (recur (- amount (first coins)) coins)
            (recur amount (rest coins)))))))

(defun problem-32 ()
  ;; We shall say that an n-digit number is pandigital if it makes use of all
  ;; the digits 1 to n exactly once; for example, the 5-digit number, 15234, is
  ;; 1 through 5 pandigital.
  ;;
  ;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
  ;; multiplicand, multiplier, and product is 1 through 9 pandigital.
  ;;
  ;; Find the sum of all products whose multiplicand/multiplier/product identity
  ;; can be written as a 1 through 9 pandigital.
  ;;
  ;; HINT: Some products can be obtained in more than one way so be sure to only
  ;; include it once in your sum.
  (labels ((split (digits a b)
             (values (digits-to-number (subseq digits 0 a))
                     (digits-to-number (subseq digits a (+ a b)))
                     (digits-to-number (subseq digits (+ a b)))))
           (check (digits a b)
             (multiple-value-bind (a b c)
                 (split digits a b)
               (when (= (* a b) c)
                 c))))
    (-<> (gathering
           (map-permutations (lambda (digits)
                               (let ((c1 (check digits 3 2))
                                     (c2 (check digits 4 1)))
                                 (when c1 (gather c1))
                                 (when c2 (gather c2))))
                             #(1 2 3 4 5 6 7 8 9)
                             :copy nil))
      remove-duplicates
      sum)))

(defun problem-33 ()
  ;; The fraction 49/98 is a curious fraction, as an inexperienced mathematician
  ;; in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
  ;; which is correct, is obtained by cancelling the 9s.
  ;;
  ;; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
  ;;
  ;; There are exactly four non-trivial examples of this type of fraction, less
  ;; than one in value, and containing two digits in the numerator and
  ;; denominator.
  ;;
  ;; If the product of these four fractions is given in its lowest common terms,
  ;; find the value of the denominator.
  (labels ((safe/ (a b)
             (unless (zerop b) (/ a b)))
           (cancel (digit other digits)
             (destructuring-bind (x y) digits
               (remove nil (list (when (= digit x) (safe/ other y))
                                 (when (= digit y) (safe/ other x))))))
           (cancellations (numerator denominator)
             (let ((nd (digits numerator))
                   (dd (digits denominator)))
               (append (cancel (first nd) (second nd) dd)
                       (cancel (second nd) (first nd) dd))))
           (curiousp (numerator denominator)
             (member (/ numerator denominator)
                     (cancellations numerator denominator)))
           (trivialp (numerator denominator)
             (and (dividesp numerator 10)
                  (dividesp denominator 10))))
    (iterate
      (with result = 1)
      (for numerator :from 10 :to 99)
      (iterate (for denominator :from (1+ numerator) :to 99)
               (when (and (curiousp numerator denominator)
                          (not (trivialp numerator denominator)))
                 (mulf result (/ numerator denominator))))
      (finally (return (denominator result))))))

(defun problem-34 ()
  ;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  ;;
  ;; Find the sum of all numbers which are equal to the sum of the factorial of
  ;; their digits.
  ;;
  ;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  (iterate
    (for n :from 3 :to 1000000)
    ;; have to use funcall here because `sum` is an iterate keyword.  kill me.
    (when (= n (funcall #'sum (digits n) :key #'factorial))
      (summing n))))

(defun problem-35 ()
  ;; The number, 197, is called a circular prime because all rotations of the
  ;; digits: 197, 971, and 719, are themselves prime.
  ;;
  ;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
  ;; 71, 73, 79, and 97.
  ;;
  ;; How many circular primes are there below one million?
  (labels ((rotate (n distance)
             (multiple-value-bind (hi lo)
                 (truncate n (expt 10 distance))
               (+ (* (expt 10 (digits-length hi)) lo)
                  hi)))
           (rotations (n)
             (mapcar (curry #'rotate n) (range 1 (digits-length n))))
           (circular-prime-p (n)
             (every #'primep (rotations n))))
    (iterate (for i :in-vector (sieve 1000000))
             (counting (circular-prime-p i)))))

(defun problem-36 ()
  ;; The decimal number, 585 = 1001001001 (binary), is palindromic in both
  ;; bases.
  ;;
  ;; Find the sum of all numbers, less than one million, which are palindromic
  ;; in base 10 and base 2.
  ;;
  ;; (Please note that the palindromic number, in either base, may not include
  ;; leading zeros.)
  (iterate (for i :from 1 :below 1000000)
           (when (and (palindromep i 10)
                      (palindromep i 2))
             (sum i))))

(defun problem-37 ()
  ;; The number 3797 has an interesting property. Being prime itself, it is
  ;; possible to continuously remove digits from left to right, and remain prime
  ;; at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
  ;; left: 3797, 379, 37, and 3.
  ;;
  ;; Find the sum of the only eleven primes that are both truncatable from left
  ;; to right and right to left.
  ;;
  ;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  (labels ((truncations (n)
             (iterate (for i :from 0 :below (digits-length n))
                      (collect (truncate-number-left n i))
                      (collect (truncate-number-right n i))))
           (truncatablep (n)
             (every #'primep (truncations n))))
    (iterate
      (with count = 0)
      (for i :from 11 :by 2)
      (when (truncatablep i)
        (sum i)
        (incf count))
      (while (< count 11)))))

(defun problem-38 ()
  ;; Take the number 192 and multiply it by each of 1, 2, and 3:
  ;;
  ;;   192 × 1 = 192
  ;;   192 × 2 = 384
  ;;   192 × 3 = 576
  ;;
  ;; By concatenating each product we get the 1 to 9 pandigital, 192384576. We
  ;; will call 192384576 the concatenated product of 192 and (1,2,3)
  ;;
  ;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
  ;; and 5, giving the pandigital, 918273645, which is the concatenated product
  ;; of 9 and (1,2,3,4,5).
  ;;
  ;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as
  ;; the concatenated product of an integer with (1,2, ... , n) where n > 1?
  (labels ((concatenated-product (number i)
             (apply #'concatenate-integers
                    (iterate (for n :from 1 :to i)
                             (collect (* number n))))))
    (iterate
      main
      (for base :from 1)
      ;; base can't be more than 5 digits long because we have to concatenate at
      ;; least two products of it
      (while (digits<= base 5))
      (iterate (for n :from 2)
               (for result = (concatenated-product base n))
               ;; result is only ever going to grow larger, so once we pass the
               ;; nine digit mark we can stop
               (while (digits<= result 9))
               (when (pandigitalp result)
                 (in main (maximizing result)))))))

(defun problem-39 ()
  ;; If p is the perimeter of a right angle triangle with integral length sides,
  ;; {a,b,c}, there are exactly three solutions for p = 120.
  ;;
  ;; {20,48,52}, {24,45,51}, {30,40,50}
  ;;
  ;; For which value of p ≤ 1000, is the number of solutions maximised?
  (iterate
    (for p :from 1 :to 1000)
    (finding p :maximizing (length (pythagorean-triplets-of-perimeter p)))))

(defun problem-40 ()
  ;; An irrational decimal fraction is created by concatenating the positive
  ;; integers:
  ;;
  ;; 0.123456789101112131415161718192021...
  ;;
  ;; It can be seen that the 12th digit of the fractional part is 1.
  ;;
  ;; If dn represents the nth digit of the fractional part, find the value of
  ;; the following expression.
  ;;
  ;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  (iterate
    top
    (with index = 0)
    (for digits :key #'digits :from 1)
    (iterate (for d :in digits)
             (incf index)
             (when (member index '(1 10 100 1000 10000 100000 1000000))
               (in top (multiplying d))
               (when (= index 1000000)
                 (in top (terminate)))))))

(defun problem-41 ()
  ;; We shall say that an n-digit number is pandigital if it makes use of all
  ;; the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
  ;; and is also prime.
  ;;
  ;; What is the largest n-digit pandigital prime that exists?
  (iterate
    ;; There's a clever observation which reduces the upper bound from 9 to
    ;; 7 from "gamma" in the forum:
    ;;
    ;; > Note: Nine numbers cannot be done (1+2+3+4+5+6+7+8+9=45 => always dividable by 3)
    ;; > Note: Eight numbers cannot be done (1+2+3+4+5+6+7+8=36 => always dividable by 3)
    (for n :downfrom 7)
    (thereis (apply (nullary #'max)
                    (remove-if-not #'primep (pandigitals 1 n))))))

(defun problem-42 ()
  ;; The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
  ;; so the first ten triangle numbers are:
  ;;
  ;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
  ;;
  ;; By converting each letter in a word to a number corresponding to its
  ;; alphabetical position and adding these values we form a word value. For
  ;; example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word
  ;; value is a triangle number then we shall call the word a triangle word.
  ;;
  ;; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
  ;; containing nearly two-thousand common English words, how many are triangle
  ;; words?
  (labels ((word-value (word)
             (sum word :key #'letter-number))
           (triangle-word-p (word)
             (trianglep (word-value word))))
    (count-if #'triangle-word-p (parse-strings-file "data/42-words.txt"))))

(defun problem-43 ()
  ;; The number, 1406357289, is a 0 to 9 pandigital number because it is made up
  ;; of each of the digits 0 to 9 in some order, but it also has a rather
  ;; interesting sub-string divisibility property.
  ;;
  ;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we
  ;; note the following:
  ;;
  ;; d2d3d4=406 is divisible by 2
  ;; d3d4d5=063 is divisible by 3
  ;; d4d5d6=635 is divisible by 5
  ;; d5d6d7=357 is divisible by 7
  ;; d6d7d8=572 is divisible by 11
  ;; d7d8d9=728 is divisible by 13
  ;; d8d9d10=289 is divisible by 17
  ;;
  ;; Find the sum of all 0 to 9 pandigital numbers with this property.
  (labels ((extract3 (digits start)
             (digits-to-number (subseq digits start (+ 3 start))))
           (interestingp (n)
             (let ((digits (digits n)))
               ;; eat shit mathematicians, indexes start from zero
               (and (dividesp (extract3 digits 1) 2)
                    (dividesp (extract3 digits 2) 3)
                    (dividesp (extract3 digits 3) 5)
                    (dividesp (extract3 digits 4) 7)
                    (dividesp (extract3 digits 5) 11)
                    (dividesp (extract3 digits 6) 13)
                    (dividesp (extract3 digits 7) 17)))))
    (sum (remove-if-not #'interestingp (pandigitals 0 9)))))

(defun problem-44 ()
  ;; Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first
  ;; ten pentagonal numbers are:
  ;;
  ;; 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
  ;;
  ;; It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference,
  ;; 70 − 22 = 48, is not pentagonal.
  ;;
  ;; Find the pair of pentagonal numbers, Pj and Pk, for which their sum and
  ;; difference are pentagonal and D = |Pk − Pj| is minimised; what is the value
  ;; of D?
  (flet ((interestingp (px py)
           (and (pentagonp (+ py px))
                (pentagonp (- py px)))))
    (iterate
      (with result = most-positive-fixnum) ; my kingdom for `CL:INFINITY`
      (for y :from 2)
      (for z :from 3)
      (for py = (pentagon y))
      (for pz = (pentagon z))
      (when (>= (- pz py) result)
        (return result))
      (iterate
        (for x :from (1- y) :downto 1)
        (for px = (pentagon x))
        (when (interestingp px py)
          (let ((distance (- py px)))
            (when (< distance result)
              ;; TODO: This isn't quite right, because this is just the FIRST
              ;; number we find -- we haven't guaranteed that it's the SMALLEST
              ;; one we'll ever see.  But it happens to accidentally be the
              ;; correct one, and until I get around to rewriting this with
              ;; priority queues it'll have to do.
              (return-from problem-44 distance)
              (setf result distance)))
          (return))))))

(defun problem-45 ()
  ;; Triangle, pentagonal, and hexagonal numbers are generated by the following
  ;; formulae:
  ;;
  ;; Triangle	 	Tn=n(n+1)/2	 	1, 3, 6, 10, 15, ...
  ;; Pentagonal	 	Pn=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
  ;; Hexagonal	 	Hn=n(2n−1)	 	1, 6, 15, 28, 45, ...
  ;;
  ;; It can be verified that T285 = P165 = H143 = 40755.
  ;;
  ;; Find the next triangle number that is also pentagonal and hexagonal.
  (iterate
    (for n :key #'triangle :from 286)
    (finding n :such-that (and (pentagonp n) (hexagonp n)))))

(defun problem-46 ()
  ;; It was proposed by Christian Goldbach that every odd composite number can
  ;; be written as the sum of a prime and twice a square.
  ;;
  ;; 9 = 7 + 2×1²
  ;; 15 = 7 + 2×2²
  ;; 21 = 3 + 2×3²
  ;; 25 = 7 + 2×3²
  ;; 27 = 19 + 2×2²
  ;; 33 = 31 + 2×1²
  ;;
  ;; It turns out that the conjecture was false.
  ;;
  ;; What is the smallest odd composite that cannot be written as the sum of
  ;; a prime and twice a square?
  (flet ((counterexamplep (n)
           (iterate
             (for prime :in-vector (sieve n))
             (never (squarep (/ (- n prime) 2))))))
    (iterate
      (for i :from 1 :by 2)
      (finding i :such-that (and (compositep i)
                                 (counterexamplep i))))))

(defun problem-47 ()
  ;; The first two consecutive numbers to have two distinct prime factors are:
  ;;
  ;; 14 = 2 × 7
  ;; 15 = 3 × 5
  ;;
  ;; The first three consecutive numbers to have three distinct prime factors are:
  ;;
  ;; 644 = 2² × 7 × 23
  ;; 645 = 3 × 5 × 43
  ;; 646 = 2 × 17 × 19
  ;;
  ;; Find the first four consecutive integers to have four distinct prime
  ;; factors each. What is the first of these numbers?
  (flet ((factor-count (n)
           (length (remove-duplicates (prime-factorization n)))))
    (iterate
      (with run = 0)
      (for i :from 1)
      (if (= 4 (factor-count i))
        (incf run)
        (setf run 0))
      (finding (- i 3) :such-that (= run 4)))))

(defun problem-48 ()
  ;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
  ;;
  ;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
  (-<> (irange 1 1000)
    (mapcar #'expt <> <>)
    sum
    (mod <> (expt 10 10))))

(defun problem-49 ()
  ;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
  ;; increases by 3330, is unusual in two ways: (i) each of the three terms are
  ;; prime, and, (ii) each of the 4-digit numbers are permutations of one
  ;; another.
  ;;
  ;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
  ;; primes, exhibiting this property, but there is one other 4-digit increasing
  ;; sequence.
  ;;
  ;; What 12-digit number do you form by concatenating the three terms in this
  ;; sequence?
  (labels ((permutation= (a b)
             (orderless-equal (digits a) (digits b)))
           (length>=3 (list)
             (>= (length list) 3))
           (arithmetic-sequence-p (seq)
             (apply #'= (mapcar (curry #'apply #'-)
                                (n-grams 2 seq))))
           (has-arithmetic-sequence-p (seq)
             (map-combinations
               (lambda (s)
                 (when (arithmetic-sequence-p s)
                   (return-from has-arithmetic-sequence-p s)))
               (sort seq #'<)
               :length 3)
             nil))
    (-<> (primes-in 1000 9999)
      (equivalence-classes #'permutation= <>) ; find all permutation groups
      (remove-if-not #'length>=3 <>) ; make sure they have at leat 3 elements
      (mapcar #'has-arithmetic-sequence-p <>)
      (remove nil <>)
      (remove-if (lambda (s) (= (first s) 1487)) <>) ; remove the example
      first
      (mapcan #'digits <>)
      digits-to-number)))

(defun problem-50 ()
  ;; The prime 41, can be written as the sum of six consecutive primes:
  ;;
  ;; 41 = 2 + 3 + 5 + 7 + 11 + 13
  ;;
  ;; This is the longest sum of consecutive primes that adds to a prime below
  ;; one-hundred.
  ;;
  ;; The longest sum of consecutive primes below one-thousand that adds to
  ;; a prime, contains 21 terms, and is equal to 953.
  ;;
  ;; Which prime, below one-million, can be written as the sum of the most
  ;; consecutive primes?
  (let ((primes (sieve 1000000)))
    (flet ((score (start)
             (iterate
               (with score = 0)
               (with winner = 0)
               (for run :from 1)
               (for prime :in-vector primes :from start)
               (summing prime :into sum)
               (while (< sum 1000000))
               (when (primep sum)
                 (setf score run
                       winner sum))
               (finally (return (values score winner))))))
      (iterate
        (for (values score winner)
             :key #'score :from 0 :below (length primes))
        (finding winner :maximizing score)))))

(defun problem-51 ()
  ;; By replacing the 1st digit of the 2-digit number *3, it turns out that six
  ;; of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
  ;;
  ;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this
  ;; 5-digit number is the first example having seven primes among the ten
  ;; generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
  ;; 56773, and 56993. Consequently 56003, being the first member of this
  ;; family, is the smallest prime with this property.
  ;;
  ;; Find the smallest prime which, by replacing part of the number (not
  ;; necessarily adjacent digits) with the same digit, is part of an eight prime
  ;; value family.
  (labels
      ((patterns (prime)
         (iterate (with size = (digits-length prime))
                  (with indices = (range 0 size))
                  (for i :from 1 :below size)
                  (appending (combinations indices :length i))))
       (apply-pattern-digit (prime pattern new-digit)
         (iterate (with result = (digits prime))
                  (for index :in pattern)
                  (when (and (zerop index) (zerop new-digit))
                    (leave))
                  (setf (nth index result) new-digit)
                  (finally (return (digits-to-number result)))))
       (apply-pattern (prime pattern)
         (iterate (for digit in (irange 0 9))
                  (for result = (apply-pattern-digit prime pattern digit))
                  (when (and result (primep result))
                    (collect result))))
       (apply-patterns (prime)
         (mapcar (curry #'apply-pattern prime) (patterns prime)))
       (winnerp (prime)
         (find-if (curry #'length= 8) (apply-patterns prime))))
    (-<> (iterate (for i :from 3 :by 2)
                  (thereis (and (primep i) (winnerp i))))
      (sort< <>)
      first)))

(defun problem-52 ()
  ;; It can be seen that the number, 125874, and its double, 251748, contain
  ;; exactly the same digits, but in a different order.
  ;;
  ;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
  ;; contain the same digits.
  (iterate (for i :from 1)
           (for digits = (digits i))
           (finding i :such-that
                    (every (lambda (n)
                             (orderless-equal digits (digits (* n i))))
                           '(2 3 4 5 6)))))

(defun problem-53 ()
  ;; There are exactly ten ways of selecting three from five, 12345:
  ;;
  ;; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
  ;;
  ;; In combinatorics, we use the notation, 5C3 = 10.
  ;;
  ;; In general,
  ;;
  ;;   nCr = n! / r!(n−r)!
  ;;
  ;; where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
  ;;
  ;; It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
  ;;
  ;; How many, not necessarily distinct, values of nCr, for 1 ≤ n ≤ 100, are
  ;; greater than one-million?
  (iterate
    main
    (for n :from 1 :to 100)
    (iterate
      (for r :from 1 :to n)
      (for nCr = (binomial-coefficient n r))
      (in main (counting (> nCr 1000000))))))

(defun problem-54 ()
  ;; In the card game poker, a hand consists of five cards and are ranked, from
  ;; lowest to highest, in the following way:
  ;;
  ;; High Card: Highest value card.
  ;; One Pair: Two cards of the same value.
  ;; Two Pairs: Two different pairs.
  ;; Three of a Kind: Three cards of the same value.
  ;; Straight: All cards are consecutive values.
  ;; Flush: All cards of the same suit.
  ;; Full House: Three of a kind and a pair.
  ;; Four of a Kind: Four cards of the same value.
  ;; Straight Flush: All cards are consecutive values of same suit.
  ;; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
  ;;
  ;; The cards are valued in the order:
  ;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
  ;;
  ;; If two players have the same ranked hands then the rank made up of the
  ;; highest value wins; for example, a pair of eights beats a pair of fives
  ;; (see example 1 below). But if two ranks tie, for example, both players have
  ;; a pair of queens, then highest cards in each hand are compared (see example
  ;; 4 below); if the highest cards tie then the next highest cards are
  ;; compared, and so on.
  ;;
  ;; The file, poker.txt, contains one-thousand random hands dealt to two
  ;; players. Each line of the file contains ten cards (separated by a single
  ;; space): the first five are Player 1's cards and the last five are Player
  ;; 2's cards. You can assume that all hands are valid (no invalid characters
  ;; or repeated cards), each player's hand is in no specific order, and in each
  ;; hand there is a clear winner.
  ;;
  ;; How many hands does Player 1 win?
  (iterate (for line :in-file "data/54-poker.txt" :using #'read-line)
           (for cards = (mapcar #'euler.poker::parse-card
                                (cl-strings:split line #\space)))
           (for p1 = (take 5 cards))
           (for p2 = (drop 5 cards))
           (counting (euler.poker::poker-hand-beats-p p1 p2))))

(defun problem-55 ()
  ;; If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
  ;;
  ;; Not all numbers produce palindromes so quickly. For example,
  ;;
  ;; 349 + 943 = 1292,
  ;; 1292 + 2921 = 4213
  ;; 4213 + 3124 = 7337
  ;;
  ;; That is, 349 took three iterations to arrive at a palindrome.
  ;;
  ;; Although no one has proved it yet, it is thought that some numbers, like
  ;; 196, never produce a palindrome. A number that never forms a palindrome
  ;; through the reverse and add process is called a Lychrel number. Due to the
  ;; theoretical nature of these numbers, and for the purpose of this problem,
  ;; we shall assume that a number is Lychrel until proven otherwise. In
  ;; addition you are given that for every number below ten-thousand, it will
  ;; either (i) become a palindrome in less than fifty iterations, or, (ii) no
  ;; one, with all the computing power that exists, has managed so far to map it
  ;; to a palindrome. In fact, 10677 is the first number to be shown to require
  ;; over fifty iterations before producing a palindrome:
  ;; 4668731596684224866951378664 (53 iterations, 28-digits).
  ;;
  ;; Surprisingly, there are palindromic numbers that are themselves Lychrel
  ;; numbers; the first example is 4994.
  ;;
  ;; How many Lychrel numbers are there below ten-thousand?
  (labels ((lychrel (n)
             (+ n (reverse-integer n)))
           (lychrelp (n)
             (iterate
               (repeat 50)
               (for i :iterating #'lychrel :initially n)
               (never (palindromep i)))))
    (iterate (for i :from 0 :below 10000)
             (counting (lychrelp i)))))

(defun problem-56 ()
  ;; A googol (10^100) is a massive number: one followed by one-hundred zeros;
  ;; 100^100 is almost unimaginably large: one followed by two-hundred zeros.
  ;; Despite their size, the sum of the digits in each number is only 1.
  ;;
  ;; Considering natural numbers of the form, a^b, where a, b < 100, what is the
  ;; maximum digital sum?
  (iterate (for-nested ((a :from 1 :below 100)
                        (b :from 1 :below 100)))
           (maximizing (funcall #'sum (digits (expt a b))))))

(defun problem-57 ()
  ;; It is possible to show that the square root of two can be expressed as an
  ;; infinite continued fraction.
  ;;
  ;; √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
  ;;
  ;; By expanding this for the first four iterations, we get:
  ;;
  ;; 1 + 1/2 = 3/2 = 1.5
  ;; 1 + 1/(2 + 1/2) = 7/5 = 1.4
  ;; 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
  ;; 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
  ;;
  ;; The next three expansions are 99/70, 239/169, and 577/408, but the eighth
  ;; expansion, 1393/985, is the first example where the number of digits in the
  ;; numerator exceeds the number of digits in the denominator.
  ;;
  ;; In the first one-thousand expansions, how many fractions contain
  ;; a numerator with more digits than denominator?
  (iterate
    (repeat 1000)
    (for i :initially 1/2 :then (/ (+ 2 i)))
    (for expansion = (1+ i))
    (counting (> (digits-length (numerator expansion))
                 (digits-length (denominator expansion))))))

(defun problem-58 ()
  ;; Starting with 1 and spiralling anticlockwise in the following way, a square
  ;; spiral with side length 7 is formed.
  ;;
  ;; 37 36 35 34 33 32 31
  ;; 38 17 16 15 14 13 30
  ;; 39 18  5  4  3 12 29
  ;; 40 19  6  1  2 11 28
  ;; 41 20  7  8  9 10 27
  ;; 42 21 22 23 24 25 26
  ;; 43 44 45 46 47 48 49
  ;;
  ;; It is interesting to note that the odd squares lie along the bottom right
  ;; diagonal, but what is more interesting is that 8 out of the 13 numbers
  ;; lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
  ;;
  ;; If one complete new layer is wrapped around the spiral above, a square
  ;; spiral with side length 9 will be formed. If this process is continued,
  ;; what is the side length of the square spiral for which the ratio of primes
  ;; along both diagonals first falls below 10%?
  (labels ((score (value)
             (if (primep value) 1 0))
           (primes-in-layer (size)
             (sum (number-spiral-corners size) :key #'score)))
    (iterate
      (for size :from 3 :by 2)
      (for count :from 5 :by 4)
      (sum (primes-in-layer size) :into primes)
      (for ratio = (/ primes count))
      (finding size :such-that (< ratio 1/10)))))

(defun problem-59 ()
  ;; Each character on a computer is assigned a unique code and the preferred
  ;; standard is ASCII (American Standard Code for Information Interchange).
  ;; For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
  ;;
  ;; A modern encryption method is to take a text file, convert the bytes to
  ;; ASCII, then XOR each byte with a given value, taken from a secret key. The
  ;; advantage with the XOR function is that using the same encryption key on
  ;; the cipher text, restores the plain text; for example, 65 XOR 42 = 107,
  ;; then 107 XOR 42 = 65.
  ;;
  ;; For unbreakable encryption, the key is the same length as the plain text
  ;; message, and the key is made up of random bytes. The user would keep the
  ;; encrypted message and the encryption key in different locations, and
  ;; without both "halves", it is impossible to decrypt the message.
  ;;
  ;; Unfortunately, this method is impractical for most users, so the modified
  ;; method is to use a password as a key. If the password is shorter than the
  ;; message, which is likely, the key is repeated cyclically throughout the
  ;; message. The balance for this method is using a sufficiently long password
  ;; key for security, but short enough to be memorable.
  ;;
  ;; Your task has been made easy, as the encryption key consists of three lower
  ;; case characters. Using cipher.txt (right click and 'Save Link/Target
  ;; As...'), a file containing the encrypted ASCII codes, and the knowledge
  ;; that the plain text must contain common English words, decrypt the message
  ;; and find the sum of the ASCII values in the original text.
  (let* ((data (-<> "data/59-cipher.txt"
                 read-file-into-string
                 (substitute #\space #\, <>)
                 read-all-from-string))
         (raw-words (-<> "/usr/share/dict/words"
                      read-file-into-string
                      (cl-strings:split <> #\newline)
                      (mapcar #'string-downcase <>)))
         (words (make-hash-set :test 'equal :initial-contents raw-words)))
    (labels
        ((stringify (codes)
           (map 'string #'code-char codes))
         (apply-cipher (key)
           (iterate (for number :in data)
                    (for k :in-looping key)
                    (collect (logxor number k))))
         (score-keyword (keyword)
           (-<> (apply-cipher keyword)
             (stringify <>)
             (string-downcase <>)
             (cl-strings:split <>)
             (remove-if-not (curry #'hset-contains-p words) <>)
             length))
         (answer (keyword)
           ;; (pr (stringify keyword)) ; keyword is "god", lol
           (sum (apply-cipher keyword))))
      (iterate (for-nested ((a :from (char-code #\a) :to (char-code #\z))
                            (b :from (char-code #\a) :to (char-code #\z))
                            (c :from (char-code #\a) :to (char-code #\z))))
               (for keyword = (list a b c))
               (finding (answer keyword) :maximizing (score-keyword keyword))))))

(defun problem-60 ()
  ;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
  ;; primes and concatenating them in any order the result will always be prime.
  ;; For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of
  ;; these four primes, 792, represents the lowest sum for a set of four primes
  ;; with this property.
  ;;
  ;; Find the lowest sum for a set of five primes for which any two primes
  ;; concatenate to produce another prime.
  (labels-memoized ((concatenates-prime-p (a b)
                      (and (primep (concatenate-integers a b))
                           (primep (concatenate-integers b a)))))
    (flet ((satisfiesp (prime primes)
             (every (curry #'concatenates-prime-p prime) primes)))
      (iterate
        main
        ;; 2 can never be part of the winning set, because if you concatenate it
        ;; in the last position you get an even number.
        (with primes = (subseq (sieve 10000) 1))
        (for a :in-vector primes :with-index ai)
        (iterate
          (for b :in-vector primes :with-index bi :from (1+ ai))
          (when (satisfiesp b (list a))
            (iterate
              (for c :in-vector primes :with-index ci :from (1+ bi))
              (when (satisfiesp c (list a b))
                (iterate
                  (for d :in-vector primes :with-index di :from (1+ ci))
                  (when (satisfiesp d (list a b c))
                    (iterate
                      (for e :in-vector primes :from (1+ di))
                      (when (satisfiesp e (list a b c d))
                        (in main (return-from problem-60 (+ a b c d e)))))))))))))))

(defun problem-61 ()
  ;; Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers
  ;; are all figurate (polygonal) numbers and are generated by the following
  ;; formulae:
  ;;
  ;; Triangle	 	P3,n=n(n+1)/2	 	1, 3, 6, 10, 15, ...
  ;; Square	 	P4,n=n²		  	1, 4, 9, 16, 25, ...
  ;; Pentagonal	 	P5,n=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
  ;; Hexagonal	 	P6,n=n(2n−1)	 	1, 6, 15, 28, 45, ...
  ;; Heptagonal	 	P7,n=n(5n−3)/2	 	1, 7, 18, 34, 55, ...
  ;; Octagonal	 	P8,n=n(3n−2)	 	1, 8, 21, 40, 65, ...
  ;;
  ;; The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three
  ;; interesting properties.
  ;;
  ;; 1. The set is cyclic, in that the last two digits of each number is the
  ;;    first two digits of the next number (including the last number with the
  ;;    first).
  ;; 2. Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and
  ;;    pentagonal (P5,44=2882), is represented by a different number in the
  ;;    set.
  ;; 3. This is the only set of 4-digit numbers with this property.
  ;;
  ;; Find the sum of the only ordered set of six cyclic 4-digit numbers for
  ;; which each polygonal type: triangle, square, pentagonal, hexagonal,
  ;; heptagonal, and octagonal, is represented by a different number in the set.
  (labels ((numbers (generator)
             (iterate (for i :from 1)
                      (for n = (funcall generator i))
                      (while (<= n 9999))
                      (when (>= n 1000)
                        (collect n))))
           (split (number)
             (truncate number 100))
           (prefix (number)
             (when number
               (nth-value 0 (split number))))
           (suffix (number)
             (when number
               (nth-value 1 (split number))))
           (matches (prefix suffix number)
             (multiple-value-bind (p s)
                 (split number)
               (and (or (not prefix)
                        (= prefix p))
                    (or (not suffix)
                        (= suffix s)))))
           (choose (numbers used prefix &optional suffix)
             (-<> numbers
               (remove-if-not (curry #'matches prefix suffix) <>)
               (set-difference <> used)))
           (search-sets (sets)
             (recursively ((sets sets)
                           (path nil))
               (destructuring-bind (set . remaining) sets
                 (if remaining
                   ;; We're somewhere in the middle, recur on any number whose
                   ;; prefix matches the suffix of the previous element.
                   (iterate
                     (for number :in (choose set path (suffix (car path))))
                     (recur remaining (cons number path)))
                   ;; We're on the last set, we need to find a number that fits
                   ;; between the penultimate element and first element to
                   ;; complete the cycle.
                   (when-let*
                       ((init (first (last path)))
                        (prev (car path))
                        (final (choose set path (suffix prev) (prefix init))))
                     (return-from problem-61
                       (sum (reverse (cons (first final) path))))))))))
    (map-permutations #'search-sets
                      (list (numbers #'triangle)
                            (numbers #'square)
                            (numbers #'pentagon)
                            (numbers #'hexagon)
                            (numbers #'heptagon)
                            (numbers #'octagon)))))

(defun problem-62 ()
  ;; The cube, 41063625 (345³), can be permuted to produce two other cubes:
  ;; 56623104 (384³) and 66430125 (405³). In fact, 41063625 is the smallest cube
  ;; which has exactly three permutations of its digits which are also cube.
  ;;
  ;; Find the smallest cube for which exactly five permutations of its digits
  ;; are cube.
  (let ((scores (make-hash-table))) ; canonical-repr => (count . first-cube)
    ;; Strategy from http://www.mathblog.dk/project-euler-62-cube-five-permutations/
    (labels ((canonicalize (cube)
               (digits-to-number (sort (digits cube) #'>)))
             (mark (cube)
               (incf (car (ensure-gethash (canonicalize cube) scores
                                          (cons 0 cube))))))
      (iterate
        (for cube :key #'cube :from 1)
        (finding (cdr (gethash (canonicalize cube) scores))
                 :such-that (= 5 (mark cube)))))))


(defun problem-74 ()
  ;; The number 145 is well known for the property that the sum of the factorial
  ;; of its digits is equal to 145:
  ;;
  ;; 1! + 4! + 5! = 1 + 24 + 120 = 145
  ;;
  ;; Perhaps less well known is 169, in that it produces the longest chain of
  ;; numbers that link back to 169; it turns out that there are only three such
  ;; loops that exist:
  ;;
  ;; 169 → 363601 → 1454 → 169
  ;; 871 → 45361 → 871
  ;; 872 → 45362 → 872
  ;;
  ;; It is not difficult to prove that EVERY starting number will eventually get
  ;; stuck in a loop. For example,
  ;;
  ;; 69 → 363600 → 1454 → 169 → 363601 (→ 1454)
  ;; 78 → 45360 → 871 → 45361 (→ 871)
  ;; 540 → 145 (→ 145)
  ;;
  ;; Starting with 69 produces a chain of five non-repeating terms, but the
  ;; longest non-repeating chain with a starting number below one million is
  ;; sixty terms.
  ;;
  ;; How many chains, with a starting number below one million, contain exactly
  ;; sixty non-repeating terms?
  (labels ((digit-factorial (n)
             (sum (mapcar #'factorial (digits n))))
           (term-count (n)
             (iterate (for i :initially n :then (digit-factorial i))
                      (until (member i prev))
                      (collect i :into prev)
                      (counting t))))
    (iterate (for i :from 1 :below 1000000)
             (counting (= 60 (term-count i))))))

(defun problem-145 ()
  ;; Some positive integers n have the property that the sum [ n + reverse(n) ]
  ;; consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and
  ;; 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and
  ;; 904 are reversible. Leading zeroes are not allowed in either n or
  ;; reverse(n).
  ;;
  ;; There are 120 reversible numbers below one-thousand.
  ;;
  ;; How many reversible numbers are there below one-billion (10^9)?
  (flet ((reversiblep (n)
           (let ((reversed (reverse-integer n)))
             (values (unless (zerop (digit 0 n))
                       (every #'oddp (digits (+ n reversed))))
                     reversed))))
    (iterate
      ;; TODO: improve this one
      ;; (with limit = 1000000000) there are no 9-digit reversible numbers...
      (with limit = 100000000)
      (with done = (make-array limit :element-type 'bit :initial-element 0))
      (for i :from 1 :below limit)
      (unless (= 1 (aref done i))
        (for (values reversible j) = (reversiblep i))
        (setf (aref done j) 1)
        (when reversible
          (sum (if (= i j) 1 2)))))))


;;;; Tests --------------------------------------------------------------------
(def-suite :euler)
(in-suite :euler)

(test p1 (is (= 233168 (problem-1))))
(test p2 (is (= 4613732 (problem-2))))
(test p3 (is (= 6857 (problem-3))))
(test p4 (is (= 906609 (problem-4))))
(test p5 (is (= 232792560 (problem-5))))
(test p6 (is (= 25164150 (problem-6))))
(test p7 (is (= 104743 (problem-7))))
(test p8 (is (= 23514624000 (problem-8))))
(test p9 (is (= 31875000 (problem-9))))
(test p10 (is (= 142913828922 (problem-10))))
(test p11 (is (= 70600674 (problem-11))))
(test p12 (is (= 76576500 (problem-12))))
(test p13 (is (= 5537376230 (problem-13))))
(test p14 (is (= 837799 (problem-14))))
(test p15 (is (= 137846528820 (problem-15))))
(test p16 (is (= 1366 (problem-16))))
(test p17 (is (= 21124 (problem-17))))
(test p18 (is (= 1074 (problem-18))))
(test p19 (is (= 171 (problem-19))))
(test p20 (is (= 648 (problem-20))))
(test p21 (is (= 31626 (problem-21))))
(test p22 (is (= 871198282 (problem-22))))
(test p23 (is (= 4179871 (problem-23))))
(test p24 (is (= 2783915460 (problem-24))))
(test p25 (is (= 4782 (problem-25))))
(test p26 (is (= 983 (problem-26))))
(test p27 (is (= -59231 (problem-27))))
(test p28 (is (= 669171001 (problem-28))))
(test p29 (is (= 9183 (problem-29))))
(test p30 (is (= 443839 (problem-30))))
(test p31 (is (= 73682 (problem-31))))
(test p32 (is (= 45228 (problem-32))))
(test p33 (is (= 100 (problem-33))))
(test p34 (is (= 40730 (problem-34))))
(test p35 (is (= 55 (problem-35))))
(test p36 (is (= 872187 (problem-36))))
(test p37 (is (= 748317 (problem-37))))
(test p38 (is (= 932718654 (problem-38))))
(test p39 (is (= 840 (problem-39))))
(test p40 (is (= 210 (problem-40))))
(test p41 (is (= 7652413 (problem-41))))
(test p42 (is (= 162 (problem-42))))
(test p43 (is (= 16695334890 (problem-43))))
(test p44 (is (= 5482660 (problem-44))))
(test p45 (is (= 1533776805 (problem-45))))
(test p46 (is (= 5777 (problem-46))))
(test p47 (is (= 134043 (problem-47))))
(test p48 (is (= 9110846700 (problem-48))))
(test p49 (is (= 296962999629 (problem-49))))
(test p50 (is (= 997651 (problem-50))))
(test p51 (is (= 121313 (problem-51))))
(test p52 (is (= 142857 (problem-52))))
(test p53 (is (= 4075 (problem-53))))
(test p54 (is (= 376 (problem-54))))
(test p55 (is (= 249 (problem-55))))
(test p56 (is (= 972 (problem-56))))
(test p57 (is (= 153 (problem-57))))
(test p58 (is (= 26241 (problem-58))))
(test p59 (is (= 107359 (problem-59))))
(test p60 (is (= 26033 (problem-60))))
(test p61 (is (= 28684 (problem-61))))
(test p62 (is (= 127035954683 (problem-62))))


(test p74 (is (= 402 (problem-74))))
(test p145 (is (= 608720 (problem-145))))


;; (run! :euler)
