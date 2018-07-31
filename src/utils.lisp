(in-package :euler)

;;;; Iterate ------------------------------------------------------------------
(defmacro-driver (FOR var ITERATING function SEED value &optional
                  INCLUDE-SEED include-seed?)
  "Iterate `var` over the series `(f seed), (f (f seed)), (f (f (f seed))), ...`.

  If `include-seed` is given the series will start with the seed itself first.

  Examples:

    (iterate (for n :iterating #'digital-sum :seed 10123456789)
             (collect n)
             (until (< n 10)))
    ; => (46 10 1)

    (iterate (for n :iterating #'digital-sum :seed 10123456789 :include-seed t)
             (collect n)
             (until (< n 10)))
    ; => (10123456789 46 10 1)

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (f)
      `(progn
         (with ,f = ,function)
         (,kwd ,var
          :initially ,(if include-seed?
                        value
                        `(funcall ,f ,value))
          :then (funcall ,f ,var))))))

(defmacro-driver (FOR var IN-LOOPING list)
  "Iterate `var` over `list`, looping when the end is reached.

  Example:

    (iterate (for x :in-looping '(1 2 3))
             (repeat 5)
             (collect x))
    ; => (1 2 3 1 2)

  "
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
  "Iterate `var` numerically as with `FOR`, applying `function` to the numbers.

  Example:

    (iterate (for x :key #'evenp :from 0 :to 8) (collect x))
    ; => (T NIL T NIL T NIL T NIL T)

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (i f)
      `(progn
         (with ,f = ,function)
         (generate ,i ,@(losh.iterate::expand-iterate-sequence-keywords))
         (,kwd ,var :next (funcall ,f (next ,i)))))))

(defmacro-driver (FOR var IN-LIST list &optional
                  BY (step-function '#'cdr)
                  INITIALLY (initial-value nil initial-value?)
                  FINALLY (final-value nil final-value?))
  "Iterate `var` over `list` like vanilla `FOR var IN`, but with more options.

  If `initially` is given, `var` will be bound to it on the first iteration,
  before proceeding to iterate over the list.

  If `finally` is given, `var` will be bound to it on one more iteration after
  the end of the list has been reached.

  Examples:

    (iterate (for x :in-list '(1 2 3))
             (collect x))
    ; => (1 2 3)

    (iterate (for x :in-list '(1 2 3) :initially 0)
             (collect x))
    ; => (0 1 2 3)

    (iterate (for x :in-list '(1 2 3) :finally 4)
             (collect x))
    ; => (1 2 3 4)

    (iterate (for x :in-list '(1 2 3) :initially 0 :finally 4)
             (collect x))
    ; => (0 1 2 3 4)

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (l i f done)
      `(progn
         (with ,l = ,list)
         ,@(when initial-value?
             `((with ,i = ,initial-value)))
         ,@(when final-value?
             `((with ,f = ,final-value)
               (with ,done = nil)))
         (,kwd ,var :next
          (cond
            ,@(when initial-value?
               `(((first-time-p) ,i)))
            ,@(when final-value?
               `((,done (terminate))))
            ((atom ,l)
             ,@(if final-value?
                 `((setf ,done t) ,f)
                 `((terminate))))
            (t (prog1 (car ,l) (setf ,l (funcall ,step-function ,l))))))))))

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

(defmacro-driver (FOR var IN-CSV-FILE filename &optional
                  KEY (key #'identity)
                  DELIMITER (delimiter #\,)
                  RESULT-TYPE (result-type ''list)
                  SKIP-HEADER (skip-header nil)
                  OMIT-EMPTY (omit-empty nil))
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (line% key% delimiter% result-type% omit-empty% skip-header%)
      `(progn
         (with ,key% = ,key)
         (with ,delimiter% = ,delimiter)
         (with ,result-type% = ,result-type)
         (with ,omit-empty% = ,omit-empty)
         (with ,skip-header% = ,skip-header)
         (generate ,line% :in-file ,filename :using #'read-line)
         (labels ((skip-empty ()
                    (when ,omit-empty%
                      (loop :while (string= "" ,line%) :do (next ,line%))))
                  (skip-header ()
                    (when ,skip-header%
                      (next ,line%)
                      (setf ,skip-header% nil)
                      (skip-empty)))
                  (next-line ()
                    (next ,line%)
                    (skip-empty)
                    (skip-header)
                    ,line%)
                  (parse-line (l)
                    (map ,result-type% ,key%
                         (cl-strings:split l ,delimiter%))))
           (,kwd ,var :next (parse-line (next-line))))))))


(defmacro when-first-time (&body body)
  `(if-first-time
     (progn ,@body)))

(defmacro unless-first-time (&body body)
  `(if-first-time
     nil
     (progn ,@body)))


;;;; Miscellaneous ------------------------------------------------------------
(defun-inlineable mutate (function list)
  "Destructively mutate each element of `list` in-place with `function`.

  Equivalent to (but can be faster than) `(map-into list function list)`.

  "
  (declare (optimize speed))
  (loop :with function = (ensure-function function)
        :for l :on list
        :do (setf (car l) (funcall function (car l))))
  list)

(defun sort< (sequence)
  (sort sequence #'<))

(defun binomial-coefficient (n k)
  "Return `n` choose `k`."
  ;; See https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  (iterate (for i :from 1 :to k)
           (multiplying (math (n + 1 - i) / i))))

(defun multiset-coefficient (n k)
  "Return `n` multichoose `k`."
  ;; See https://www.lucamoroni.it/the-dice-roll-sum-problem/ section 2.B
  (binomial-coefficient (+ n k -1) k))

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

(defun mapcar-long (function fill list &rest more-lists)
  "Like `mapcar`, but using the longest list, filling with `fill`."
  (declare (optimize speed))
  (flet ((head (list)
           (if (null list) fill (car list))))
    (iterate (with (the cons lists) = (cons list more-lists))
             (with function = (ensure-function function))
             (until (every #'null lists))
             (collect (apply function (mapcar #'head lists)))
             (map-into lists #'cdr lists))))

(defun phi (n)
  "Return `φ(n)` (Euler's totient function)."
  ;; https://en.wikipedia.org/wiki/Euler%27s_totient_function#Computing_Euler.27s_totient_function
  (* n (iterate (for p :in (prime-factors n))
                (multiplying (- 1 (/ p))))))

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

(defun subsequencep (needles haystack &key key (test #'eql))
  "Return whether `needles` is a (possibly non-contiguous) subsequence of `haystack`."
  (ctypecase haystack
    (list
      (every (lambda (el)
               (let ((result (member el haystack :key key :test test)))
                 (setf haystack (rest result))
                 result))
             needles))
    (sequence
      (let ((p 0))
        (every (lambda (el)
                 (setf p (position el haystack :start p :key key :test test)))
               needles)))))

(defun convert-to-multidimensional-array (nested-sequences)
  "Convert nested sequences into a single multidimensional array.

  All sequences for a given dimension must have the same length.  This is not
  checked.

  "
  (let* ((dimensions (gathering
                       (recursively ((s nested-sequences))
                         (when (typep s 'sequence)
                           (gather (length s))
                           (recur (elt s 0))))))
         (array (make-array dimensions))
         (i 0))
    (recursively ((s nested-sequences))
      (if (typep s 'sequence)
        (map nil #'recur s)
        (setf (row-major-aref array i) s
              i (1+ i))))
    array))

(defun barycentric (a b c point)
  "Return the barycentric coords of `point` with respect to the triangle `abc`.

  `a`, `b`, `c`, and `point` must be `vec2`s.

  The resulting `u`, `v`, and `w` barycentric coordinates will be returned as
  three values.

  "
  ;; https://gamedev.stackexchange.com/a/23745
  (let* ((v0 (vec2- b a))
         (v1 (vec2- c a))
         (v2 (vec2- point a))
         (d00 (vec2-dot v0 v0))
         (d01 (vec2-dot v0 v1))
         (d11 (vec2-dot v1 v1))
         (d20 (vec2-dot v2 v0))
         (d21 (vec2-dot v2 v1))
         (denom (- (* d00 d11) (* d01 d01)))
         (v (/ (- (* d11 d20) (* d01 d21)) denom))
         (w (/ (- (* d00 d21) (* d01 d20)) denom))
         (u (- 1 v w)))
    (values u v w)))


;;;; Digits -------------------------------------------------------------------
(defun digits-length (n &optional (radix 10))
  "Return how many digits `n` has in base `radix`."
  (if (zerop n)
    1
    (values (1+ (truncate (log (abs n) radix))))))

(defun digits (n &key (radix 10) from-end)
  "Return a fresh list of the digits of `n` in base `radix`.

  By default, the digits are returned high-order first, as you would read them.
  Use `from-end` to get them low-order first:

    (digits 123)             ; => (1 2 3)
    (digits 123 :from-end t) ; => (3 2 1)

  "
  (if from-end
    (iterate (for d :in-digits-of n :radix radix)
             (collect d))
    (iterate (for d :in-digits-of n :radix radix)
             (collect d :at :beginning))))


(defun-inlineable digital-sum (n &optional (radix 10))
  "Return the sum of the digits of `n` in base `radix`."
  (iterate (for digit :in-digits-of n :radix radix)
           (summing digit)))

(defun digital-root (n &optional (radix 10))
  "Return the digital root of `n` in base `radix`."
  (declare (inline digital-sum))
  (iterate (for i :first n :then (digital-sum i radix))
           (finding i :such-that (< i radix))))

(defun digital-roots (n &optional (radix 10))
  "Return a list of the digital roots of `n` in base `radix`."
  (declare (inline digital-sum))
  (iterate (for i :first n :then (digital-sum i radix))
           (collect i)
           (until (< i radix))))


(defun-inline append-digit (digit number &optional (radix 10))
  "Appened `digit` to `number` in base `radix` as a low-order digit."
  (+ digit (* number radix)))


(defun-inline nth-digit (n integer &optional (radix 10))
  "Return the `n`th digit of `integer` in base `radix`, counting from the right."
  (mod (truncate integer (expt radix n)) radix))


(defun digits-to-number (digits &key from-end (radix 10))
  "Concatenate `digits` to return an integer in base `radix`.

  If `from-end` is `t`, start at the end of the list.

  "
  (if digits
    (if from-end
      (iterate (for d :in digits)
               (for multiplier :first 1 :then (* radix multiplier))
               (summing (* multiplier d)))
      (reduce (lambda (total digit)
                (+ (* total radix) digit))
              digits))
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


(defun reverse-integer (n)
  (digits-to-number (nreverse (digits n))))


;;;; Divisors -----------------------------------------------------------------
(defun-inlineable divisors-up-to-square-root (n)
  (loop :for i :from 1 :to (floor (sqrt n))
        :when (zerop (rem n i))
        :collect i))


(defun unsorted-divisors (n &key proper)
  (if (= n 1)
    nil
    (iterate (for i :from 1 :to (sqrt n))
             (for (values divisor remainder) = (truncate n i))
             (when (zerop remainder)
               (collect i)
               (unless (or (= i divisor) ;; don't collect the square root twice
                           (and proper (= i 1))) ;; don't collect n if proper
                 (collect divisor))))))

(defun divisors (n &key proper)
  (sort< (unsorted-divisors n :proper proper)))

(defun count-divisors (n &key proper)
  ;; From *Recreations in the Theory of Numbers: The Queen of Mathematics
  ;; Entertains* by Albert Beiler.
  ;;
  ;; To find the number of divisors of n, we first find its prime factorization:
  ;;
  ;;   p₁^k₁ × p₂^k₂ × ... × pₓ^kₓ
  ;;
  ;; The divisors of n are each formed by some combination of these numbers.  To
  ;; find the total number of divisors we take ∏(k+1) for each exponent k in the
  ;; factorization.  This is because there are k₁+1 options for how many of the
  ;; first prime to include (because 0 is an option), k₂+1 options for the
  ;; second prime, etc.
  (- (iterate (for (nil . exponent) :in (prime-factorization-pairs n))
              (multiplying (1+ exponent)))
     (if proper 1 0)))

(defun sum-of-divisors (n &key proper)
  "Return the sum of the divisors of `n`.

  If `proper` is given, only include the proper divisors (i.e. not `n` itself).

  "
  (summation (unsorted-divisors n :proper proper)))

(defun product-of-divisors (n &key proper)
  ;; From *Recreations in the Theory of Numbers: The Queen of Mathematics
  ;; Entertains* by Albert Beiler.
  ;;
  ;; If `N` is a positive integer with `d` divisors, the product of all the
  ;; divisors of `N` is `N^(d/2)`.
  ;;
  ;; If we only want the product of the proper divisors, just divide `N` back
  ;; out at the end.
  (/ (expt n (/ (count-divisors n) 2))
     (if proper n 1)))


;;;; Collatz ------------------------------------------------------------------
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


;;;; Matrices and Vectors -----------------------------------------------------
(defun count-rows (matrix)
  (array-dimension matrix 0))

(defun count-cols (matrix)
  (array-dimension matrix 1))

(defun mat* (m n)
  (assert (= (count-cols m) (count-rows n))
      () "Cannot multiply incompatibly-sized matrices.")
  (let ((rows (count-rows m))
        (cols (count-cols n)))
    (iterate
      (with numbers = (count-cols m))
      (with result = (make-array (list rows cols) :element-type 'number))
      (for-nested ((row :from 0 :below rows)
                   (col :from 0 :below cols)))
      (setf (aref result row col)
            (iterate (for i :below numbers)
                     (summing (* (aref m row i)
                                 (aref n i col)))))
      (finally (return result)))))

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


(defun mat2 (a b c d)
  (let ((result (make-array (list 2 2) :element-type 'number)))
    (setf (aref result 0 0) a
          (aref result 0 1) b
          (aref result 1 0) c
          (aref result 1 1) d)
    result))


(defun vec2 (x y)
  (vector x y))

(defun vx (vec2)
  (aref vec2 0))

(defun vy (vec2)
  (aref vec2 1))

(defun vec2+ (a b)
  (vec2 (+ (vx a) (vx b))
        (+ (vy a) (vy b))))

(defun vec2- (a b)
  (vec2 (- (vx a) (vx b))
        (- (vy a) (vy b))))

(defun vec2-dot (a b)
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))))

(defun vec2= (a b)
  (and (= (vx a) (vx b))
       (= (vy a) (vy b))))


;;;; Fibonacci ----------------------------------------------------------------
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

(defun nth-fibonacci (n)
  ;; https://blog.paulhankin.net/fibonacci/
  (labels
      ((mexpt (matrix exponent)
         (cond ((zerop exponent) (mat2 1 0 0 1))
               ((= 1 exponent) matrix)
               ((evenp exponent)
                (mexpt (mat* matrix matrix)
                       (truncate exponent 2)))
               (t (mat* matrix
                        (mexpt (mat* matrix matrix)
                               (truncate exponent 2)))))))
    (-<> (mat2 1 1
               1 0)
      (mexpt <> n)
      (mv* <> (vec2 1 1))
      (aref <> 1))))


;;;; Factorial ----------------------------------------------------------------
(defun factorial% (n)
  (iterate (for i :from 1 :to n)
           (multiplying i)))

(defun precompute-factorials (size)
  (iterate
    (with cache = (make-array size :element-type 'integer))
    (for i :from 0 :below size)
    (setf (aref cache i) (factorial% i))
    (finally (return cache))))

(defparameter *factorial-cache-size* 2000)
(defparameter *factorial-cache* (precompute-factorials *factorial-cache-size*))

(defun factorial (n)
  (if (< n *factorial-cache-size*)
    (aref *factorial-cache* n)
    (factorial% n)))


;;;; Amicability --------------------------------------------------------------
(defun perfectp (n)
  (= n (sum-of-divisors n :proper t)))

(defun abundantp (n)
  (< n (sum-of-divisors n :proper t)))

(defun deficientp (n)
  (> n (sum-of-divisors n :proper t)))

(defun amicablep (m &optional n)
  "Return whether `m` and `n` are amicable numbers.

  If `n` is omitted the sum of the aliquot divisors of `m` will be used, and
  thus this function will return whether `m` is part of an amicable pair.

  "
  (nest (let ((sm (sum-of-divisors m :proper t))))
        (let ((n (or n sm))))
        (unless (= m n))
        (let ((sn (sum-of-divisors n :proper t))))
        (and (= m sn)
             (= n sm))))

(defun amicable-number-chain (n &optional (cutoff 50))
  "Return the amicable number chain of `n`, or `nil` if it is not part of one.

  If `cutoff` is given, stop searching (and return `nil`) after that many steps.

  Returns two values:

  * The chain (if any)
  * A boolean specifying whether the computation was aborted by hitting the cutoff.

  "
  ;; Perfection, amicability, and sociability are all special cases of this:
  ;;
  ;;   (defun perfectp  (n) (= 1 (length (amicable-number-chain n))))
  ;;   (defun amicablep (n) (= 2 (length (amicable-number-chain n))))
  ;;   (defun sociablep (n) (> 2 (length (amicable-number-chain n))))
  (iterate
    (for number :iterating (rcurry #'sum-of-divisors :proper t) :seed n)
    (for i :from 0)
    (cond ((and cutoff (= cutoff i))
           (return (values nil t)))

          ((or (= number 1) (member number result))
           (return (values nil nil)))

          ((= number n)
           (return (values (cons number result) nil)))

          (t (collect number :into result)))))

(defun aliquot-sequence (n &optional (cutoff 50))
  "Return the aliquot sequence starting with `n`.

  The sequence will end when any of the following occur:

  * The sequence hits zero.
  * The sequence encounters a cycle.
  * `cutoff` steps have been given.

  Returns two values:

  * The sequence.
  * A boolean specifying whether the cutoff was hit.

  "
  ;; https://en.wikipedia.org/wiki/Aliquot_sequence
  (iterate
    (for number :iterating (rcurry #'sum-of-divisors :proper t)
         :seed n :include-seed t)
    (for i :from 0)
    (cond ((and cutoff (= cutoff i)) (return (values result t)))
          ((member number result) (return (values result nil)))
          (t (collect number :into result)))
    (when (zerop number)
      (return (values result nil)))))


;;;; Chopping/Concating Numbers -----------------------------------------------
(defun-inline truncate-number-left (n amount &optional (radix 10))
  "Chop `amount` digits off the left side of `n` in base `radix`."
  (mod n (expt radix (- (digits-length n radix) amount))))

(defun-inline truncate-number-right (n amount &optional (radix 10))
  "Chop `amount` digits off the right side of `n` in base `radix`."
  (truncate n (expt radix amount)))


(defun concatenate-integers (&rest integers)
  "Concatenate each integer in `integers` and return a big ol' integer result."
  (values (parse-integer
            (format nil "~{~D~}" integers))))


;;;; Permutations and Combinations --------------------------------------------
(defun permutations (sequence &key length)
  (gathering (map-permutations #'gather sequence :length length)))

(defun combinations (sequence &key length)
  (gathering (map-combinations #'gather sequence :length length)))


(defun adjoin% (list item &rest keyword-args)
  (apply #'adjoin item list keyword-args))

(define-modify-macro adjoinf (item &rest keyword-args) adjoin%)




(defun pythagorean-triplet-p (a b c)
  (math a ^ 2 + b ^ 2 = c ^ 2))

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


;;;; Geometric Numbers --------------------------------------------------------
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
  (math n (n + 1) / 2))

(defun trianglep (n)
  "Return whether `n` is a triangle number."
  ;; http://mathforum.org/library/drmath/view/57162.html
  ;;
  ;; A number is triangular if and only if 8T + 1 is an odd perfect square.
  (let ((x (1+ (* 8 n))))
    (and (oddp x)
         (squarep x))))


(defun pentagon (n)
  (math (3 n ^ 2 - n) / 2)
  (* n (- (* 3 n) 1) 1/2))

(defun pentagonp (n)
  ;; We can ignore the - branch of the quadratic equation because negative
  ;; numbers aren't indexes.
  (dividesp (math (sqrt (24.0d0 n + 1) + 1))
            6))


(defun hexagon (n)
  (math (2 n - 1) * n))

(defun hexagonp (n)
  ;; We can ignore the - branch of the quadratic equation because negative
  ;; numbers aren't indexes.
  (dividesp (+ 1 (sqrt (1+ (* 8.0d0 n)))) 4))


(defun heptagon (n)
  (math (5 n ^ 2 - 3 n) / 2))


(defun octagon (n)
  (math 3 n ^ 2 - 2 n))


;;;; Matrices -----------------------------------------------------------------
(deftype matrix (&optional (element-type '*))
  `(array ,element-type (* *)))

(defun transpose-matrix (matrix)
  (check-type matrix matrix)
  (iterate (with (rows cols) = (array-dimensions matrix))
           (with result = (make-array (list cols rows)))
           (for-nested ((i :from 0 :below rows)
                        (j :from 0 :below cols)))
           (setf (aref result j i)
                 (aref matrix i j))
           (finally (return result))))

(defun rotate-matrix-clockwise (matrix)
  (check-type matrix matrix)
  (iterate (with (rows cols) = (array-dimensions matrix))
           (with result = (make-array (list cols rows)))
           (for source-row :from 0 :below rows)
           (for target-col = (- rows source-row 1))
           (dotimes (source-col cols)
             (for target-row = source-col)
             (setf (aref result target-row target-col)
                   (aref matrix source-row source-col)))
           (finally (return result))))

(defun rotate-matrix-counterclockwise (matrix)
  (check-type matrix matrix)
  (iterate (with (rows cols) = (array-dimensions matrix))
           (with result = (make-array (list cols rows)))
           (for source-row :from 0 :below rows)
           (for target-col = source-row)
           (dotimes (source-col cols)
             (for target-row = (- cols source-col 1))
             (setf (aref result target-row target-col)
                   (aref matrix source-row source-col)))
           (finally (return result))))


;;;; Probability --------------------------------------------------------------
(defun geometric-pmf (success-probability trials)
  "The probability mass function of the geometric distribution.

  Returns the probability that exactly `trials` trials will be required to see
  the first success in a series of Bernoulli trials with `success-probability`.

  "
  (* (expt (- 1 success-probability) (1- trials))
     success-probability))

(defun geometric-cdf (success-probability trials)
  "The cumulative distribution function of the geometric distribution.

  Returns the probability that `trials` or fewer trials will be required to see
  the first success in a series of Bernoulli trials with `success-probability`.

  "
  (- 1 (expt (- 1 success-probability) trials)))


;;;; Rounding -----------------------------------------------------------------
(defun round-decimal (number decimal-places &optional (rounder #'round))
  ;; http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Common_Lisp
  (coerce (let ((div (expt 10 decimal-places)))
            (/ (funcall rounder (* number div)) div))
          (type-of number)))

(defun round-to (number precision)
  "Round `number` to the given `precision`.

  Examples:

    (round-to 13 10)      ; => 10
    (round-to 15 10)      ; => 20
    (round-to 44 25)      ; => 50
    (round-to 457/87 1/2) ; => 11/2

  "
  (* precision (round number precision)))



;;;; A* Search ----------------------------------------------------------------
(defstruct path
  state
  (estimate 0)
  (cost 0)
  (previous nil))

(defun path-to-list (path &aux result)
  (recursively ((path path))
    (unless (null path)
      (push (path-state path) result)
      (recur (path-previous path))))
  result)

(defun astar (&key start neighbors goalp cost heuristic test)
  "Search for a path from `start` to a goal using A★.

  The following parameters are all required:

  * `start`: the starting state.

  * `neighbors`: a function that takes a state and returns all states reachable
    from it.

  * `goalp`: a predicate that takes a state and returns whether it is a goal.

  * `cost`: a function that takes two states `a` and `b` and returns the cost
    to move from `a` to `b`.

  * `heuristic`: a function that takes a state and estimates the distance
    remaining to the goal.

  * `test`: an equality predicate for comparing nodes.  It must be suitable for
    passing to `make-hash-table`.

  If the heuristic function is admissable (i.e. it never overestimates the
  remaining distance) the algorithm will find the shortest path.

  Note that `test` is required.  The only sensible default would be `eql`, but
  if you were using states that need a different predicate and forgot to pass it
  the algorithm would end up blowing the heap, which is unpleasant.

  "
  (let ((seen (make-hash-table :test test))
        (frontier (pileup:make-heap #'< :key #'path-estimate)))
    (labels ((mark-seen (path)
               (setf (gethash (path-state path) seen) (path-cost path)))
             (push-path (path)
               (mark-seen path)
               (pileup:heap-insert path frontier)))
      (iterate
        (initially (push-path (make-path :state start)))

        (for (values current found) = (pileup:heap-pop frontier))
        (unless found
          (return (values nil nil)))

        (for current-state = (path-state current))

        (when (funcall goalp current-state)
          (return (values (path-to-list current) t)))

        (for current-cost = (path-cost current))

        (iterate
          (for next-state :in (funcall neighbors current-state))
          (for next-cost = (+ current-cost (funcall cost current-state next-state)))
          (for (values seen-cost previously-seen) = (gethash next-state seen))
          (when (or (not previously-seen)
                    (< next-cost seen-cost))
            (for next-estimate = (+ next-cost (funcall heuristic next-state)))
            (push-path (make-path :state next-state
                                  :cost next-cost
                                  :estimate next-estimate
                                  :previous current))))))))
