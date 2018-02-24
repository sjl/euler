(in-package :euler.hungarian)

;;;; Hungarian/Munkres Algorithm ----------------------------------------------
;;; This is an implementation of the Hungarian algorithm for finding a minimal
;;; assignment for a cost matrix in polynomial (O(n³)) time.
;;;
;;; Useful references:
;;;
;;; * http://www.netlib.org/utk/lsi/pcwLSI/text/node222.html
;;; * http://csclab.murraystate.edu/~bob.pilgrim/445/munkres.html


;;;; Data ---------------------------------------------------------------------
(defstruct (assignment-problem (:conc-name ap-)
                               (:constructor make-assignment-problem%))
  original-matrix
  cost-matrix
  rows
  cols
  starred-rows
  starred-cols
  covered-rows
  covered-cols
  primed-rows
  primed-cols)

(define-with-macro (assignment-problem :conc-name ap-)
  original-matrix
  cost-matrix
  rows
  cols
  starred-rows
  starred-cols
  covered-rows
  covered-cols
  primed-rows
  primed-cols)

(defun make-assignment-problem (matrix)
  (destructuring-bind (rows cols) (array-dimensions matrix)
    (make-assignment-problem%
      :original-matrix matrix
      :cost-matrix (copy-array matrix)
      :rows rows
      :cols cols
      :starred-rows (make-array rows :initial-element nil)
      :starred-cols (make-array cols :initial-element nil)
      :covered-rows (make-array rows :initial-element nil)
      :covered-cols (make-array cols :initial-element nil)
      :primed-rows (make-array rows :initial-element nil)
      :primed-cols (make-array cols :initial-element nil))))


;;;; Debug --------------------------------------------------------------------
(defun dump (data)
  (with-assignment-problem (data)
    (format t "   ~{   ~A ~^ ~}~%"
            (iterate (for col :below cols)
                     (collect (if (col-covered-p data col) #\X #\space))))
    (dotimes (row rows)
      (format t "~A [~{~4D~A~A~^ ~}]~%"
              (if (row-covered-p data row) #\X #\space)
              (iterate (for col :below cols)
                       (collect (aref cost-matrix row col))
                       (collect
                         (if (starredp data row col)
                           #\*
                           #\space))
                       (collect
                         (if (primedp data row col)
                           #\'
                           #\space))))))
  (pr))


;;;; Marking ------------------------------------------------------------------
(defun mark (row col row-vector col-vector)
  (setf (aref row-vector row) col
        (aref col-vector col) row))

(defun unmark (row col row-vector col-vector)
  ;; This is a bit fucky.
  ;;
  ;; Intuitively you would think that there should be at most one starred entry
  ;; in each row/col, and at the end of each step this is true.  But when we're
  ;; processing the alternating star/prime list, we start with the prime and
  ;; star it before moving to the next entry in the list, which is what removes
  ;; the existing star.
  ;;
  ;; If we just blindly nil out both vectors we might overwrite the coordinate
  ;; that was just starred.  The solution is to make sure we're only niling out
  ;; data for the actual thing we're unmarking.
  ;;
  ;; Another (possibly cleaner?) solution would be to reverse the alternating
  ;; prime/star list.  That was we'd unstar the existing zeros before starring
  ;; their primed twin, and everything would work nicely.
  (when (eql (aref row-vector row) col)
    (setf  (aref row-vector row) nil))
  (when (eql (aref col-vector col) row)
    (setf (aref col-vector col) nil)))


;;;; Starring -----------------------------------------------------------------
(defun star (data row col)
  (with-assignment-problem (data)
    (mark row col starred-rows starred-cols)))

(defun unstar (data row col)
  (with-assignment-problem (data)
    (unmark row col starred-rows starred-cols)))

(defun row-starred-p (data row)
  (aref (ap-starred-rows data) row))

(defun col-starred-p (data col)
  (aref (ap-starred-cols data) col))

(defun starred-col-for-row (data row)
  (aref (ap-starred-rows data) row))

(defun starred-row-for-col (data col)
  (aref (ap-starred-cols data) col))

(defun starred-list (data)
  (with-assignment-problem (data)
    (gathering
      (dotimes (row rows)
        (gather (cons row (starred-col-for-row data row)))))))

(defun starredp (data row col)
  (eql (starred-col-for-row data row) col))


;;;; Priming ------------------------------------------------------------------
(defun prime (data row col)
  (with-assignment-problem (data)
    (mark row col primed-rows primed-cols)))

(defun unprime (data row col)
  (with-assignment-problem (data)
    (unmark row col primed-rows primed-cols)))

(defun unprime-all (data)
  (fill (ap-primed-rows data) nil)
  (fill (ap-primed-cols data) nil))

(defun primed-col-for-row (data row)
  (aref (ap-primed-rows data) row))

(defun primed-row-for-col (data col)
  (aref (ap-primed-cols data) col))

(defun primedp (data row col)
  (eql (primed-col-for-row data row) col))


;;;; Covering -----------------------------------------------------------------
(defun cover-row (data row)
  (setf (aref (ap-covered-rows data) row) t))

(defun cover-col (data col)
  (setf (aref (ap-covered-cols data) col) t))

(defun uncover-row (data row)
  (setf (aref (ap-covered-rows data) row) nil))

(defun uncover-col (data col)
  (setf (aref (ap-covered-cols data) col) nil))

(defun row-covered-p (data row)
  (aref (ap-covered-rows data) row))

(defun col-covered-p (data col)
  (aref (ap-covered-cols data) col))

(defun uncover-all (data)
  (fill (ap-covered-rows data) nil)
  (fill (ap-covered-cols data) nil))


(defmacro-driver (FOR var INDEXES-OF element IN vector)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (vec el)
      `(progn
         (with ,vec = ,vector)
         (with ,el = ,element)
         (,kwd ,var :next
          (or (position ,el ,vec :start (if-first-time 0 (1+ ,var)))
              (terminate)))))))

(defmacro-driver (FOR var IN-UNCOVERED-ROWS assignment-problem)
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,var :indexes-of nil :in (ap-covered-rows ,assignment-problem))))

(defmacro-driver (FOR var IN-UNCOVERED-COLS assignment-problem)
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,var :indexes-of nil :in (ap-covered-cols ,assignment-problem))))

(defmacro-driver (FOR var IN-COVERED-ROWS assignment-problem)
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,var :indexes-of t :in (ap-covered-rows ,assignment-problem))))

(defmacro-driver (FOR var IN-COVERED-COLS assignment-problem)
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,var :indexes-of t :in (ap-covered-cols ,assignment-problem))))


(defun all-rows-covered-p (data)
  (every #'identity (ap-covered-rows data)))

(defun all-cols-covered-p (data)
  (every #'identity (ap-covered-cols data)))


;;;; Incrementing -------------------------------------------------------------
(defun incf-row (data row i)
  (with-assignment-problem (data)
    (dotimes (col cols)
      (incf (aref cost-matrix row col) i))))

(defun incf-col (data col i)
  (with-assignment-problem (data)
    (dotimes (row rows)
      (incf (aref cost-matrix row col) i))))

(defun decf-row (data row i)
  (incf-row data row (- i)))

(defun decf-col (data col i)
  (incf-col data col (- i)))


;;;; Step 1: Initialization ---------------------------------------------------
(defun subtract-smallest-element-in-col (data col)
  (decf-col data col (iterate
                       (for row :below (ap-rows data))
                       (minimizing (aref (ap-cost-matrix data) row col)))))


(defun initial-subtraction (data)
  ;; The first step is to subtract the smallest item in each column from all
  ;; entries in the column.
  (with-assignment-problem (data)
    (dotimes (col cols)
      (subtract-smallest-element-in-col data col))))

(defun initial-zero-starring (data)
  ;; Find a zero Z in the distance matrix.
  ;;
  ;; If there is no starred zero already in its row or column, star this zero.
  ;;
  ;; Repeat steps 1.1, 1.2 until all zeros have been considered.
  (with-assignment-problem (data)
    (iterate
      ;; This could be faster if we split the iteration and bailed after marking
      ;; the first thing in a row, but meh, it's cleaner this way.
      (for (value row col) :in-array cost-matrix)
      (when (and (zerop value)
                 (not (row-starred-p data row))
                 (not (col-starred-p data col)))
        (star data row col)))))


(defun step-1 (data)
  (initial-subtraction data)
  (initial-zero-starring data)
  (step-2 data))


;;;; Step 2: Z* Count and Solution Assessment ---------------------------------
(defun cover-all-starred-columns (data)
  (with-assignment-problem (data)
    (dotimes (col cols)
      (when (col-starred-p data col)
        (cover-col data col)))))


(defun step-2 (data)
  ;; Cover every column containing a Z*.
  ;;
  ;; Terminate the algorithm if all columns are covered. In this case, the
  ;; locations of the  entries in the matrix provide the solution to the
  ;; assignment problem.
  (cover-all-starred-columns data)
  (if (all-cols-covered-p data)
    (report-solution data)
    (step-3 data)))



;;;; Step 3: Main Zero Search -------------------------------------------------
(defun find-uncovered-zero (data)
  (iterate (for row :in-uncovered-rows data)
           (iterate (for col :in-uncovered-cols data)
                    (when (zerop (aref (ap-cost-matrix data) row col))
                      (return-from find-uncovered-zero (values t row col)))))
  (values nil nil nil))

(defun step-3 (data)
  ;; Find an uncovered Z in the distance matrix and prime it, Z -> Z'. If no
  ;; such zero exists, go to Step 5.
  (multiple-value-bind (found row col) (find-uncovered-zero data)
    (if (not found)
      (step-5 data)
      (progn
        (prime data row col)
        (let ((starred-col (starred-col-for-row data row)))
          (if (not starred-col)
            ;; If No Z* exists in the row of the Z', go to Step 4.
            (step-4 data row col)
            ;; If a Z* exists, cover this row and uncover the column of the Z*.
            ;; Return to Step 3.1 to find a new Z.
            (progn (cover-row data row)
                   (uncover-col data starred-col)
                   (step-3 data))))))))


;;;; Step 4: Increment Set of Starred Zeros -----------------------------------
(defun construct-zero-sequence (data initial-row initial-col)
  (gathering
    (labels
        ((find-next-starred (prime-col)
           ;; The Z* in the same column as the given Z', if one exists.
           (let ((star-row (starred-row-for-col data prime-col)))
             (if (null star-row)
               (values nil nil)
               (values star-row prime-col))))
         (find-next-primed (star-row)
           ;; The Z' in the same row as the given Z* (there will always be one).
           (values star-row (primed-col-for-row data star-row)))
         (mark-starred (row col)
           (when row
             (gather (cons row col))
             (multiple-value-call #'mark-primed (find-next-primed row))))
         (mark-primed (row col)
           (gather (cons row col))
           (multiple-value-call #'mark-starred (find-next-starred col))))
      (mark-primed initial-row initial-col))))

(defun process-zero-sequence (data zeros)
  ;; Unstar each starred zero of the sequence.
  ;;
  ;; Star each primed zero of the sequence, thus increasing the number of
  ;; starred zeros by one.
  (iterate (for (row . col) :in zeros)
           (for primed? :first t :then (not primed?))
           (if primed?
             (star data row col)
             (unstar data row col))))

(defun step-4 (data row col)
  (process-zero-sequence data (construct-zero-sequence data row col))
  (unprime-all data)
  (uncover-all data)
  (step-2 data))


;;;; Step 5: New Zero Manufactures --------------------------------------------
(defun find-smallest-uncovered-entry (data)
  (iterate
    main
    (for row :in-uncovered-rows data)
    (iterate (for col :in-uncovered-cols data)
             (in main (minimizing (aref (ap-cost-matrix data) row col))))))

(defun incf-covered-rows (data i)
  (iterate (for row :in-covered-rows data)
           (incf-row data row i)))

(defun decf-uncovered-cols (data i)
  (iterate (for col :in-uncovered-cols data)
           (decf-col data col i)))


(defun step-5 (data)
  ;; Let h be the smallest uncovered entry in the (modified) distance matrix.
  ;;
  ;; Add h to all covered rows.
  ;;
  ;; Subtract h from all uncovered columns
  ;;
  ;; Return to Step 3, without altering stars, primes, or covers.
  (let ((i (find-smallest-uncovered-entry data)))
    (incf-covered-rows data i)
    (decf-uncovered-cols data i)
    (step-3 data)))


;;;; Reporting Solution -------------------------------------------------------
(defun report-solution (data)
  (starred-list data))


;;;; API ----------------------------------------------------------------------
(defun find-minimal-assignment (matrix)
  (step-1 (make-assignment-problem matrix)))


;;;; Scratch ------------------------------------------------------------------
;; (untrace)
