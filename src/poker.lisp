(in-package :euler.poker)

;;;; Parsing ------------------------------------------------------------------
(defun parse-value (char)
  (switch (char :test #'char=)
    (#\T 10)
    (#\J 11)
    (#\Q 12)
    (#\K 13)
    (#\A 14)
    (t (digit-char-p char))))

(defun parse-suit (char)
  (eswitch (char :test #'char=)
    (#\D 'diamonds)
    (#\S 'spades)
    (#\H 'hearts)
    (#\C 'clubs)))

(defun parse-card (card)
  (list (parse-value (aref card 0))
        (parse-suit (aref card 1))))


;;;; Utils --------------------------------------------------------------------
(defun emptyp (sequence)
  (if (listp sequence)
    (null sequence)
    (= 0 (length sequence))))

(defun all-equal (sequence &key (test #'eql))
  (or (emptyp sequence)
      (not (find (elt sequence 0) sequence :start 1 :test-not test))))

(defun high-card-value (cards)
  (apply #'max (mapcar #'first cards)))

(defun same-suit-p (cards)
  (all-equal (mapcar #'second cards)))

(defun run-p (cards)
  (-<> cards
    (mapcar #'first <>)
    (sort <> #'<)
    (n-grams 2 <>)
    (mapcar (curry #'apply #'-) <>)
    (apply #'= -1 <>)))

(defun groupings (cards)
  (-<> cards
    (mapcar #'first <>)
    (equivalence-classes #'= <>)))

(defun pairs (cards)
  (remove 2 (groupings cards) :key #'length :test-not #'=))


;;;; Hand Predicates ----------------------------------------------------------
;;; These return `nil` if the hand is not of the appropriate type, or a list of
;;; card values if it is.  E.g. a hand with a 9-high straight would return (10),
;;; and a full house of 7's and 2's would return (7 2).

(defun royal-flush-p (hand)
  (when (and (straight-flush-p hand)
             (member 14 hand :key #'first))
    (list 14)))

(defun straight-flush-p (hand)
  (when (and (same-suit-p hand)
             (run-p hand))
    (list (high-card-value hand))))

(defun four-of-a-kind-p (hand)
  (when-let* ((group (find 4 (groupings hand) :key #'length)))
    (list (first group))))

(defun full-house-p (hand)
  (let ((groups (groupings hand)))
    (when (euler::set-equal '(3 2) (mapcar #'length groups))
      (mapcar #'first groups))))

(defun flush-p (hand)
  (when (same-suit-p hand)
    (list (high-card-value hand))))

(defun straight-p (hand)
  (when (run-p hand)
    (list (high-card-value hand))))

(defun three-of-a-kind-p (hand)
  (when-let* ((group (find 3 (groupings hand) :key #'length)))
    (list (first group))))

(defun two-pair-p (hand)
  (let ((pairs (pairs hand)))
    (when (= 2 (length pairs))
      (sort (mapcar #'first pairs) #'>))))

(defun one-pair-p (hand)
  (let ((pairs (pairs hand)))
    (when (= 1 (length pairs))
      (list (first (first pairs))))))


;;;; Hand Comparison ----------------------------------------------------------
(defun hand-value (hand)
  (acond
    ((royal-flush-p hand) (values 9 it))
    ((straight-flush-p hand) (values 8 it))
    ((four-of-a-kind-p hand) (values 7 it))
    ((full-house-p hand) (values 6 it))
    ((flush-p hand) (values 5 it))
    ((straight-p hand) (values 4 it))
    ((three-of-a-kind-p hand) (values 3 it))
    ((two-pair-p hand) (values 2 it))
    ((one-pair-p hand) (values 1 it))
    (t (values 0 (list (high-card-value hand))))))


(defun tie-break-p (values-1 values-2)
  (iterate (for c1 :in (sort (copy-list values-1) #'>))
           (for c2 :in (sort (copy-list values-2) #'>))
           (cond ((< c1 c2) (return 2))
                 ((> c1 c2) (return 1)))))


(defun poker-hand-beats-p (hand1 hand2)
  (multiple-value-bind* (((hand-value-1 components-1) (hand-value hand1))
                         ((hand-value-2 components-2) (hand-value hand2)))
    (cond ((< hand-value-1 hand-value-2) nil)
          ((> hand-value-1 hand-value-2) t)
          (t (ecase (tie-break-p components-1 components-2)
               ((1) t)
               ((2) nil)
               ((nil) (ecase (tie-break-p (mapcar #'first hand1)
                                          (mapcar #'first hand2))
                        ((1) t)
                        ((2) nil))))))))
