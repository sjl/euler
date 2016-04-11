(in-package #:euler.utils)

(defun random-exclusive (min max)
  "Return an integer in the range (`min`, `max`)."
  (+ 1 min (random (- max min 1))))

(defun dividesp (n divisor)
  "Return whether `n` is evenly divisible by `divisor`."
  (zerop (mod n divisor)))


(defmacro repeat (n &body body)
  "Repeat `body` `n` times."
  `(dotimes (,(gensym) ,n)
     ,@body))
