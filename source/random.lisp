;;;; Copyright (C) 2005 -- 2021, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.


(defpackage :sigma/random
  (:use :common-lisp
        :sigma/behave
        :sigma/control
        :sigma/sequence)
  (:export :coin-toss
           :gauss
           :nshuffle
           :random-argument
           :random-array
           :random-element
           :random-in-range
           :random-in-ranges
           :random-range
           :randomize-array
           :shuffle))
(in-package :sigma/random)


;;; When x and y are two variables from [0, 1), uniformly distributed, then
;;;
;;;   cos(2*pi*x)*sqrt(-2*log(1-y))
;;;   sin(2*pi*x)*sqrt(-2*log(1-y))
;;;
;;; are two independent variables with normal distribution (mu = 0, sigma = 1).
;;;
;;; [This approach is from Python's random library.  The implementation used
;;; here is somewhat different though.  They say it is faster then the normal
;;; algorithm I'm used to, Kinderman and Monahan.]
(let ((next nil))
  (defun gauss (mu sigma)
    "This is the Gaussian distribution. Mu is the mean and sigma is the standard
    deviation."
    (let ((z next)
          (x (random 1.0))
          (y (random 1.0)))
      (setf next nil)
      (when (null z)
        (setf z    (* (cos (* 2 pi x))
                      (expt 0.5 (* -2 (log (- 1 y)))))
              next (* (sin (* 2 pi x))
                      (expt 0.5 (* -2 (log (- 1 y)))))))
      (+ mu (* z sigma)))))


(defgeneric random-element (sequence)
  (:documentation
   "Return a randomly chosen element from SEQUENCE, or NIL if SEQUENCE is
empty."))


(defmethod random-element ((list list))
  "This method returns a random element from a list."
  (when list
    (nth (random (length list)) list)))


(defmethod random-element ((array array))
  "This method returns a random element from an array."
  (when (plusp (array-total-size array))
    (row-major-aref array (random (array-total-size array)))))

(behavior 'random-element
  (should-be-null (random-element nil))
  (should-be-null (random-element #()))
  (should= 42 (random-element '(42)))
  (should= 7 (random-element #(7)))
  (dotimes (i 20)
    (should-be-true (member (random-element '(a b c)) '(a b c) :test #'eq))))


(defun random-argument (&rest rest)
  "Return one of the arguments REST chosen uniformly at random, or NIL if no
arguments are given."
  (random-element rest))

(behavior 'random-argument
  (should-be-null (random-argument))
  (should= 99 (random-argument 99))
  (dotimes (i 20)
    (should-be-true (member (random-argument :x :y) '(:x :y) :test #'eq))))


(defun coin-toss ()
  "Return T or NIL with equal probability, like a fair coin toss."
  (random-argument t nil))

;; This is a simple assertion to ensure that the distribution of coin tosses is
;; within our general assumptions for distribution.
(let ((nils 0)
      (ts 0))
  (loop for i from 1 to 100000
        do (if (coin-toss)
             (incf ts)
             (incf nils)))
  (assert (< 0.9 (/ ts nils) 1.1)))


(defun random-in-range (lower upper)
  "This function returns a random number in the range [lower, upper).  Lower
and upper may both be sequences, in which case their most extreme members."
  (when (sequence? lower)
    (setf lower (maximum lower)))
  (when (sequence? upper)
    (setf upper (minimum upper)))
  (assert (numberp lower))
  (assert (numberp upper))
  (cond ((< lower upper)
         (+ lower (random (- upper lower))))
        ((= lower upper)
         lower)
        ((> lower upper)
         (random-in-range upper lower))))

(behavior 'random-in-range
  (should= 5 (random-in-range 5 5))
  (dotimes (i 30)
    (let ((r (random-in-range 0 10)))
      (should-be-true (<= 0 r))
      (should-be-true (< r 10))))
  (dotimes (i 20)
    (let ((r (random-in-range 10 0))) ; arguments swapped
      (should-be-true (<= 0 r))
      (should-be-true (< r 10)))))


(defun random-in-ranges (&rest ranges)
  "This function, given many restricting ranges all as two-element lists, will
return a random number in the range that is a common subset to all of them."
  (let ((lower (minimum ranges :key #'minimum))
        (upper (maximum ranges :key #'maximum)))
    (random-in-range lower upper)))

(behavior 'random-in-ranges
  (dotimes (i 20)
    (let ((r (random-in-ranges '(0 10) '(2 8))))
      (should-be-true (<= 0 r))
      (should-be-true (< r 10)))))


(defun random-range (lower upper &key (containing nil))
  "Return a two-element list (LO HI) of random bounds within [LOWER, UPPER).
Without CONTAINING, LO and HI are two independent samples ordered so LO <= HI.
With CONTAINING (a number or sequence), LO is chosen in [LOWER, low-max) and
HI in [high-min, UPPER) so the resulting range covers CONTAINING."
  (when (null containing)
    (let ((a (random-in-range lower upper))
          (b (random-in-range lower upper)))
      (return-from random-range
                   (list (min a b)
                         (max a b)))))
  (let ((low-max (if (sequence? containing)
                   (minimum containing)
                   containing))
        (high-min (if (sequence? containing)
                    (maximum containing)
                    containing)))
    (list (random-in-range lower low-max)
          (random-in-range high-min upper))))

(behavior 'random-range
  (dotimes (i 20)
    (destructuring-bind (lo hi) (random-range 0 100)
      (should-be-true (<= lo hi))
      (should-be-true (<= 0 lo))
      (should-be-true (< hi 100))))
  (dotimes (i 20)
    (destructuring-bind (lo hi) (random-range 0 100 :containing 50)
      (should-be-true (<= lo 50))
      (should-be-true (<= 50 hi)))))


(defun randomize-array (array argument-for-random)
  "This function randomizes the contents of the array."
  (assert (arrayp array))
  (dotimes (index (array-total-size array) array)
    (setf (row-major-aref array index)
          (random argument-for-random))))


(defun random-array (dimensions argument-for-random)
  "This function returns a new array with randomized contents."
  (randomize-array (make-array dimensions) argument-for-random))

(behavior 'random-array
  (let ((a (random-array '(2 3) 10)))
    (should-be-true (arrayp a))
    (should-equal (array-dimensions a) '(2 3))
    (dotimes (i (array-total-size a))
      (let ((v (row-major-aref a i)))
        (should-be-true (<= 0 v))
        (should-be-true (< v 10))))))


(defgeneric shuffle (container)
  (:documentation
   "Return a new container with the same elements as CONTAINER in random
order.  The original CONTAINER is not modified; see NSHUFFLE to shuffle in
place."))


(defmethod shuffle ((array array))
  "This randomly shuffles the contents of an array.
Uses Fisher–Yates: for an array of size 0 or 1 the result is a copy of the
input; larger arrays are shuffled in linear time."
  (let* ((result (duplicate array))
         (n (array-total-size result)))
    (loop for j from (1- n) downto 1
          for k = (random (1+ j)) ; 0 <= k <= j
          do (swap (row-major-aref result j)
                   (row-major-aref result k)))
    result))


(defmethod shuffle ((list list))
  "This randomly shuffles a list."
  (vector-to-list (shuffle (list-to-vector list))))

(behavior 'shuffle
  (should-be-null (shuffle nil))
  (should-equal '(1) (shuffle '(1)))
  (let* ((original '(1 2 3 4 5 6 7 8))
         (copy (copy-list original))
         (shuffled (shuffle original)))
    (should-equal original copy) ; non-destructive
    (should-equal (sort (copy-list shuffled) #'<) original))
  (let* ((original #(a b c d))
         (shuffled (shuffle original)))
    (should-equalp original #(a b c d))
    (should-equal (sort (coerce shuffled 'list) #'string< :key #'symbol-name)
                  '(a b c d))))


(defmacro nshuffle (argument)
  "This randomly shuffles the argument in place."
  `(setf ,argument (shuffle ,argument)))

(behavior 'nshuffle
  (let ((lst '(1 2 3 4 5)))
    (nshuffle lst)
    (should-equal (sort (copy-list lst) #'<) '(1 2 3 4 5))))

(behavior 'gauss
  (dotimes (i 10)
    (should-be-a 'float (gauss 0.0 1.0)))
  ;; With zero variance the result is the mean (generator still runs).
  (should= 3.0 (gauss 3.0 0.0)))
