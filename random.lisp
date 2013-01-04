;;;; Copyright (C) 2005 -- 2013, Christopher Mark Gore, All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
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


(defpackage :cgore-random
  (:nicknames :random)
  (:use :common-lisp
        :cgore-control)
  (:export
    :coin-toss
    :gauss
    :nshuffle
    :random-argument
    :random-array
    :random-element
    :random-in-range
    :random-in-ranges
    :random-range
    :randomize-array
    :shuffle
    ))
(in-package :cgore-random)


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


(defgeneric random-element (sequence))


(defmethod random-element ((list list))
  "This method returns a random element from a list."
  (when list
    (nth (random (length list)) list)))


(defmethod random-element ((array array))
  "This method returns a random element from an array."
  (when (plusp (array-total-size array))
    (row-major-aref array (random (array-total-size array)))))


(defun random-argument (&rest rest)
  (random-element rest))


(defun coin-toss ()
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


(defun random-in-ranges (&rest ranges)
  "This function, given many restricting ranges all as two-element lists, will
return a random number in the range that is a common subset to all of them."
  (let ((lower (minimum ranges :key #'minimum))
        (upper (maximum ranges :key #'maximum)))
    (random-in-range lower upper)))


(defun random-range (lower upper &key (containing nil))
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


(defun randomize-array (array argument-for-random)
  "This function randomizes the contents of the array."
  (assert (arrayp array))
  (dotimes (index (array-total-size array) array)
    (setf (row-major-aref array index)
          (random argument-for-random))))


(defun random-array (dimensions argument-for-random)
  "This function returns a new array with randomized contents."
  (randomize-array (make-array dimensions) argument-for-random))


(defgeneric shuffle (container))


(defmethod shuffle ((array array))
  "This randomly shuffles the contents of an array."
  (let ((result (duplicate array)))
    (do* ((j (1- (array-total-size result)) (1- j))
          (k (random-in-range 1 (1+ j)) (random-in-range 1 (1+ j))))
      ((minusp j) result)
      (swap (row-major-aref result j)
            (row-major-aref result k)))))


(defmethod shuffle ((list list))
  "This randomly shuffles a list."
  (vector-to-list (shuffle (list-to-vector list))))


(defmacro nshuffle (argument)
  "This randomly shuffles the argument in place."
  `(setf ,argument (shuffle ,argument)))
