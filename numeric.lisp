;;;; Copyright (c) 2005 -- 2012, Christopher Mark Gore,
;;;; All rights reserved.
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


(defpackage :cgore-numeric
  (:nicknames :numeric)
  (:use :common-lisp
        :cgore-design-pattern)
  (:export
    :divf
    :fractional-part
    :fractional-value
    :multf
    :nonnegative?
    :nonnegative-float
    :nonnegative-integer
    :nonnegative-integer?
    :positive-float
    :positive-integer
    :positive-integer?
    :product
    :sum
    :unsigned-integer?
    ))
(in-package :cgore-numeric)


(defmacro divf (variable &rest divisors)
  `(opf #'/ ,variable ,@divisors))


(defun fractional-part (number)
  "This is the fractional part formula most familiar to computer scientists.
It possesses the useful feature that frac(x)+int(x)=x, but may be negative."
  (assert (numberp number))
  (if (minusp number)
    (- number (floor number) 1)
    (- number (floor number))))


(defun fractional-value (number)
  "This is the fractional value formula most familiar to most mathematicians.
Note that the result of this is always positive, forming a sawtooth."
  (assert (numberp number))
  (- number (floor number)))


(defmacro multf (variable &rest multiplicands)
  `(opf #'* ,variable ,@multiplicands))


(defun nonnegative? (x)
  (not (minusp x)))


(deftype nonnegative-float ()
  '(float 0.0 *))


(deftype nonnegative-integer ()
  '(integer 0 *))


(defun nonnegative-integer? (nonnegative-integer)
  (typep nonnegative-integer 'nonnegative-integer))


(deftype positive-float ()
  '(float (0.0) *))


(deftype positive-integer ()
  '(integer (0) *))


(defun positive-integer? (positive-integer)
  (typep positive-integer 'positive-integer))


(defun product (sequence &key (key 'identity) (start 0) (end nil))
  (assert (sequence? sequence))
  (reduce #'* sequence :key key :start start :end end :initial-value 1))


(defun sum (sequence &key (key 'identity) (start 0) (end nil))
  (assert (sequence? sequence))
  (reduce #'+ sequence :key key :start start :end end :initial-value 0))


(defun unsigned-integer? (x)
  (and (integerp x)
       (not (minusp x))))