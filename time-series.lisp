;;;; Copyright (C) 2005 -- 2012, Christopher Mark Gore,
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


(defpackage :cgore-time-series
  (:nicknames :time-series)
  (:use
    :common-lisp
    :cgore-design-pattern
    :cgore-numeric
    :cgore-sequence)
  (:export
    :time-multiseries
    :time-multiseries?
    :time-series?
    :tms-dimensions
    :tmsref
    :tms-values
    ))
(in-package :cgore-time-series)


(defun time-series? (time-series &optional (element-type t))
  "The TIME-SERIES? predicate returns true if the argument could be a time
series."
  (and (listp time-series)
       (not (null time-series))
       (every (rcurry #'typep element-type) time-series)))


(defun time-multiseries? (time-multiseries)
  "This predicate returns true if the argument is a time multiseries
(multivariate time series), which we represent as a list of arrays of equal
dimensions, where each array represents data from a single time step."
  (and (listp time-multiseries)
       (not (null time-multiseries))
       (every #'arrayp time-multiseries)
       (let ((dimensions (array-dimensions (first time-multiseries))))
         (every #'(lambda (array)
                    (equalp dimensions (array-dimensions array)))
                time-multiseries))))


(deftype time-multiseries ()
  '(satisfies time-multiseries?))


(defun tmsref (time-multiseries time &rest position)
  "This function works like AREF, but for a time series or multiseries.  The
time multiseries is represented as a list of arrays, where there is an array
for each time step representing all of the data for that step in time.
A time series is represented as a list."
  (assert (or (time-multiseries? time-multiseries)
              (and (listp time-multiseries)
                   (null position))))
  (if (null position)
    ;; A (one-dimensional) time series.
    (nth time time-multiseries)
    ;; A (multi-dimensional) time multiseries.
    (apply #'aref (nth time time-multiseries) position)))


(defun tms-values (time-multiseries positions)
  "This function returns a list of the values in a time series or multiseries
at the specified positions.  A time multiseries is represented as a list of
arrays with identical dimensions, where each array represents a single time
step's entire data.  A time series is represented as a list.  The first value
in each position is the time position."
  (assert (listp positions))
  (setf positions (mapcar #'(lambda (position)
                              (if (listp position)
                                position
                                (list position)))
                          positions))
  (assert (or (and (time-multiseries? time-multiseries)
                   (every #'listp positions))
              (and (listp time-multiseries)
                   (every #'(lambda (position)
                              (or (nonnegative-integer? position)
                                  (and (listp position)
                                       (= 1 (length position))
                                       (nonnegative-integer?
                                         (first position)))))
                          positions))))
  (mapcar #'(lambda (position)
              (apply #'tmsref time-multiseries position))
          positions))


(defun tms-dimensions (time-multiseries)
  "This works like the ARRAY-DIMENSIONS function, but for a time multiseries.
  The first dimension listed is the time dimension."
  (assert (or (time-multiseries? time-multiseries)
              (listp time-multiseries)))
  (if (time-multiseries? time-multiseries)
    ;; A (multi-dimensional) time multi-series.
    (cons (length time-multiseries)
          (array-dimensions (first time-multiseries)))
    ;; A (one-dimensional) time series.
    (list (length time-multiseries))))
