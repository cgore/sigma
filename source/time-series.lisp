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


(defpackage :sigma/time-series
  (:use :common-lisp
        :sigma/control
        :sigma/numeric
        :sigma/sequence)
  (:export :array-raster-line
           :distance
           :next-point
           :norm
           :raster-line
           :similar-points?
           :snap-index
           :time-multiseries
           :time-multiseries?
           :time-series?
           :tms-dimensions
           :tmsref
           :tms-values))
(in-package :sigma/time-series)


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


(defmacro snap-index (index bound)
  "This wraps the value of index between 0 and bound."
  `(progn
     (when (< ,index 0)
       (setf ,index (+ ,index ,bound)))
     (when (>= ,index ,bound)
       (setf ,index (- ,index ,bound)))))


(defun similar-points? (p q &optional (coordinate-assertion #'numberp))
  "This predicate determines if the points P and Q are similar."
  (and (listp p)
       (listp q)
       (= (length p) (length q))
       (every coordinate-assertion p)
       (every coordinate-assertion q)))


(defun raster-line (start-point
                     end-point
                     &key
                     (coordinate-assertion #'integerp)
                     (from-start 0)
                     (from-end 0))
  "This is derived from the algorithm for raster conversion of a 3D line as
found in ``3D Scan-Conversion Algorithms for Voxel-Based Graphics'' by
Arie Kaufman and Eyal Shimony, 1986 Workshop on Interactive 3D Graphics.
Here it should work for any any n-dimensional space where n is non-negative."
  (assert (similar-points? start-point end-point coordinate-assertion))
  (assert (integerp from-end))
  (when (equalp start-point end-point)
    (return-from raster-line start-point))
  ;;; We perform the only logical thing in the case of the two points being
  ;;; equal, that is to view it as a degenerate condition and return a line
  ;;; consisting of only that point.
  (let* ((dimensions (length start-point))
         (deltas (mapcar (compose #'abs #'-) start-point end-point))
         ;; We need to work on the coordinates such that
         ;; delta x >= delta y >= delta z >= ... >= 0.
         ;; NB: this mapping is reflexive.
         (coordinate-order (sort-order deltas #'>))
         ;; This is the point we start at, ordered.
         (from-point (mapcar (rcurry #'nth start-point) coordinate-order))
         ;; This is the point we end at, ordered.
         (to-point (mapcar (rcurry #'nth end-point) coordinate-order))
         ;; We want x2 > x1, and swap the points if necessary.
         (swap-points? (let ((swap-points? (< (first to-point)
                                              (first from-point))))
                         (when swap-points?
                           (swap to-point from-point))
                         swap-points?))
         ;; This is the point we are currently at in the loop.
         (current-point from-point)
         ;; We recalculate the deltas because that is O(n), but the lookup as
         ;; we used for from-point and to-point is O(n^2).
         (deltas (mapcar (compose #'abs #'-) from-point to-point))
         ;; The basic length of the raster line is the number of points that
         ;; would exist in the raster line without any FROM-START or FROM-END
         ;; arguments used by the algorithm.
         (basic-length (apply #'max deltas))
         ;; These are all in '(+1 0 -1), and they are the direction that the
         ;; line is travelling in for that particular dimension.
         (signums (mapcar (compose #'signum #'-) to-point from-point))
         ;; We don't use the decision variable for the first coordinate, but we
         ;; calculate it anyway to ease variable addressing later on.
         (deciders (mapcar #'(lambda (delta)
                               (- (* 2 delta)
                                  (first deltas)))
                           deltas))
         ;;; These two are the two increments for the deciders.
         (if-increments (mapcar (curry #'* 2) deltas))
         (else-increments (mapcar #'(lambda (delta)
                                      (* 2 (- delta (first deltas))))
                                  deltas))
         ;; We now update CURRENT-POINT so that it accurately reflects any
         ;; offset from the FROM-START argument.
         (current-point (mapcar #'(lambda (x delta signum)
                                    (round (+ x (* signum
                                                   (/ delta basic-length)
                                                   from-start))))
                                current-point deltas signums))
         (result (list (duplicate current-point))))
    (when (equal start-point end-point)
      (return-from raster-line (list start-point)))
    (when (not (plusp (+ basic-length (- from-start) from-end)))
      (return-from raster-line nil))
    (while (< (first current-point)
              (+ from-end (first to-point)))
      (incf (first current-point))
      (loop for i from 1 to (1- dimensions) do
            (if (minusp (nth i deciders))
              ;; If no change in the current coordinate.
              (incf (nth i deciders)
                    (nth i if-increments))
              ;; If a change in the current coordinate.
              (progn (incf (nth i deciders)
                           (nth i else-increments))
                     (incf (nth i current-point)
                           (nth i signums)))))
      (push (duplicate current-point) result))
    (map-into result
              #'(lambda (point)
                  (mapcar (rcurry #'nth point)
                          (sort-order coordinate-order #'<)))
              result)
    ;;; We need to reverse the list of points before returning from this
    ;;; function unless we reversed the end points originally.
    (unless swap-points?
      (opf #'reverse result))
  result))


(defun norm (sequence &optional (power 2))
  "This function returns the mathematical vector norm of a sequence.  For the
infinity norm, use :INFINITY for the power."
  (cond ((equalp power :infinity)
         (apply #'max sequence))
        ((numberp power)
         (expt (sum sequence :key (rcurry #'expt power))
               (/ power)))
        ;; We don't currently understand any other sort of norm.
        (t nil)))

(defun distance (initial-point final-point &optional (power 2))
  "This calculates the distance between two points."
  (norm (mapcar #'- initial-point final-point) power))


(defun array-raster-line (array
                           start-point
                           end-point
                           &key
                           (coordinate-assertion #'positive-integer?)
                           (from-start 0)
                           (from-end 0))
  "This function returns a one-dimensional list of values from the array which
  starts at the start-point and ends at the end-point, as taken from the
  straight-line path between the two points.  The array may be of any rank."
  (array-values array
                (raster-line start-point end-point
                             :coordinate-assertion coordinate-assertion
                             :from-start from-start
                             :from-end from-end)))


(defun tms-raster-line (time-multiseries
                         start-point
                         end-point
                         &key
                         (coordinate-assertion #'positive-integer?)
                         (from-start 0)
                         (from-end 0))
  "This function returns a one-dimensional list of values from a time
  multiseries which starts at the start-point and ends at the end-point, as
  taken from the straight-line path between the two points.  The time
  multiseries may be of any rank."
  (tms-values time-multiseries
              (raster-line start-point
                           end-point
                           :coordinate-assertion coordinate-assertion
                           :from-start from-start
                           :from-end from-end)))
