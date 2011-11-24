;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore,
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

(unless (find-package 'utilities) (load "utilities"))

(unless (find-package 'random)
  (defpackage :random
    (:use :common-lisp :utilities)
    (:export :gauss)))
(in-package :random)

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
