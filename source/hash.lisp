;;;; Copyright (c) 2005 -- 2014, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
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

(defpackage :sigma/hash
  (:use :common-lisp
        :sigma/behave
        :sigma/control)
  (:export :sethash
           :populate-hash-table
           :inchash
           :dechash
           :gethash-in))
(in-package :sigma/hash)

(defmacro sethash (key hash-table value)
  "The SETHASH macro is a shortcut for SETF GETHASH."
  `(setf (gethash ,key ,hash-table) ,value))

(defun populate-hash-table (&rest pairs)
  "The POPULATE-HASH-TABLE function makes initial construction of hash tables a
lot easier, just taking in key/value pairs as the arguments to the function, and
returning a newly-constructed hash table."
  (let ((result (make-hash-table)))
    (a?while key (pop pairs)
      (sethash key result (pop pairs)))
    result))

(behavior 'populate-hash-table
  (spec "makes hash tables"
    (let ((h (populate-hash-table 'a 1
                                  'b 2
                                  'c 3)))
      (should= (gethash 'a h) 1)
      (should= (gethash 'b h) 2)
      (should= (gethash 'c h) 3)))
  (spec "handles lists as values"
    (let ((v (populate-hash-table 'name "Valentinus"
                                  'likes '(birds roses)
                                  'dislikes '(beheadings epilepsy "false idols")
                                  'died 269)))
      (should= (gethash 'died v) 269)
      (should-string-equal (gethash 'name v) "Valentinus")
      (should-equal (gethash 'likes v) '(birds roses)))))

(defun inchash (key hash-table)
  "The INCHASH function will increment the value in key of the hash,
initializing it to 1 if it isn't currently defined."
  (assert (typep hash-table 'hash-table))
  (if (null (gethash key hash-table))
      (setf (gethash key hash-table) 1)
      (incf (gethash key hash-table))))

(defun dechash (key hash-table)
  "The DECHASH function will decrement the value in key of the hash,
initializing it to -1 if it isn't currently defined."
  (assert (typep hash-table 'hash-table))
  (if (null (gethash key hash-table))
      (setf (gethash key hash-table) -1)
      (decf (gethash key hash-table))))

(defun gethash-in (keys hash-table &optional (default nil))
  "The GETHASH-IN function works like gethash, but allows for multiple keys to
be specified at once, to work with nested hash tables."
  (if (listp keys)
      (multiple-value-bind (value present?)
          (gethash (car keys) hash-table default)
        (if present?
            (if (cadr keys)
                (gethash-in (cdr keys) value default)
                (values value t))
            (values default false)))
      (gethash keys hash-table default)))

(behavior 'gethash-in
  (spec "works for a single-deep hash"
    (let ((h (make-hash-table)))
      (sethash 'a h 12)
      (should= (gethash-in '(a) h) 12)))
  (spec "works for the degenerate case of just gethash"
    (let ((h (make-hash-table)))
      (sethash 'a h 34)
      (should= (gethash-in 'a h) 34)))
  (spec "works for a two-deep hash"
    (let ((h (make-hash-table))
          (i (make-hash-table)))
      (sethash 'b i 123)
      (sethash 'a h i)
      (should= (gethash-in '(a b) h 123)))))
