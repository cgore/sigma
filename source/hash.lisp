;;;; Copyright (c) 2005 -- 2021, Christopher Mark Gore,
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

(defpackage :sigma/hash
  (:use :common-lisp
        :sigma/behave
        :sigma/control)
  (:export :sethash
           :populate-hash-table
           :inchash
           :dechash
           :gethash-in
           :make-partition
           :set-partition
           :populate-partition))
(in-package :sigma/hash)

(defmacro sethash (key hash-table value)
  "The SETHASH macro is a shortcut for SETF GETHASH."
  `(setf (gethash ,key ,hash-table) ,value))

(defun populate-hash-table (&rest pairs)
  "The POPULATE-HASH-TABLE function makes initial construction of hash tables a
lot easier, just taking in key/value pairs as the arguments to the function, and
returning a newly-constructed hash table."
  (let ((result (make-hash-table :test 'equal)))
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
  (spec "works with string keys"
    (let ((h (populate-hash-table "a" 1
                                  "b" 2
                                  "c" 3)))
      (should= (gethash "a" h) 1)
      (should= (gethash "b" h) 2)
      (should= (gethash "c" h) 3)))
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
            (values default nil)))
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

(defun make-partition ()
  "The MAKE-PARTITION function is a variant of MAKE-HASH-TABLE that assumes
you're going to use the hash table for partitions with multiple entries per
key, not a one-to-one hashmap."
  (make-hash-table :test 'equal))

(defmacro set-partition (key hash-table value)
  "The SET-PARTITION macro is a variant of SETHASH that assumes the hash entries
are all sequences, allowing for multiple results per key.  When you call GETHASH
on the hashmap you will get back a sequence of all the entries for that key."
  `(sethash ,key ,hash-table
            (if (gethash ,key ,hash-table)
                (cons ,value (gethash ,key ,hash-table))
                (list ,value))))

(behavior 'set-partition
  (spec "simple example"
    (let ((p (make-partition)))
      (set-partition 'a p 1)
      (set-partition 'b p 2)
      (set-partition 'c p 3)
      (set-partition 'a p 4)
      (should-equal (gethash 'a p) '(4 1))
      (should-equal (gethash 'b p) '(2))
      (should-equal (gethash 'c p) '(3))
      (should-equal (gethash 'd p) nil)))
  (spec "works with lists"
    (let ((p (make-partition)))
      (set-partition 'a p '(1 2 3))
      (set-partition 'a p 4)
      (set-partition 'a p '(5 6 7))
      (set-partition 'b p 8)
      (set-partition 'c p 9)
      (should-equal (gethash 'a p) '((5 6 7) 4 (1 2 3)))
      (should-equal (gethash 'b p) '(8))
      (should-equal (gethash 'c p) '(9)))))

(defun populate-partition (&rest pairs)
  "The POPULATE-PARTITION function make initial construction of a partition a
lot easier, just taking in key/value pairs as the arguments to the function,
where there can be multiple entries for any key, and returning a
newly-constructed partition."
  (let ((result (make-partition)))
    (a?while key (pop pairs)
      (set-partition key result (pop pairs)))
    result))

(behavior 'populate-partition
  (spec "simple example"
    (let ((p (populate-partition 'a 1
                                 'b 2
                                 'c 3
                                 'a 4)))
      (should-equal (gethash 'a p) '(4 1))
      (should-equal (gethash 'b p) '(2))
      (should-equal (gethash 'c p) '(3))
      (should-equal (gethash 'd p) nil)))
  (spec "works with lists"
    (let ((p (populate-partition 'a '(1 2 3)
                                 'a 4
                                 'a '(5 6 7)
                                 'b 8
                                 'c 9)))
      (should-equal (gethash 'a p) '((5 6 7) 4 (1 2 3)))
      (should-equal (gethash 'b p) '(8))
      (should-equal (gethash 'c p) '(9)))))
