;;;; Copyright (C) 2005 -- 2013, Christopher Mark Gore,
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


(defpackage :sigma/sequence
  (:use :common-lisp
        :sigma/control)
  (:export :arefable?
           :array-values
           :best
           :empty-sequence?
           :join-symbol-to-all-preceeding
           :join-symbol-to-all-following
           :list-to-vector
           :maximum
           :maximum?
           :minimum
           :minimum?
           :nconcf
           :nthable?
           :nth-from-end
           :sequence?
           :set-equal
           :set-nthcdr
           :simple-vector-to-list
           :slice
           :sort-on
           :sort-order
           :split
           :the-last
           :vector-to-list
           :worst))
(in-package :sigma/sequence)


(defun nth-from-end (n list)
  "This macro is similar to NTH, but counting from the back."
  (assert (integerp n))
  (assert (<= 0 n))
  (assert (listp list))
  (maplist #'(lambda (a b)
               (when (null (rest b))
                 (return-from nth-from-end (first a))))
           list (nthcdr n list)))

(let ((0-to-10 '(0 1 2 3 4 5 6 7 8 9 10)))
  (assert (equal (nth-from-end 0 0-to-10)
                 10))
  (assert (equal (nth-from-end 3 0-to-10)
                 7))
  (assert (equal (nth-from-end 10 0-to-10)
                 0))
  (assert (equal (nth-from-end 11 0-to-10)
                 nil)))


(defmacro set-nthcdr (n list new-value)
  `(progn (assert (nonnegative-integer? ,n))
          (if (zerop ,n)
            (setf ,list ,new-value)
            (setf (cdr (nthcdr (1- ,n) ,list)) ,new-value))))

#+cmu (defsetf nthcdr set-nthcdr)
#+sbcl (sb-ext:without-package-locks (defsetf nthcdr set-nthcdr))
#+clisp (ext:without-package-lock () (defsetf nthcdr set-nthcdr))


(defun sequence? (sequence)
  (typep sequence 'sequence))


(defun empty-sequence? (sequence)
  (and (sequence? sequence)
       (or (null sequence)
           (and (arrayp sequence)
                (some #'zerop (array-dimensions sequence))))))


(defmacro nconcf (list-1 list-2)
  `(setf ,list-1 (nconc ,list-1 ,list-2)))


(defun the-last (list)
  (assert (listp list))
  (car (last list)))

(defun list-to-vector (list)
  "This takes in a list and returns an equivalent vector."
  (assert (listp list))
  (coerce list 'vector))

(defun vector-to-list (vector)
  "This takes in a vector and returns an equivalent list."
  (assert (vectorp vector))
  (coerce vector 'list))

(defun simple-vector-to-list (vector)
  "This takes in a vector and returns an equivalent list."
  (assert (vectorp vector))
  (loop for index from 0 to (1- (length vector))
        collect (svref vector index)))


(defgeneric minimum (sequence &key key start end))


(defmethod minimum ((sequence sequence)
                    &key (key #'identity) (start 0) (end nil))
  "This reduces MIN onto the sequence provided."
  (reduce #'min sequence :key key :start start :end end))


(defgeneric maximum (sequence &key key start end))


(defmethod maximum ((sequence sequence)
                    &key (key #'identity) (start 0) (end nil))
  "This reduces MAX onto the sequence provided."
  (reduce #'max sequence :key key :start start :end end))


(defgeneric minimum? (sequence &key position key start end))


(defmethod minimum? ((sequence sequence)
                      &key (position nil) (key #'identity) (start 0) (end nil))
  (when (null position)
    (setf position (1- (length sequence))))
  (<= (funcall key (elt sequence position))
      (minimum sequence :key key :start start :end end)))


(defgeneric maximum? (sequence &key position key start end))


(defmethod maximum? ((sequence sequence)
                     &key (position nil) (key #'identity) (start 0) (end nil))
  (when (null position)
    (setf position (1- (length sequence))))
  (>= (funcall key (elt sequence position))
      (maximum sequence :key key :start start :end end)))


(defgeneric best (sequence predicate &key key))


(defmethod best ((list list) predicate &key (key #'identity))
  "This returns the ``best'' element in a list.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the first element after sorting the
sequence with the same predicate and key."
  (when (null list)
    (return-from best nil))
  (let ((best (first list)))
    (dolist (i list best)
      (when (funcall predicate
                     (funcall key i)
                     (funcall key best))
        (setf best i)))
    best))


(defmethod best ((vector vector) predicate &key (key #'identity))
  "This returns the ``best'' element in a vector.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the first element after sorting the
sequence with the same predicate and key."
  (when (zerop (length vector))
    (return-from best nil))
  (let ((best (aref vector 0)))
    (dotimes (i (length vector) best)
      (when (funcall predicate
                     (funcall key (aref vector i))
                     (funcall key best))
        (setf best (aref vector i))))
    best))


(defgeneric worst (sequence predicate &key key))


(defmethod worst ((list list) predicate &key (key #'identity))
  "This returns the ``worst'' element in a list.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the last element after sorting the
sequence with the same predicate and key."
  (when (null list)
    (return-from worst nil))
  (let ((worst (first list)))
    (dolist (i list worst)
      (when (funcall predicate
                     (funcall key worst)
                     (funcall key i))
        (setf worst i)))
    worst))


(defmethod worst ((vector vector) predicate &key (key #'identity))
  "This returns the ``worst'' element in a vector.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the last element after sorting the
sequence with the same predicate and key."
  (when (zerop (length vector))
    (return-from worst nil))
  (let ((worst (aref vector 0)))
    (dotimes (i (length vector) worst)
      (when (funcall predicate
                     (funcall key worst)
                     (funcall key (aref vector i)))
        (setf worst (aref vector i))))
    worst))


(defun nthable? (n list)
  (and (listp list)
       (typep n `(integer 0 ,(1- (length list))))))


(defun arefable? (array position)
  (and (arrayp array)
       (listp position)
       (= (length (array-dimensions array))
          (length position))
       (every #'(lambda (position dimension)
                  (typep position `(integer 0 ,(1- dimension))))
              position
              (array-dimensions array))))


(defgeneric sort-on (sequence-to-sort ordering-sequence predicate &key key))


(defmethod sort-on ((sequence-to-sort list)
                    (ordering-sequence list)
                    predicate
                    &key
                    (key #'identity))
  "This function sorts the sequence-to-sort based upon the ordering-sequence."
  (assert (listp sequence-to-sort))
  (assert (listp ordering-sequence))
  (assert (<= (length sequence-to-sort)
              (length ordering-sequence)))
  (mapcar #'cdr (sort (mapcar #'cons ordering-sequence sequence-to-sort)
                      predicate
                      :key (compose key #'car))))


(defmethod sort-on ((sequence-to-sort vector)
                    (ordering-sequence list)
                    predicate
                    &key (key #'identity))
  (list-to-vector (sort-on (vector-to-list sequence-to-sort)
                           ordering-sequence
                           predicate
                           :key key)))


(defmethod sort-on (sequence-to-sort
                    (ordering-sequence vector)
                    predicate
                    &key (key #'identity))
  (sort-on sequence-to-sort
           (vector-to-list ordering-sequence)
           predicate
           :key key))


(defun sort-order (sequence predicate &key (key #'identity))
  "This function returns the indices in the order for the sorted sequence."
  (sort-on (integer-range (1- (length sequence)))
           sequence
           predicate
           :key key))


(defgeneric split (sequence separators &key key test remove-separators?))


(defmethod split ((list list)
                   separators
                   &key
                   (key #'identity)
                   (test #'eql)
                   (remove-separators? t))
  "This splits LIST on the SEPERATORS, returning a list of all the fields.
The optional KEY and TEST arguments are for the comparison of items in the
SEQUENCE for membership in the SEPERATORS."
  (assert (not (null list)))
  (assert (not (null separators)))
  (unless (listp separators)
    (setf separators (list separators)))
  (let ((result nil)
        (current-list nil))
    (mapc #'(lambda (item)
              (if (member item separators :key key :test test)
                (progn (unless remove-separators?
                         (push item current-list))
                       (push (reverse current-list) result)
                       (setf current-list nil))
                (push item current-list)))
          list)
    (push (reverse current-list) result)
    (reverse result)))


(defgeneric slice (sequence &optional slice))


(defmethod slice ((vector vector) &optional (slice 1))
  "This method returns a slice from a one-dimensional vector; that is, a modular
subset of the vector.  For example,
> (slice #(1 2 3 4 5 6 7 8 9) 2)
=> #(1 3 5 7 9)
The slice argument may be any positive rational number."
  (assert (and (rationalp slice)
               (plusp slice)))
  (let ((index 0)
        (result nil))
    (while (< index (length vector))
      (when (integerp index)
        (push (svref vector index) result))
      (incf index slice))
    (make-array (list (length result))
                :initial-contents (reverse result))))


(defmethod slice ((list list) &optional (slice 1))
  "This method returns a slice from a one-dimensional list; that is, a modular
subset of the list.  For example,
> (slice '(1 2 3 4 5 6 7 8 9) 2)
=> '(1 3 5 7 9)
The slice argument may be any positive rational number."
  (assert (and (rationalp slice)
               (plusp slice)))
  (let ((index 0)
        (vector (list-to-vector list))
        (result nil))
    (while (< index (length vector))
      (when (integerp index)
        (push (svref vector index) result))
      (incf index slice))
    (reverse result)))


(defun join-symbol-to-all-preceeding (symbol list)
  "This function takes a symbol and a list, and for every occurance of the
symbol in the list, it joins it to the item preceeding it.  For example:

> (join-symbol-to-all-preceeding :% '(10 :% 20 :% 30 :%))
=> '(:10% :20% :30%)

The result is affected by all of the *PRINT-...* variables in the same was as
the FORMAT builtin function."
  (assert (symbolp symbol))
  (assert (listp list))
  (aif (position symbol list)
    ;;; There is at least one instance of the symbol in the list.  We will
    ;;; therefore remove it and modify the previous item.
    (progn
      (assert (<= 1 it))
      (let ((previous (nth (1- it) list)))
        (setf (nth (1- it) list)
              (intern (format nil "~A~A" previous symbol) "KEYWORD"))
        ;; Recursively apply the modification to the entire list.
        (join-symbol-to-all-preceeding symbol (remove symbol list :count 1))))
    ;; Otherwise, we have no instances of the specified symbol in the list.
    ;; Just return the list passed in unmodified.
    list))

(assert (equal (join-symbol-to-all-preceeding :% '(100 :%))
               '(:100%)))
(let ((*print-base* 8))
  (assert (equal (join-symbol-to-all-preceeding :% '(64 :%))
                 '(:100%))))
(assert (equal (join-symbol-to-all-preceeding :% '(10 :% 20 :% 30 :%))
               '(:10% :20% :30%)))
(assert (equal (join-symbol-to-all-preceeding :% '(10 :55%))
               '(10 :55%)))
(assert (equal (join-symbol-to-all-preceeding :% '(1 2 3 4 5))
               '(1 2 3 4 5)))
(assert (equal (join-symbol-to-all-preceeding :% '(:a :b :c :d :e))
               '(:a :b :c :d :e)))
(assert (equal (join-symbol-to-all-preceeding :foo '(:bar :foo :baz :foo))
               '(:barfoo :bazfoo)))
(let ((*print-case* :downcase))
  (assert (equal (join-symbol-to-all-preceeding :foo '(:bar :foo :baz :foo))
                 '(:|barfoo| :|bazfoo|))))


(defun join-symbol-to-all-following (symbol list)
  "This function takes a symbol and a list, and for every occurance of the
symbol in the list, it joins it to the item following it.  For example:

> (join-symbol-to-all-following :# '(:# 10 :# 20 :# 30))
=> '(:#10 :#20 :#30)

The result is affected by all of the *PRINT-...* variables in the same was as
the FORMAT builtin function."
  (assert (symbolp symbol))
  (assert (listp list))
  (aif (position symbol list)
    ;;; There is at least one instance of the symbol in the list.  We will
    ;;; therefore remove it and modify the previous item.
    (progn
      (assert (< it (length list)))
      (let ((next (nth (1+ it) list)))
        (setf (nth (1+ it) list)
              (intern (format nil "~A~A" symbol next) "KEYWORD"))
        ;; Recursively apply the modification to the entire list.
        (join-symbol-to-all-following symbol (remove symbol list :count 1))))
    ;; Otherwise, we have no instances of the specified symbol in the list.
    ;; Just return the list passed in unmodified.
    list))

(assert (equal (join-symbol-to-all-following :# '(:# :aabbcc))
               '(:#aabbcc)))
(assert (equal (join-symbol-to-all-following :# '(:# 10 :# 20 :# 30))
               '(:#10 :#20 :#30)))
(let ((*print-base* 8))
  (assert (equal (join-symbol-to-all-following :# '(:# 64))
                 '(:#100))))
(assert (equal (join-symbol-to-all-following :# '(:#55 10))
               '(:#55 10)))
(assert (equal (join-symbol-to-all-following :# '(1 2 3 4 5))
               '(1 2 3 4 5)))
(assert (equal (join-symbol-to-all-following :# '(:a :b :c :d :e))
               '(:a :b :c :d :e)))
(assert (equal (join-symbol-to-all-following :foo '(:foo bar :foo :baz))
               '(:foobar :foobaz)))


(defun set-equal (list-1 list-2 &key (key #'identity) test test-not)
  (assert (listp list-1))
  (assert (listp list-2))
  (assert (not (and test test-not)))
  (cond (test (and (not (set-difference list-1 list-2 :key key :test test))
                   (not (set-difference list-2 list-1 :key key :test test))))
        (test-not  (and (not (set-difference list-1 list-2
                                             :key key :test-not test-not))
                        (not (set-difference list-2 list-1
                                             :key key :test-not test-not))))
        (t (and (not (set-difference list-1 list-2 :key key))
                (not (set-difference list-2 list-1 :key key))))))



(defun array-values (array positions)
  "This function returns a list of the values in array found at the specified
positions."
  (assert (arrayp array))
  (assert (listp positions))
  (mapcar #'(lambda (position)
              (assert (and (listp position)
                           (= (length position)
                              (length (array-dimensions array)))))
              (apply #'aref array position))
          positions))
