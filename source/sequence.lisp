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

(defpackage :sigma/sequence
  (:use :common-lisp
        :sigma/behave
        :sigma/control)
  (:export :arefable?
           :array-values
           :best
           :empty-sequence?
           :join-symbol-to-all-preceeding
           :join-symbol-to-all-following
           :list-to-vector
           :max*
           :min*
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

(behavior 'nth-from-end
  (let ((0-to-10 '(0 1 2 3 4 5 6 7 8 9 10)))
    (should-equal (nth-from-end 0 0-to-10) 10)
    (should-equal (nth-from-end 3 0-to-10) 7)
    (should-equal (nth-from-end 10 0-to-10) 0)
    (should-be-null (nth-from-end 11 0-to-10))))

(defmacro set-nthcdr (n list new-value)
  "Set the Nth CDR of LIST to NEW-VALUE.  When N is 0, SETF LIST itself.
Used as the SETF expander for NTHCDR on implementations that allow it."
  `(progn (assert (and (integerp ,n)
                       (not (minusp ,n)))
                  (,n)
                  "The value ~S is not a nonnegative integer." ,n)
          (if (zerop ,n)
              (setf ,list ,new-value)
              (setf (cdr (nthcdr (1- ,n) ,list)) ,new-value))))

(behavior 'set-nthcdr
  ;; N must be a place-friendly form for ASSERT's restart list inside SET-NTHCDR.
  (let ((lst (list 1 2 3 4))
        (n 2))
    (set-nthcdr n lst (list 9 9))
    (should-equal lst '(1 2 9 9)))
  (let ((lst (list 1 2 3))
        (n 0))
    (set-nthcdr n lst (list 'a 'b))
    (should-equal lst '(a b))))

#+cmu (defsetf nthcdr set-nthcdr)
#+sbcl (sb-ext:without-package-locks (defsetf nthcdr set-nthcdr))
#+clisp (ext:without-package-lock () (defsetf nthcdr set-nthcdr))

(defun sequence? (sequence)
  "Return true if SEQUENCE is of type SEQUENCE (a list or vector)."
  (typep sequence 'sequence))

(defun empty-sequence? (sequence)
  "Return true if SEQUENCE is a sequence that contains no elements.  For
arrays, any dimension of size zero counts as empty."
  (and (sequence? sequence)
       (or (null sequence)
           (and (arrayp sequence)
                (some #'zerop (array-dimensions sequence))))))

(behavior 'sequence?
  (should-be-true (sequence? nil))
  (should-be-true (sequence? '(1 2)))
  (should-be-true (sequence? #(1 2)))
  (should-be-false (sequence? 5))
  (should-be-false (sequence? 'symbol)))

(behavior 'empty-sequence?
  (should-be-true (empty-sequence? nil))
  (should-be-true (empty-sequence? #()))
  (should-be-true (empty-sequence? (make-array 0)))
  ;; Multi-dimensional arrays are not SEQUENCEs, so this is false under the
  ;; current predicate (SEQUENCE? fails first).
  (should-be-false (empty-sequence? (make-array '(0 3))))
  (should-be-false (empty-sequence? '(1)))
  (should-be-false (empty-sequence? #(1)))
  (should-be-false (empty-sequence? 1)))

(defmacro nconcf (list-1 list-2)
  "Destructively concatenate LIST-2 onto LIST-1 and store the result back
into the place LIST-1, analogous to NCONC with assignment."
  `(setf ,list-1 (nconc ,list-1 ,list-2)))

(behavior 'nconcf
  ;; Use LIST (not quoted literals): NCONC is destructive and must not mutate
  ;; constants shared across the compiled file.
  (let ((a (list 1 2))
        (b (list 3 4)))
    (nconcf a b)
    (should-equal a '(1 2 3 4))))

(defun the-last (list)
  "Return the last element of LIST (the CAR of its LAST cons), or NIL if
LIST is empty."
  (assert (listp list))
  (car (last list)))

(behavior 'the-last
  (should-be-null (the-last nil))
  (should= 3 (the-last '(1 2 3))))

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

(behavior 'list-to-vector
  (should-equalp (list-to-vector '(1 2 3)) #(1 2 3))
  (should-equalp (list-to-vector nil) #()))

(behavior 'vector-to-list
  (should-equal (vector-to-list #(1 2 3)) '(1 2 3))
  (should-equal (vector-to-list #()) nil))

(behavior 'simple-vector-to-list
  (should-equal (simple-vector-to-list #(a b c)) '(a b c))
  (should-equal (simple-vector-to-list #()) nil)
  (should-equal (simple-vector-to-list (vector 1 2 3)) '(1 2 3)))

(defun max* (&rest lists)
  "The MAX* function is a shortcut for MAX. It takes in one or more lists and finds
the maximum value within all of them."
  (apply #'max (apply #'concatenate 'list lists)))

(behavior 'max*
  (should= 100 (max* '(1 2 3 4 5 100 6)))
  (should= 100 (max* '(1 2 3 4 5)
                     '(7 8 100)
                     '(10 11))))

(defun min* (&rest lists)
  "The MIN* function is a shortcut for MIN. It takes in one or more lists and finds
the minimum value within all of them."
  (apply #'min (apply #'concatenate 'list lists)))

(behavior 'min*
  (should= -100 (min* '(1 2 3 4 5 -100 6)))
  (should= -100 (min* '(1 2 3 4 5)
                      '(7 8 -100)
                      '(10 11))))

(defgeneric minimum (sequence &key key start end)
  (:documentation
   "Return the minimum element of SEQUENCE, optionally after applying KEY,
considering only the subsequence from START to END."))

(defmethod minimum ((sequence sequence)
                    &key (key #'identity) (start 0) (end nil))
  "This reduces MIN onto the sequence provided."
  (reduce #'min sequence :key key :start start :end end))

(defgeneric maximum (sequence &key key start end)
  (:documentation
   "Return the maximum element of SEQUENCE, optionally after applying KEY,
considering only the subsequence from START to END."))

(defmethod maximum ((sequence sequence)
                    &key (key #'identity) (start 0) (end nil))
  "This reduces MAX onto the sequence provided."
  (reduce #'max sequence :key key :start start :end end))

(defgeneric minimum? (sequence &key position key start end)
  (:documentation
   "Return true if the element at POSITION in SEQUENCE is a minimum under KEY
within the subsequence from START to END.  POSITION defaults to the last
index."))

(defmethod minimum? ((sequence sequence)
                      &key (position nil) (key #'identity) (start 0) (end nil))
  "Return true if the element at POSITION is a minimum of SEQUENCE under KEY."
  (when (null position)
    (setf position (1- (length sequence))))
  (<= (funcall key (elt sequence position))
      (minimum sequence :key key :start start :end end)))

(defgeneric maximum? (sequence &key position key start end)
  (:documentation
   "Return true if the element at POSITION in SEQUENCE is a maximum under KEY
within the subsequence from START to END.  POSITION defaults to the last
index."))

(defmethod maximum? ((sequence sequence)
                     &key (position nil) (key #'identity) (start 0) (end nil))
  "Return true if the element at POSITION is a maximum of SEQUENCE under KEY."
  (when (null position)
    (setf position (1- (length sequence))))
  (>= (funcall key (elt sequence position))
      (maximum sequence :key key :start start :end end)))

(behavior 'minimum
  (should= 1 (minimum '(3 1 4 1 5)))
  (should= 1 (minimum #(3 1 4 1 5)))
  ;; Subsequence from index 2 is (4 1 5).
  (should= 1 (minimum '(3 1 4 1 5) :start 2)))

(behavior 'maximum
  (should= 5 (maximum '(3 1 4 1 5)))
  (should= 5 (maximum #(3 1 4 1 5)))
  ;; Subsequence before index 3 is (3 1 4).
  (should= 4 (maximum '(3 1 4 1 5) :end 3)))

(behavior 'minimum?
  (should-be-true (minimum? '(3 1 4) :position 1))
  (should-be-false (minimum? '(3 1 4) :position 0)))

(behavior 'maximum?
  (should-be-true (maximum? '(3 1 4) :position 2))
  (should-be-false (maximum? '(3 1 4) :position 0)))

(defgeneric best (sequence predicate &key key)
  (:documentation
   "Return the ``best'' element of SEQUENCE according to PREDICATE and KEY.
Equivalent to the first element of SEQUENCE sorted by PREDICATE, but in
linear time."))

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

(defgeneric worst (sequence predicate &key key)
  (:documentation
   "Return the ``worst'' element of SEQUENCE according to PREDICATE and KEY.
Equivalent to the last element of SEQUENCE sorted by PREDICATE, but in
linear time."))

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

(behavior 'best
  (should-be-null (best nil #'<))
  (should= 1 (best '(3 1 4 1 5) #'<))
  (should= 5 (best '(3 1 4 1 5) #'>))
  (should= 1 (best #(3 1 4) #'<))
  (should-equal "aa" (best '("bb" "aa" "cc") #'string<)))

(behavior 'worst
  (should-be-null (worst nil #'<))
  (should= 5 (worst '(3 1 4 1 5) #'<))
  (should= 1 (worst '(3 1 4 1 5) #'>))
  (should= 4 (worst #(3 1 4) #'<)))

(defun nthable? (n list)
  "Return true if N is a valid index for LIST via NTH (a nonnegative integer
strictly less than the length of LIST)."
  (and (listp list)
       (typep n `(integer 0 ,(1- (length list))))))

(defun arefable? (array position)
  "Return true if POSITION is a valid multidimensional index list for ARRAY
via AREF: same rank as ARRAY, each coordinate in range for its dimension."
  (and (arrayp array)
       (listp position)
       (= (length (array-dimensions array))
          (length position))
       (every #'(lambda (position dimension)
                  (typep position `(integer 0 ,(1- dimension))))
              position
              (array-dimensions array))))

(behavior 'nthable?
  (should-be-true (nthable? 0 '(a b c)))
  (should-be-true (nthable? 2 '(a b c)))
  (should-be-false (nthable? 3 '(a b c)))
  (should-be-false (nthable? -1 '(a b c)))
  (should-be-false (nthable? 0 'not-a-list)))

(behavior 'arefable?
  (let ((a (make-array '(2 3))))
    (should-be-true (arefable? a '(0 0)))
    (should-be-true (arefable? a '(1 2)))
    (should-be-false (arefable? a '(2 0)))
    (should-be-false (arefable? a '(0)))
    (should-be-false (arefable? 1 '(0)))))

(defgeneric sort-on (sequence-to-sort ordering-sequence predicate &key key)
  (:documentation
   "Sort SEQUENCE-TO-SORT according to the parallel ORDERING-SEQUENCE.
Corresponding elements are paired; pairs are sorted by PREDICATE on the
ordering values (optionally through KEY), and the sorted SEQUENCE-TO-SORT
elements are returned."))

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
  "Sort the vector SEQUENCE-TO-SORT using the list ORDERING-SEQUENCE; return
a vector."
  (list-to-vector (sort-on (vector-to-list sequence-to-sort)
                           ordering-sequence
                           predicate
                           :key key)))

(defmethod sort-on (sequence-to-sort
                    (ordering-sequence vector)
                    predicate
                    &key (key #'identity))
  "Sort SEQUENCE-TO-SORT using the vector ORDERING-SEQUENCE."
  (sort-on sequence-to-sort
           (vector-to-list ordering-sequence)
           predicate
           :key key))

(defun sort-order (sequence predicate &key (key #'identity))
  "This function returns the indices in the order for the sorted sequence."
  (sort-on (loop for i from 0 below (length sequence) collect i)
           sequence
           predicate
           :key key))

(behavior 'sort-on
  (should-equal (sort-on '("a" "b" "c") '(3 1 2) #'<)
                '("b" "c" "a"))
  (should-equalp (sort-on #("a" "b" "c") '(3 1 2) #'<)
                 #("b" "c" "a")))

(behavior 'sort-order
  (should-equal (sort-order '(30 10 20) #'<) '(1 2 0))
  (should-equal (sort-order '(30 10 20) #'>) '(0 2 1)))

(defgeneric split (sequence separators &key key test remove-separators?)
  (:documentation
   "Split SEQUENCE into subsequences wherever an element is a member of
SEPARATORS (under KEY and TEST).  When REMOVE-SEPARATORS? is true (the
default), separator elements are omitted from the results."))

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

(behavior 'split
  (should-equal (split '(a b sep c d sep e) 'sep)
                '((a b) (c d) (e)))
  (should-equal (split '(a b x c) 'x)
                '((a b) (c)))
  (should-equal (split '(a x b) 'x :remove-separators? nil)
                '((a x) (b))))

(defgeneric slice (sequence &optional slice)
  (:documentation
   "Return a modular subset of SEQUENCE, taking every SLICE-th element
(SLICE defaults to 1).  SLICE may be any positive rational number."))

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

(behavior 'slice
  (should-equal (slice '(1 2 3 4 5 6 7 8 9) 2)
                '(1 3 5 7 9))
  (should-equalp (slice #(1 2 3 4 5 6 7 8 9) 2)
                 #(1 3 5 7 9))
  (should-equal (slice '(1 2 3 4 5) 1)
                '(1 2 3 4 5)))

(defun join-symbol-to-all-preceeding (symbol list)
  "This function takes a symbol and a list, and for every occurance of the
symbol in the list, it joins it to the item preceeding it.  For example:

> (join-symbol-to-all-preceeding :% '(10 :% 20 :% 30 :%))
=> '(:10% :20% :30%)

The result is affected by all of the *PRINT-...* variables in the same was as
the FORMAT builtin function."
;  (format t "join-symbol-to-all-preceeding ~A ~A~%" symbol list)
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

(behavior 'join-symbol-to-all-preceeding
  ;; Fresh lists: JOIN-SYMBOL-TO-ALL-PRECEEDING mutates its list argument.
  (should-equal (join-symbol-to-all-preceeding :% (list 100 :%))
                '(:100%))
  (let ((*print-base* 8))
    (should-equal (join-symbol-to-all-preceeding :% (list 64 :%))
                  '(:100%)))
  (should-equal (join-symbol-to-all-preceeding :% (list 10 :% 20 :% 30 :%))
                '(:10% :20% :30%))
  (should-equal (join-symbol-to-all-preceeding :% (list 10 :55%))
                '(10 :55%))
  (should-equal (join-symbol-to-all-preceeding :% (list 1 2 3 4 5))
                '(1 2 3 4 5))
  (should-equal (join-symbol-to-all-preceeding :% (list :a :b :c :d :e))
                '(:a :b :c :d :e))
  (should-equal (join-symbol-to-all-preceeding :foo (list :bar :foo :baz :foo))
                '(:barfoo :bazfoo))
  (should-equal
   (let ((*print-case* :downcase))
     (join-symbol-to-all-preceeding :b (list :a :b :c :b)))
   '(:|ab| :|cb|)))

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

(behavior 'join-symbol-to-all-following
  ;; Fresh lists: JOIN-SYMBOL-TO-ALL-FOLLOWING mutates its list argument.
  (should-equal (join-symbol-to-all-following :# (list :# :aabbcc))
                '(:#aabbcc))
  (should-equal (join-symbol-to-all-following :# (list :# 10 :# 20 :# 30))
                '(:#10 :#20 :#30))
  (let ((*print-base* 8))
    (should-equal (join-symbol-to-all-following :# (list :# 64))
                  '(:#100)))
  (should-equal (join-symbol-to-all-following :# (list :#55 10))
                '(:#55 10))
  (should-equal (join-symbol-to-all-following :# (list 1 2 3 4 5))
                '(1 2 3 4 5))
  (should-equal (join-symbol-to-all-following :# (list :a :b :c :d :e))
                '(:a :b :c :d :e))
  (should-equal (join-symbol-to-all-following :foo (list :foo 'bar :foo :baz))
                '(:foobar :foobaz)))

(defun set-equal (list-1 list-2 &key (key #'identity) test test-not)
  "Return true if LIST-1 and LIST-2 contain the same elements when viewed as
sets, using KEY and either TEST or TEST-NOT for element comparison.  Order
and duplicate multiplicity are ignored."
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

(behavior 'set-equal
  (should-be-true (set-equal '(1 2 3) '(3 2 1)))
  (should-be-true (set-equal '(1 1 2) '(2 1)))
  (should-be-false (set-equal '(1 2) '(1 2 3)))
  (should-be-true (set-equal '("a" "b") '("B" "A") :test #'string-equal)))

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

(behavior 'array-values
  (let ((a (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
    (should-equal (array-values a '((0 0) (1 1) (0 1)))
                  '(1 4 2))))
