;;;; Copyright (c) 2005 -- 2013, Christopher Mark Gore,
;;;; Soli Deo Gloria,
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

(defpackage :cgore-control
  (:nicknames :control)
  (:use :common-lisp :cgore-behave)
  (:export :aand
	   :a?and
	   :ablock
	   :a?block
	   :acond
	   :a?cond
	   :aif
	   :a?if
	   :awhen
	   :a?when
	   :awhile
	   :a?while
	   :alambda
	   :a?lambda
	   :compose
	   :conjoin
	   :curry
	   :deletef
	   :disjoin
	   :do-until
	   :do-while
	   :duplicate
	   :for
	   :forever
	   :function-alias
	   :function-aliases
	   :it
	   :multicond
	   :operator-to-function
	   :opf
	   :rcompose
	   :rcurry
           :self
	   :swap
	   :swap-unless
	   :swap-when
	   :unimplemented
	   :until
	   :while))
(in-package :cgore-control)

(defmacro aif (conditional t-action &optional nil-action)
  "AIF is an anaphoric IF, from Paul Graham's ``On Lisp'' page 190.
   It works like IF, but automatically sets IT to the conditional."
  `(let ((it ,conditional))
     (if it ,t-action ,nil-action)))

(should-eq 'foo (aif 'foo it))
(should-eq 'no (aif nil 'yes 'no))
(should-equal '(nil) (aif nil 'yes `(,it)))
(should-eq 'inner (aif 'outer (aif 'inner it)))
(should= 30 (aif (* 2 3) (* 5 it)))

(defmacro a?if (anaphor conditional t-action &optional nil-action)
  "A?IF This is a variant of AIF that allows for specification of the anaphor."
  `(let ((,anaphor ,conditional))
     (if ,anaphor ,t-action ,nil-action)))

(should-eq 'value (a?if foo 'value foo))
(should-eq 'no (a?if foo nil 'yes 'no))
(should-equal '(nil) (a?if foo nil 'yes `(,foo)))
(should-equal '(outer inner) (a?if foo 'outer (a?if bar 'inner `(,foo ,bar))))

(defmacro aand (&rest arguments)
  "AAND is an anaphoric AND, from Paul Graham's ``On Lisp'' page 191.
   It works like AND, but defines IT over and over for each argument."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(aif ,(first arguments)
                  (aand ,@(rest arguments))))))

(should-eq nil (aand nil))
(should-eq nil (aand nil nil nil))
(should-eq nil (aand 1 2 3 nil 4 5 6))
(should= 1 (aand 1))
(should= 2 (aand 1 (* 2 it)))
(should= 4 (aand 1
		 (* 2 it)
		 (* 2 it)))
(should= 16 (aand 1
		  (* 2 it)
		  (* 2 it)
		  (* 2 it)
		  (* 2 it)))
(should= 2 (aand 100
		 (* 200 it)
		 (aand 2 it)))
(should= 6 (aand 1 2 3 (aand 4 5 6)))

(defmacro a?and (anaphor &rest arguments)
  "This is an anaphoric AND that allows for the specification of the anaphor."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(a?if ,anaphor ,(first arguments)
                  (a?and ,anaphor ,@(rest arguments))))))

(should-be-null (a?and foo nil))
(should-be-null (a?and foo nil nil nil))
(should-be-null (a?and foo 1 2 3 nil 4 5 6))
(should= 1 (a?and foo 1))
(should= 2 (a?and foo 1 (* 2 foo)))
(should= 6 (a?and foo 1 2 3 (a?and foo 4 5 6)))
(should-equal '(outer inner)
	      (a?and foo 1 2 3 'outer (a?and bar 4 5 6 'inner `(,foo ,bar))))

(defmacro alambda (parms &body body)
  "ALAMBDA is an anaphoric LAMBDA, from Paul Graham's ``On Lisp'' page 193.
   It works like LAMBDA, but you can call it recursively with SELF."
  `(labels ((self ,parms ,@body))
           #'self))

(should= (* 10 9 8 7 6 5 4 3 2 1)
	 (funcall (alambda (x) ; Simple recursive factorial example.
			   (if (<= x 0)
			       1
			       (* x (self (1- x)))))
		  10))

(defmacro a?lambda (anaphor parms &body body)
  "A?LAMBDA is a variant of ALAMBDA that allows you to specify the anaphor."
  `(labels ((,anaphor ,parms ,@body))
           #',anaphor))

(should= (* 10 9 8 7 6 5 4 3 2 1)
	 (funcall (a?lambda foo (x) ; Simple recursive factorial example.
			    (if (<= x 0)
				1
				(* x (foo (1- x)))))
		  10))

(defmacro ablock (tag &rest args)
  "ABLOCK is an anaphoric BLOCK, from Paul Graham's ``On Lisp'' page 193.
   It works like BLOCK, but defines IT over and over for each argument."
  `(block ,tag
          ,(funcall (alambda (args)
                             (case (length args)
                               (0 nil)
                               (1 (car args))
                               (t `(let ((it ,(car args)))
                                        ,(self (cdr args))))))
                    args)))

(let ((x 1))
  (ablock foo
	  (setf x (* x 2))
	  (setf x (* it 2))
	  (setf x (* it 3))
	  (return-from foo)
	  (setf x 1234))
  (should= x (* 1 2 2 3))
  (should/= x 1234))
			 
(defmacro a?block (tag anaphor &rest args)
  "A?BLOCK is a variant of ABLOCK that allows you to specify the anaphor."
  `(block ,tag
          ,(funcall (alambda (args)
                             (case (length args)
                               (0 nil)
                               (1 (car args))
                               (t `(let ((,anaphor ,(car args)))
                                        ,(self (cdr args))))))
                    args)))

(let ((x 1))
  (a?block foo bar
	  (setf x (* x 2))
	  (setf x (* bar 2))
	  (setf x (* bar 3))
	  (return-from foo)
	  (setf x 1234))
  (should= x (* 1 2 2 3))
  (should/= x 1234))
			 
(defmacro acond (&rest clauses)
  "ACOND is an anaphoric COND, from Paul Graham's ``On Lisp'' page 191.
   It works like COND, but defines IT over and over for each argument."
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
            (if ,sym
              (let ((it ,sym)) ,@(cdr cl1))
              (acond ,@(cdr clauses)))))))

(let ((a nil)
      (b nil)
      (c 3))
  (should= c (acond (a :foo)
		    (b :bar)
		    (c it))))

(defmacro a?cond (anaphor &rest clauses)
  "A?COND is a variant of ACOND that allows you to specify the anaphor."
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
            (if ,sym
              (let ((,anaphor ,sym)) ,@(cdr cl1))
              (a?cond ,anaphor ,@(cdr clauses)))))))

(let ((a nil)
      (b nil)
      (c 3))
  (should= c (a?cond baz
		     (a :foo)
		     (b :bar)
		     (c baz))))

(defmacro awhen (test-form &body body)
  "This is anaphoric WHEN, from Paul Graham's ``On Lisp'' page 191."
  `(aif ,test-form (progn ,@body)))

(should= 24 (awhen 12 (* 2 it)))
(should-be-null (awhen nil (* 2 it)))
(let* ((it :foo)
       (result (awhen :bar it)))
  (should-not-eq result :foo)
  (should-eq result :bar))

(defmacro a?when (anaphor test-form &body body)
  "This is an anaphoric WHEN that allows for the specification of the anaphor."
  `(a?if ,anaphor ,test-form (progn ,@body)))

(should= 24 (a?when foo 12 (* 2 foo)))
(should-be-null (a?when foo nil (* 2 foo)))
(let* ((baz :foo)
       (result (a?when baz :bar baz)))
  (should-not-eq result :foo)
  (should-eq result :bar))

(defmacro awhile (expression &body body)
  "This is anaphoric WHILE, from Paul Graham's ``On Lisp'' page 191."
  `(do ((it ,expression ,expression))
       ((not it))
     ,@body))

(let ((i 0))
  (should-be-null (awhile (< i 10) (incf i)))
  (should= i 10))
(let ((forward '(1 2 3 4 5))
      (backward nil))
  (should-be-null (awhile (pop forward)
		      (push it backward)))
  (should-be-null forward)
  (should-equal '(5 4 3 2 1) backward))

(defmacro a?while (anaphor expression &body body)
  "This is an anaphoric WHILE that allows for the specification of the anaphor."
  `(do ((,anaphor ,expression ,expression))
       ((not ,anaphor))
     ,@body))

(let ((i 0))
  (should-be-null (a?while foo (< i 10) (incf i)))
  (should= i 10))
(let ((forward '(1 2 3 4 5))
      (backward nil))
  (should-be-null (a?while number (pop forward)
			   (push number backward)))
  (should-be-null forward)
  (should-equal '(5 4 3 2 1) backward))

(defun rcompose (&rest functions)
  "A version of COMPOSE in reverse order."
  (dolist (function functions)
    (assert (or (functionp function)
                (symbolp function))))
  (destructuring-bind (function-1 . rest)
    functions
    #'(lambda (&rest arguments)
        (reduce #'(lambda (v f)
                    (funcall f v))
                rest
                :initial-value (apply function-1 arguments)))))

(let ((numbers '(1 2 3 4 5 6 7 8 9)))
  (should-equal (mapcar (lambda (number)
			  (sin (cos number)))
			numbers)
		(mapcar (rcompose #'cos #'sin) numbers)))

(defun compose (&rest functions)
  "This function composes a single function from a list of several functions
such that the new function is equivalent to calling the functions in
succession.  This is based upon a COMPOSE function in Paul Graham's ``ANSI
Common Lisp'' which is  based upon the compose function from Dylan, a
programming language which he describes as a ``cross between Scheme and Common
Lisp, with a syntax like Pascal.''"
  (apply #'rcompose (reverse functions)))

(let ((numbers '(1 2 3 4 5 6 7 8 9)))
  (should-equal (mapcar (lambda (number)
			  (sin (cos number)))
			numbers)
		(mapcar (compose #'sin #'cos) numbers)))

(defun conjoin (predicate &rest predicates)
  "This function takes in one or more predicates, and returns a predicate that
returns true whenever all of the predicates return true.  This is from Paul
Graham's ``ANSI Common Lisp'' and is based upon the conjoin function from
Dylan, a programming language which he describes as a ``cross between Scheme
and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp predicate)
              (symbolp predicate)))
  (dolist (predicate predicates)
    (assert (or (functionp predicate)
                (symbolp predicate))))
  (if (null predicates)
    predicate
    (let ((conjoinment (apply #'conjoin predicates)))
      #'(lambda (&rest arguments)
          (and (apply predicate arguments)
               (apply conjoinment arguments))))))

(flet ((%2? (i)
	 (zerop (mod i 2)))
       (%3? (i)
	 (zerop (mod i 3))))
  (loop for i from 1 to 100
     do (should-eq (and (%2? i) (%3? i))
		   (funcall (conjoin #'%2? #'%3?) i))))

(defun curry (function &rest arguments)
  "This function takes in a function and some of its arguments, and returns a
function that expects the rest of the required arguments.  This is from Paul
Graham's ``ANSI Common Lisp'' and is based upon the curry function from
Dylan, a programming language which he describes as a ``cross between Scheme
and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp function)
              (symbolp function)))
  #'(lambda (&rest more-arguments)
      (apply function (append arguments more-arguments))))

#|
(loop for i from 1 to 100
     do (should= (funcall (curry #'sin #'cos #'tan) i)
		 (sin (cos (tan i)))))
|#

#-cmu
(defmacro deletef (item sequence &rest rest)
  `(setf ,sequence
         (delete ,item ,sequence ,@rest)))

(defun disjoin (predicate &rest predicates)
  "This function takes in one or more predicates, and returns a predicate that
returns true whenever any of the predicates return true.  This is from Paul
Graham's ``ANSI Common Lisp'' and is based upon the disjoin function from
Dylan, a programming language which he describes as a ``cross between Scheme
and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp predicate)
              (symbolp predicate)))
  (dolist (predicate predicates)
    (assert (or (functionp predicate)
                (symbolp predicate))))
  (if (null predicates)
    predicate
    (let ((disjoinment (apply #'disjoin predicates)))
      #'(lambda (&rest arguments)
          (or (apply predicate arguments)
              (apply disjoinment arguments))))))


(flet ((%2? (i)
	 (zerop (mod i 2)))
       (%3? (i)
	 (zerop (mod i 3))))
  (loop for i from 1 to 100
     do (should-eq (or (%2? i) (%3? i))
		   (funcall (disjoin #'%2? #'%3?) i))))

(defgeneric duplicate (item))

(defmethod duplicate ((list list))
  "This returns a deeply new duplicate of the list."
  (mapcar 'duplicate list))

(defmethod duplicate ((array array))
  "This returns a deeply new duplicate of the array."
  (let ((result (make-array (array-dimensions array)
                            :element-type (array-element-type array)
                            :adjustable (adjustable-array-p array))))
    (when (array-dimensions array)
      (dotimes (index (array-total-size array))
        (setf (row-major-aref result index)
              (duplicate (row-major-aref array index)))))
    result))

(defmethod duplicate ((number number))
  number)

(defmethod duplicate ((symbol symbol))
  symbol)

(defmethod duplicate ((function function))
  ;; XXX: I believe this is correct, but I am not really sure.
  function)

(defmacro for (initial conditional step-action &rest body)
  "A FOR macro, much like the ``for'' in the C programming language.
A simple example:
  (for ((i 0))
       (< i 10)
       (incf i)
    (format t \"~%~A\" i))
prints the numbers from 0 through 9, each on their own lines.
Generally this should not be used, but instead the native looping methods."
  `(let ,initial
     (while ,conditional
       (prog1 (progn ,@body) ,step-action))))

(defmacro forever (&rest body)
  `(while t ,@body))

(defun function-alias (function &rest aliases)
  "This produces one or more aliases (alternate names) for a function.
For example, you might do something like:
> (function-alias 'that-guy-doesnt-know-when-to-stop-typing 'shorter)"
  (loop for alias in aliases
        do (setf (fdefinition alias) (fdefinition function))))

(function-alias 'function-alias 'function-aliases) ; This line seemed fitting.

(defmacro multicond (&rest clauses)
  "A macro much like COND, but where multiple clauses may be evaluated."
  `(mapcar #'(lambda (clause)
               (when (first clause)
                 (mapcar #'eval (rest clause))))
           ',clauses))

(defun operator-to-function (operator)
  (lambda (&rest rest)
    (eval `(,operator ,@rest))))

(defmacro opf (operator variable &rest arguments)
  "OPF is a generic operate-and-store macro, along the lines of INCF and DECF,
but allowing for any operation.  For example:
  (opf #'+ foo 42)
does something like
  (incf foo 42)
but you could also do
  (opf #'+ foo 1 2 3 4 5)
with it doing the obvious thing, whereas you cannot do
  (incf foo 1 2 3 4 5)
in any Common Lisp I have used."
  `(setf ,variable
         (funcall ,operator ,variable ,@arguments)))

(defun rcurry (function &rest arguments)
  "This function takes in a function and some of its ending arguments, and
returns a function that expects the rest of the required arguments.  This is
from Paul Graham's ``ANSI Common Lisp'' and is based upon the rcurry function
from Dylan, a programming language which he describes as a ``cross between
Scheme and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp function)
              (symbolp function)))
  #'(lambda (&rest more-arguments)
      (apply function (append more-arguments arguments))))

#|
(loop for i from 1 to 100
     do (should= (funcall (curry #'sin #'cos #'tan) i)
		 (tan (cos (sin i)))))
|#

(defmacro swap (x y)
  "A simple SWAP macro.  The values of the first form and the second form are
swapped with each other."
  `(psetf ,x ,y
          ,y ,x))

(let ((x 15)
      (y 37))
  (swap x y)
  (should= y 15)
  (should= x 37))

(defmacro swap-unless (predicate x y)
  "This macro calls SWAP unless the predicate evaluates to true."
  `(unless (funcall ,predicate ,x ,y)
     (swap ,x ,y)))

(let ((smaller 1)
      (larger 2))
  (swap-unless #'< smaller larger)
  (should= smaller 1)
  (should= larger 2))

(defmacro swap-when (predicate x y)
  "This macro calls SWAP only when the predicate evaluates to true."
  `(when (funcall ,predicate ,x ,y)
     (swap ,x ,y)))

(let ((smaller 2)
      (larger 1))
  (swap-when #'> smaller larger)
  (should= smaller 1)
  (should= larger 2))

(defun unimplemented ()
  (error "This is not yet implemented."))

;;; TODO: It would be nice if this returned the last evaluated element of
;;;       the body instead of the conditional.
(defmacro while (conditional &rest body)
  "A WHILE macro, operating in a matter similar to the while loop in C."
  `(do ()
     ((not ,conditional))
     ,@body))

(let ((x 0))
  (while (< x 10)
    (incf x))
  (should= x 10))

(defmacro do-while (conditional &rest body)
  "The DO-WHILE macro operates like a do {BODY} while (CONDITIONAL) in the C
  programming language."
  `(progn ,@body
          (while ,conditional
                 ,@body)))

(let ((i 100))
  (do-while (<= 0 i) (decf i))
  (should= i -1))

(defmacro do-until (conditional &rest body)
  "A DO-UNTIL loop construct; it operates like do {BODY} while (! CONDITIONAL)
construct in the C programming language."
  `(do-while (not ,conditional)
     ,@body))

(let ((i 100))
  (do-until (<= i 0) (decf i))
  (should= i 0))

(defmacro until (conditional &rest body)
  "An UNTIL loop construct.  It operates in the negative sense as WHILE."
  `(while (not ,conditional)
     ,@body))

#|
(let ((x 0))
  (until (<= 10 x)
    (incf x))
  (should= x 10))
|#