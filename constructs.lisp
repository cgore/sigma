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


(defpackage :cgore-constructs
  (:nicknames :constructs)
  (:use :common-lisp)
  (:export
    :aand
    :a?and
    :ablock
    :acond
    :aif
    :a?if
    :aif-otherwise-nil
    :awhen
    :a?when
    :awhile
    :a?while
    :alambda
    :compose
    :conjoin
    :curry
    :deletef
    :disjoin
    :do-until
    :do-while
    :for
    :forever
    :function-alias
    :function-aliases
    :it
    :multicond
    :operator-to-function
    :otherwise-nil
    :rcompose
    :rcurry
    :swap
    :swap-unless
    :swap-when
    :unimplemented
    :until
    :while
    ))
(in-package :cgore-constructs)


(defmacro aand (&rest arguments)
  "This is anaphoric AND, from Paul Graham's ``On Lisp'' page 191."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(aif ,(first arguments)
                  (aand ,@(rest arguments))))))


(defmacro a?and (anaphor &rest arguments)
  "This is an anaphoric AND that allows for the specification of the anaphor."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(a?if ,anaphor ,(first arguments)
                  (a?and ,anaphor ,@(rest arguments))))))


(defmacro alambda (parms &body body)
  "This is anaphoric LAMBDA, from Paul Graham's ``On Lisp'' page 193."
  `(labels ((self ,parms ,@body))
           #'self))


(defmacro ablock (tag &rest args)
  "This is anaphoric COND, from Paul Graham's ``On Lisp'' page 193."
  `(block ,tag
          ,(funcall (alambda (args)
                             (case (length args)
                               (0 nil)
                               (1 (car args))
                               (t `(let ((it ,(car args)))
                                        ,(self (cdr args))))))
                    args)))


(defmacro acond (&rest clauses)
  "This is anaphoric COND, from Paul Graham's ``On Lisp'' page 191."
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
            (if ,sym
              (let ((it ,sym)) ,@(cdr cl1))
              (acond ,@(cdr clauses)))))))


(defmacro aif (conditional t-action &optional nil-action)
  "This is anaphoric IF, from Paul Graham's ``On Lisp'' page 190."
  `(let ((it ,conditional))
     (if it ,t-action ,nil-action)))


(defmacro a?if (anaphor conditional t-action &optional nil-action)
  "This is an anaphoric IF that allows for specification of the anaphor."
  `(let ((,anaphor ,conditional))
     (if ,anaphor ,t-action ,nil-action)))


(defmacro aif-otherwise-nil (conditional t-action)
  `(aif ,conditional ,t-action nil))


(defmacro awhen (test-form &body body)
  "This is anaphoric WHEN, from Paul Graham's ``On Lisp'' page 191."
  `(aif ,test-form (progn ,@body)))


(defmacro a?when (anaphor test-form &body body)
  "This is an anaphoric WHEN that allows for the specification of the anaphor."
  `(a?if ,anaphor ,test-form (progn ,@body)))


(defmacro awhile (expression &body body)
  "This is anaphoric WHILE, from Paul Graham's ``On Lisp'' page 191."
  `(do ((it ,expression ,expression))
       ((not it))
     ,@body))


(defmacro a?while (anaphor expression &body body)
  "This is an anaphoric WHILE that allows for the specification of the anaphor."
  `(do ((,anaphor ,expression ,expression))
       ((not ,anaphor))
     ,@body))


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


(defun compose (&rest functions)
  "This function composes a single function from a list of several functions
such that the new function is equivalent to calling the functions in
succession.  This is based upon a COMPOSE function in Paul Graham's ``ANSI
Common Lisp'' which is  based upon the compose function from Dylan, a
programming language which he describes as a ``cross between Scheme and Common
Lisp, with a syntax like Pascal.''"
  (apply #'rcompose (reverse functions)))


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


(defmacro do-until (conditional &rest body)
  "A DO-UNTIL loop construct; it operates like do {BODY} while (! CONDITIONAL)
construct in the C programming language."
  `(do-while (not ,conditional)
     ,@body))


(defmacro do-while (conditional &rest body)
  "The DO-WHILE macro operates like a do {BODY} while (CONDITIONAL) in the C
  programming language."
  `(progn ,@body
          (while ,conditional
                 ,@body)))


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


(defmacro otherwise-nil (conditional t-action)
  `(if ,conditional ,t-action nil))


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


(defmacro swap (x y)
  "A simple SWAP macro.  The values of the first form and the second form are
swapped with each other."
  `(psetf ,x ,y
          ,y ,x))

(let ((x 15)
      (y 37))
  (swap x y)
  (assert (= y 15))
  (assert (= x 37)))


(defmacro swap-unless (predicate x y)
  "This macro calls SWAP unless the predicate evaluates to true."
  `(unless (funcall ,predicate ,x ,y)
     (swap ,x ,y)))


(defmacro swap-when (predicate x y)
  "This macro calls SWAP only when the predicate evaluates to true."
  `(when (funcall ,predicate ,x ,y)
     (swap ,x ,y)))


(defun unimplemented ()
  (assert nil))


(defmacro until (conditional &rest body)
  "An UNTIL loop construct."
  `(while (not ,conditional)
     ,@body))


(defmacro while (conditional &rest body)
  "A WHILE macro, operating in a matter similar to the while loop in C."
  `(do ()
     ((not ,conditional))
     ,@body))
