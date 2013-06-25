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

(defpackage :sigma/behave
  (:nicknames :behave)
  (:use :common-lisp)
  (:export :behavior
	   :spec
	   :should
	   :should-not
	   :should-be-null
	   :should-be-a
	   :should-be-true
	   :should-be-false))
(in-package :sigma/behave)

(defmacro behavior (thing &body body)
  "The BEHAVIOR macro is used to specify a block of expected behavior for a
THING.  It specifies an example group, similar to the 'describe' blocks in
Ruby's RSpec.  It takes a single argument, the THING we are trying to describe,
and then a body of code to evaluate that is evaluated in an implicit PROGN.  It
is to be used around a set of examples, or around a set of assertions directly.

A contrived example:

(behavior 'float
	  (spec \"is an Abelian group\"
		(let ((a (random 10.0))
		      (b (random 10.0))
		      (c (random 10.0))
		      (e 1.0))
		  (spec \"closure\"
			(should-be-a 'float (* a b)))
		  (spec \"associativity\"
			(should= (* (* a b) c)
				 (* a (* b c))))
		  (spec \"identity element\"
			(should= a (* e a)))
		  (spec \"inverse element\"
			(let ((1/a (/ 1 a)))
			  (should= (* 1/a a)
				   (* a 1/a)
				   1.0)))
		  (spec \"commutitativity\"
			(should= (* a b) (* b a))))))"
  ;; Currently we don't actually use the THING specification for anything and
  ;; just throw it away.  Eventually we'll use it for logging or something.
  (declare (ignore thing))
  `(progn ,@body))

(defmacro spec (description &body body)
  "The SPEC macro is used to indicate a specification for a desired behavior.
It will normally serve as a grouping for assertions or nested SPECs.  For an
example, see the documentation of BEHAVIOR."
  ;; Currently we don't actually use the DESCRIPTION string for anything and
  ;; just throw it away.  Eventually we'll use it for logging or something.
  (declare (ignore description))
  `(progn ,@body))

(defmacro should (test &rest arguments)
  "The SHOULD macro is the basic building block for most of the behavior
checking.  It asserts that TEST returns truthfully for the arguments.  Typically
you will want to use one of the macros defined on top of SHOULD instead of using
it directly, such as SHOULD=."
  `(assert (funcall ,test ,@arguments)))

(defmacro should-not (test &rest arguments)
  "The SHOULD-NOT macro is identical to the SHOULD macro, except that it inverts
the result of the call with NOT."
  `(assert (not (funcall ,test ,@arguments))))

(defmacro should-be-null (&rest arguments)
  "The SHOULD-BE-NULL macro is a short-hand method for (SHOULD #'NULL ...)"
  `(should #'null ,@arguments))

(defmacro should-be-a (type &rest things)
  "The SHOULD-BE-A macro specifies that one or more THINGS should be of the type
specified by TYPE.

(should-be-a 'integer 1) ; passes
(should-be-a 'float 1) ; passes
(should-be-a 'integer 1 2 3 4 5 6 7 8 9) ; passes
(should-be-a 'integer 1.0) ; fails"
  (mapcar (lambda (thing)
	    (assert (typep thing type)))
	  things))

(defun should-macro-constructor (should-prefix test-function)
  (assert (symbolp should-prefix))
  (assert (symbolp test-function))
  (let ((macro-name (intern (concatenate 'string
					 (symbol-name should-prefix)
					 (symbol-name test-function)))))
    (eval `(progn (defmacro ,macro-name (&rest arguments)
		    `(should #',',test-function ,@arguments))
		  (export ',macro-name)))))

(defun should-not-macro-constructor (should-not-prefix test-function)
  (assert (symbolp should-not-prefix))
  (assert (symbolp test-function))
  (let ((macro-name (intern (concatenate 'string
					 (symbol-name should-not-prefix)
					 (symbol-name test-function)))))
    (eval `(progn (defmacro ,macro-name (&rest arguments)
		    `(should-not #',',test-function ,@arguments))
		  (export ',macro-name)))))

(loop for test-function in '(=
			     /=
			     <
			     >
			     <=
			     >=)
   do (should-macro-constructor 'should test-function))

(loop for test-function in '(eq
			     eql
			     equal
			     equalp
			     string=
			     string/=
			     string<
			     string>
			     string<=
			     string>=
			     string-equal
			     string-not-equal
			     string-lessp
			     string-greaterp
			     string-not-greaterp
			     string-not-lessp)
     do (should-macro-constructor 'should- test-function)
        (should-not-macro-constructor 'should-not- test-function))

(defmacro should-be-true (&body body)
  "The SHOULD-BE-TRUE macro is a short-hand method for (SHOULD #'IDENTITY ...)."
  `(should #'identity (progn ,@body)))

(defmacro should-be-false (&body body)
  "The SHOULD-BE-FALSE macro is a short-hand method for (SHOULD #'NOT ...)."
  `(should #'not (progn ,@body)))
