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

(defpackage :cgore-behave
  (:nicknames :behave)
  (:use :common-lisp)
  (:export :should
	   :should-not
	   :should-be-null))
(in-package :cgore-behave)

(defmacro should (test &rest arguments)
  `(assert (funcall ,test ,@arguments)))

(defmacro should-not (test &rest arguments)
  `(assert (not (funcall ,test ,@arguments))))

(defmacro should-be-null (&rest arguments)
  `(should #'null ,@arguments))

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