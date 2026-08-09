;;;; Copyright (c) 2005 -- 2026, Christopher Mark Gore,
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

(defpackage :sigma/control
  (:use :common-lisp
        :sigma/behave)
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
           :->
           :->>
           :as->
           :alambda
           :a?lambda
           :compose
           :conjoin
           :cond->
           :cond->>
           :curry
           :defconstant-once
           :deletef
           :disjoin
           :do-until
           :do-while
           :duplicate
           :fop
           :for
           :forever
           :function-alias-as-a-function
           :function-aliases-as-a-function
           :function-alias
           :function-aliases
           :it
           :juxt
           :juxtapose
           :macro-alias
           :multicond
           :operator-to-function
           :opf
           :rcompose
           :rcurry
           :self
           :some->
           :some->>
           :swap
           :swap-unless
           :swap-when
           :thread-as
           :thread-cond-first
           :thread-cond-last
           :thread-first
           :thread-last
           :thread-some-first
           :thread-some-last
           :unimplemented
           :until
           :while))
(in-package :sigma/control)

(defmacro aif (conditional t-action &optional nil-action)
  "AIF is an anaphoric IF, from Paul Graham's ``On Lisp'' page 190.
   It works like IF, but automatically sets IT to the conditional."
  `(let ((it ,conditional))
     (if it ,t-action ,nil-action)))

(behavior 'aif
          (should-eq 'foo (aif 'foo it))
          (should-eq 'no (aif nil 'yes 'no))
          (should-equal '(nil) (aif nil 'yes `(,it)))
          (should-eq 'inner (aif 'outer (aif 'inner it)))
          (should= 30 (aif (* 2 3) (* 5 it))))

(defmacro a?if (anaphor conditional t-action &optional nil-action)
  "A?IF This is a variant of AIF that allows for specification of the anaphor."
  `(let ((,anaphor ,conditional))
     (if ,anaphor ,t-action ,nil-action)))

(behavior 'a?if
          (should-eq 'value (a?if foo 'value foo))
          (should-eq 'no (a?if foo nil 'yes 'no))
          (should-equal '(nil) (a?if foo nil 'yes `(,foo)))
          (should-equal '(outer inner)
                        (a?if foo 'outer (a?if bar 'inner `(,foo ,bar)))))

(defmacro aand (&rest arguments)
  "AAND is an anaphoric AND, from Paul Graham's ``On Lisp'' page 191.
   It works like AND, but defines IT over and over for each argument."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(aif ,(first arguments)
                  (aand ,@(rest arguments))))))

(behavior 'aand
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
          (should= 6 (aand 1 2 3 (aand 4 5 6))))

(defmacro a?and (anaphor &rest arguments)
  "This is an anaphoric AND that allows for the specification of the anaphor."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(a?if ,anaphor ,(first arguments)
                  (a?and ,anaphor ,@(rest arguments))))))

(behavior 'a?and
          (should-be-null (a?and foo nil))
          (should-be-null (a?and foo nil nil nil))
          (should-be-null (a?and foo 1 2 3 nil 4 5 6))
          (should= 1 (a?and foo 1))
          (should= 2 (a?and foo 1 (* 2 foo)))
          (should= 6 (a?and foo 1 2 3 (a?and foo 4 5 6)))
          (should-equal '(outer inner)
                        (a?and foo 1 2 3 'outer
                               (a?and bar 4 5 6 'inner `(,foo ,bar)))))

(defmacro alambda (parms &body body)
  "ALAMBDA is an anaphoric LAMBDA, from Paul Graham's ``On Lisp'' page 193.
   It works like LAMBDA, but you can call it recursively with SELF."
  `(labels ((self ,parms ,@body))
           #'self))

(behavior 'alambda
          (should= (* 10 9 8 7 6 5 4 3 2 1)
                   (funcall (alambda (x) ; Simple recursive factorial.
                              (if (<= x 0)
                                  1
                                  (* x (self (1- x)))))
                            10)))

(defmacro a?lambda (anaphor parms &body body)
  "A?LAMBDA is a variant of ALAMBDA that allows you to specify the anaphor."
  `(labels ((,anaphor ,parms ,@body))
           #',anaphor))

(behavior 'a?lambda
          (should= (* 10 9 8 7 6 5 4 3 2 1)
                   (funcall (a?lambda foo (x) ; Simple recursive factorial.
                              (if (<= x 0)
                                  1
                                  (* x (foo (1- x)))))
                            10)))

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

(behavior 'ablock
          (let ((x 1))
            (ablock foo
                    (setf x (* x 2))
                    (setf x (* it 2))
                    (setf x (* it 3))
                    (return-from foo)
                    (setf x 1234))
            (should= x (* 1 2 2 3))
            (should/= x 1234)))

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

(behavior 'a?block
          (let ((x 1))
            (a?block foo bar
                     (setf x (* x 2))
                     (setf x (* bar 2))
                     (setf x (* bar 3))
                     (return-from foo)
                     (setf x 1234))
            (should= x (* 1 2 2 3))
            (should/= x 1234)))

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

(behavior 'acond
          (let ((a nil)
                (b nil)
                (c 3))
            (should= c (acond (a :foo)
                              (b :bar)
                              (c it)))))

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

(behavior 'a?cond
          (let ((a nil)
                (b nil)
                (c 3))
            (should= c (a?cond baz
                               (a :foo)
                               (b :bar)
                               (c baz)))))

(defmacro awhen (test-form &body body)
  "This is anaphoric WHEN, from Paul Graham's ``On Lisp'' page 191."
  `(aif ,test-form (progn ,@body)))

(behavior 'awhen
          (should= 24 (awhen 12 (* 2 it)))
          (should-be-null (awhen nil (* 2 it)))
          (let* ((it :foo)
                 (result (awhen :bar it)))
            (should-not-eq result :foo)
            (should-eq result :bar)))

(defmacro a?when (anaphor test-form &body body)
  "This is an anaphoric WHEN that allows for the specification of the anaphor."
  `(a?if ,anaphor ,test-form (progn ,@body)))

(behavior 'a?when
          (should= 24 (a?when foo 12 (* 2 foo)))
          (should-be-null (a?when foo nil (* 2 foo)))
          (let* ((baz :foo)
                 (result (a?when baz :bar baz)))
            (should-not-eq result :foo)
            (should-eq result :bar)))

(defmacro awhile (expression &body body)
  "This is anaphoric WHILE, from Paul Graham's ``On Lisp'' page 191.
Returns the value of the last form in BODY from the last iteration, or NIL if
BODY never runs."
  (let ((result (gensym "RESULT")))
    `(let ((,result nil))
       (do ((it ,expression ,expression))
           ((not it) ,result)
         (setf ,result (progn ,@body))))))

(behavior 'awhile
          (let ((i 0))
            (should= 10 (awhile (< i 10) (incf i)))
            (should= i 10))
          (let ((forward '(1 2 3 4 5))
                (backward nil))
            (should-equal '(5 4 3 2 1)
                          (awhile (pop forward)
                            (push it backward)))
            (should-be-null forward)
            (should-equal '(5 4 3 2 1) backward))
          (should-be-null (awhile nil t)))

(defmacro a?while (anaphor expression &body body)
  "This is an anaphoric WHILE that allows for the specification of the anaphor.
Returns the value of the last form in BODY from the last iteration, or NIL if
BODY never runs."
  (let ((result (gensym "RESULT")))
    `(let ((,result nil))
       (do ((,anaphor ,expression ,expression))
           ((not ,anaphor) ,result)
         (setf ,result (progn ,@body))))))

(behavior 'a?while
          (let ((i 0))
            (should= 10 (a?while foo (< i 10) (incf i)))
            (should= i 10))
          (let ((forward '(1 2 3 4 5))
                (backward nil))
            (should-equal '(5 4 3 2 1)
                          (a?while number (pop forward)
                            (push number backward)))
            (should-be-null forward)
            (should-equal '(5 4 3 2 1) backward))
          (should-be-null (a?while foo nil t)))

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

(behavior 'rcompose
          (let ((numbers '(1 2 3 4 5 6 7 8 9)))
            (should-equal (mapcar (lambda (number)
                                    (sin (cos number)))
                                  numbers)
                          (mapcar (rcompose #'cos #'sin) numbers))))

(defun compose (&rest functions)
  "This function composes a single function from a list of several functions
such that the new function is equivalent to calling the functions in
succession.  This is based upon a COMPOSE function in Paul Graham's ``ANSI
Common Lisp'' which is  based upon the compose function from Dylan, a
programming language which he describes as a ``cross between Scheme and Common
Lisp, with a syntax like Pascal.''"
  (apply #'rcompose (reverse functions)))

(behavior 'compose
          (let ((numbers '(1 2 3 4 5 6 7 8 9)))
            (should-equal (mapcar (lambda (number)
                                    (sin (cos number)))
                                  numbers)
                          (mapcar (compose #'sin #'cos) numbers))))

(defun juxtapose (&rest functions)
  "Return a function that is the juxtaposition of FUNCTIONS, as in Clojure's
JUXT.  The returned function takes any number of arguments and returns a list
of the results of applying each of FUNCTIONS to those arguments, in order:

  (funcall (juxtapose #'a #'b #'c) x)  =>  (list (a x) (b x) (c x))

Unlike COMPOSE, which pipes a value through functions in series, JUXTAPOSE
applies each function independently to the same arguments (fan-out).
FUNCTIONS may be function objects or symbols naming functions.  JUXT is an
alias for this function."
  (dolist (function functions)
    (assert (or (functionp function)
                (symbolp function))))
  (lambda (&rest arguments)
    (mapcar (lambda (function)
              (apply function arguments))
            functions)))

(behavior 'juxtapose
  (should-equal (funcall (juxtapose #'1+ #'1-) 10) '(11 9))
  (should-equal (funcall (juxtapose #'car #'cdr) '(a b c)) '(a (b c)))
  (should-equal (funcall (juxtapose #'+ #'*) 2 3 4) '(9 24))
  (should-equal (funcall (juxtapose #'identity) 'x) '(x))
  (should-equal (funcall (juxtapose)) '()))

(defmacro thread-first (x &rest forms)
  "Thread-first macro, as in Clojure's ->.

Inserts X as the second element of each of FORMS (the first argument after the
operator), nesting left-to-right:

  (thread-first x (foo y) (bar z) (baz w))
  => (baz (bar (foo x y) z) w)

A non-list form F is treated as (F).  With no FORMS, expands to X.
The macro -> is an alias for THREAD-FIRST."
  (reduce (lambda (acc form)
            (let ((form (if (listp form) form (list form))))
              `(,(first form) ,acc ,@(rest form))))
          forms
          :initial-value x))

(behavior 'thread-first
  (should= 5 (thread-first 5))
  (should= 6 (thread-first 5 1+))
  (should= 6 (thread-first 5 (1+)))
  (should-equal '(a b c)
                (thread-first 'a (list 'b) (append '(c))))
  (should= 9
           (thread-first 2
             (* 3)
             (+ 3)))
  (should-equal '(3 2 1)
                (thread-first (list 1 2 3)
                  (reverse)
                  (copy-list)))
  ;; Classic expansion shape from the issue.
  (should-equal (macroexpand-1 '(thread-first x (foo y) (bar z) (baz w)))
                '(baz (bar (foo x y) z) w)))

(defmacro thread-last (x &rest forms)
  "Thread-last macro, as in Clojure's ->>.

Inserts X as the last argument of each of FORMS, nesting left-to-right:

  (thread-last x (foo y) (bar z) (baz w))
  => (baz w (bar z (foo y x)))

A non-list form F is treated as (F).  With no FORMS, expands to X.
The macro ->> is an alias for THREAD-LAST."
  (reduce (lambda (acc form)
            (let ((form (if (listp form) form (list form))))
              `(,@form ,acc)))
          forms
          :initial-value x))

(behavior 'thread-last
  (should= 5 (thread-last 5))
  (should= 6 (thread-last 5 1+))
  (should= 6 (thread-last 5 (1+)))
  (should-equal '(2 4 6)
                (thread-last '(1 2 3 4 5)
                  (mapcar #'1+)
                  (remove-if-not #'evenp)))
  (should= 9
           (thread-last 2
             (* 3)
             (+ 3)))
  (should-equal '(b a c)
                (thread-last (list 'a 'b)
                  (cons 'c)
                  (reverse)))
  (should-equal (macroexpand-1 '(thread-last x (foo y) (bar z) (baz w)))
                '(baz w (bar z (foo y x)))))

(defmacro thread-as (expr name &rest forms)
  "Named threading macro, as in Clojure's as->.

Binds NAME to EXPR, evaluates the first of FORMS in that lexical environment,
rebinds NAME to that result, and repeats for each successive form.  Unlike
THREAD-FIRST / THREAD-LAST, NAME may appear anywhere in each form.  Unlike
A?AND / AAND, there is no short-circuit on NIL — every form is evaluated.

  (thread-as 0 n
    (1+ n)
    (* n 2)
    (+ n 5))
  => 7

With no FORMS, expands to EXPR.  The macro AS-> is an alias for THREAD-AS."
  (if (null forms)
      expr
      `(let ((,name ,expr))
         (thread-as ,(first forms) ,name ,@(rest forms)))))

(behavior 'thread-as
  (should= 0 (thread-as 0 n))
  (should= 7
           (thread-as 0 n
             (1+ n)
             (* n 2)
             (+ n 5)))
  (should-equal "oo"
                (thread-as "foo" s
                  (subseq s 1)
                  (string-upcase s)
                  (string-downcase s)))
  ;; Continues through NIL (unlike A?AND).
  (should= 1
           (thread-as nil n
             (or n 0)
             (1+ n)))
  (should-equal '(3 2 1)
                (thread-as (list 1 2 3) xs
                  (reverse xs)
                  (copy-list xs)))
  ;; NAME can sit in the middle of a form.
  (should-equal '(a X b)
                (thread-as 'X v
                  (list 'a v 'b))))

(defmacro thread-some-first (expr &rest forms)
  "Like THREAD-FIRST, but if any intermediate value is NIL, stop and return NIL
without evaluating further forms.  As in Clojure's some->.
The macro SOME-> is an alias for THREAD-SOME-FIRST."
  (reduce (lambda (acc form)
            (let ((g (gensym "SOME"))
                  (form (if (listp form) form (list form))))
              `(let ((,g ,acc))
                 (when ,g
                   (,(first form) ,g ,@(rest form))))))
          forms
          :initial-value expr))

(behavior 'thread-some-first
  (should= 5 (thread-some-first 5))
  (should= 6 (thread-some-first 5 1+))
  (should-be-null (thread-some-first nil 1+))
  (should-be-null
   (thread-some-first (cons 1 nil)
     (cdr)
     (car)
     (1+)))
  (should= 2
           (thread-some-first (cons 1 (cons 2 nil))
             (cdr)
             (car)))
  (should= 11
           (thread-some-first 10
             (1+)
             identity)))

(defmacro thread-some-last (expr &rest forms)
  "Like THREAD-LAST, but if any intermediate value is NIL, stop and return NIL
without evaluating further forms.  As in Clojure's some->>.
The macro SOME->> is an alias for THREAD-SOME-LAST."
  (reduce (lambda (acc form)
            (let ((g (gensym "SOME"))
                  (form (if (listp form) form (list form))))
              `(let ((,g ,acc))
                 (when ,g
                   (,@form ,g)))))
          forms
          :initial-value expr))

(behavior 'thread-some-last
  (should= 5 (thread-some-last 5))
  (should= 6 (thread-some-last 5 1+))
  (should-be-null (thread-some-last nil reverse))
  (should-be-null
   (thread-some-last nil
     (cons 1)
     (cons 2)))
  (should-equal '(3 2 1)
                (thread-some-last '(1 2 3)
                  (copy-list)
                  (reverse)))
  (should-equal '(2 4)
                (thread-some-last '(1 2 3 4)
                  (mapcar #'1+)
                  (remove-if-not #'evenp))))

(defmacro thread-cond-first (expr &rest clauses)
  "Conditional thread-first, as in Clojure's cond->.

CLAUSES are alternating TESTs and FORMs.  Starting from EXPR, for each pair:
if TEST is true, thread the current value through FORM as in THREAD-FIRST;
otherwise skip FORM.  Tests are always evaluated; a false test does not abort
the remaining clauses.  The macro COND-> is an alias for THREAD-COND-FIRST."
  (assert (evenp (length clauses)) (clauses)
          "THREAD-COND-FIRST expects an even number of clause elements (test form).")
  (let ((g (gensym "COND")))
    `(let ((,g ,expr))
       ,@(loop for (test form) on clauses by #'cddr
               for f = (if (listp form) form (list form))
               collect `(when ,test
                          (setf ,g (,(first f) ,g ,@(rest f)))))
       ,g)))

(behavior 'thread-cond-first
  (should= 1 (thread-cond-first 1))
  (should= 6
           (thread-cond-first 1
             t 1+
             t (* 3)
             nil (* 100)))
  (should= 1
           (thread-cond-first 1
             nil 1+
             nil (* 3)))
  (should= 11
           (thread-cond-first 10
             (> 5 3) 1+
             t identity))
  (should-equal '(1 2 3)
                (thread-cond-first nil
                  t (or '(1 2 3))
                  t (copy-list))))

(defmacro thread-cond-last (expr &rest clauses)
  "Conditional thread-last, as in Clojure's cond->>.

CLAUSES are alternating TESTs and FORMs.  Starting from EXPR, for each pair:
if TEST is true, thread the current value through FORM as in THREAD-LAST;
otherwise skip FORM.  Tests are always evaluated; a false test does not abort
the remaining clauses.  The macro COND->> is an alias for THREAD-COND-LAST."
  (assert (evenp (length clauses)) (clauses)
          "THREAD-COND-LAST expects an even number of clause elements (test form).")
  (let ((g (gensym "COND")))
    `(let ((,g ,expr))
       ,@(loop for (test form) on clauses by #'cddr
               for f = (if (listp form) form (list form))
               collect `(when ,test
                          (setf ,g (,@f ,g))))
       ,g)))

(behavior 'thread-cond-last
  (should= 1 (thread-cond-last 1))
  (should-equal '(2 4 6)
                (thread-cond-last '(1 2 3 4 5)
                  t (mapcar #'1+)
                  t (remove-if-not #'evenp)
                  nil (cons 0)))
  (should-equal '(1 2 3)
                (thread-cond-last '(1 2 3)
                  nil reverse
                  nil (cons 0)))
  (should-equal '(c a b)
                (thread-cond-last '(a b)
                  t (cons 'c)
                  nil reverse)))

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

(behavior 'conjoin
          (flet ((%2? (i)
                   (zerop (mod i 2)))
                 (%3? (i)
                   (zerop (mod i 3))))
            (loop for i from 1 to 100
               do (should-eq (and (%2? i) (%3? i))
                             (funcall (conjoin #'%2? #'%3?) i)))))

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

(behavior 'curry
          (loop for i from 1 to 100
             do (should= (funcall (curry #'+ 1 2) i)
                         (+ 1 2 i))))

(defmacro defconstant-once (name value &optional docstring)
  "Define NAME as a constant with VALUE, but only if it is not already bound.

Unlike wrapping DEFCONSTANT in UNLESS, this expands to a real top-level
DEFCONSTANT (inside EVAL-WHEN) so the compiler can see NAME when compiling
later forms in the same file.  On reload, the value form returns the existing
binding so the constant is not redefined inconsistently.  Documentation is set
only when NAME has no variable documentation yet, so reloads do not overwrite
an existing docstring.

NAME should be a symbol (typically in +plus-signs+ convention).
VALUE is evaluated once, when the constant is first defined.
DOCSTRING is an optional documentation string."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name
       (if (boundp ',name)
           (symbol-value ',name)
           ,value))
     ,@(when docstring
         `((unless (documentation ',name 'variable)
             (setf (documentation ',name 'variable) ,docstring))))))

(let ((unique-name (gentemp "TEST-CONSTANT-")))
  (eval `(defconstant-once ,unique-name 42 "A test constant."))

  (should-be-true (boundp unique-name))
  (should= (symbol-value unique-name) 42)
  (should-equalp (documentation unique-name 'variable) "A test constant.")
  )

(behavior 'defconstant-once
  "Tests for the defconstant-once macro."

  (spec "defines only on first call and ignores subsequent calls"
    (let ((unique-name (gentemp "TEST-CONSTANT-")))
      ;; First definition
      (eval `(defconstant-once ,unique-name 42 "A test constant."))

      (should-be-true (boundp unique-name))
      (should= (symbol-value unique-name) 42)
      (should-equalp (documentation unique-name 'variable) "A test constant.")

      ;; Second call should be ignored (no redefinition)
      (eval `(defconstant-once ,unique-name 999 "This should be ignored."))

      (should= (symbol-value unique-name) 42)        ; value unchanged
      (should-equalp (documentation unique-name 'variable) "A test constant."))))

#-cmu
(defmacro deletef (item sequence &rest rest)
  "The DELETEF macro deletes ITEM from SEQUENCE in-place."
  `(setf ,sequence
         (delete ,item ,sequence ,@rest)))

(behavior 'deletef
  (let ((l '(a b c d e f g)))
    (deletef 'd l)
    (should-equalp l '(a b c e f g))))

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

(behavior 'disjoin
          (flet ((%2? (i)
                   (zerop (mod i 2)))
                 (%3? (i)
                   (zerop (mod i 3))))
            (loop for i from 1 to 100
               do (should-eq (or (%2? i) (%3? i))
                             (funcall (disjoin #'%2? #'%3?) i)))))

(defgeneric duplicate (item)
  (:documentation
   "Return a deep copy of ITEM.  Composite structures such as lists and arrays
are recursively duplicated; atomic values such as numbers, symbols, and
functions are returned as-is."))

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
  "Numbers are immutable, so this returns NUMBER unchanged."
  number)

(defmethod duplicate ((symbol symbol))
  "Symbols are not copied; this returns SYMBOL unchanged."
  symbol)

(defmethod duplicate ((function function))
  "Functions are not copied; this returns FUNCTION unchanged.
XXX: I believe this is correct, but I am not really sure."
  function)

(behavior 'duplicate
  (should= 42 (duplicate 42))
  (should-eq 'foo (duplicate 'foo))
  (let* ((orig (list 1 (list 2 3) 4))
         (copy (duplicate orig)))
    (should-equal copy '(1 (2 3) 4))
    (should-not-eq orig copy)
    (should-not-eq (second orig) (second copy))
    (setf (first (second copy)) 99)
    (should-equal orig '(1 (2 3) 4)))
  (let* ((orig (make-array '(2 2) :initial-contents '((1 2) (3 4))))
         (copy (duplicate orig)))
    (should-equalp copy #2A((1 2) (3 4)))
    (should-not-eq orig copy)
    (setf (aref copy 0 0) 0)
    (should= 1 (aref orig 0 0)))
  (let ((f #'identity))
    (should-eq f (duplicate f))))

(defmacro for (initial conditional step-action &body body)
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

(defmacro forever (&body body)
  "The FOREVER macro is just a way to say (while t ...) with a bit of added
expressiveness and explicitness."
  `(while t ,@body))

(defmacro macro-alias (macro &rest aliases)
  "This produces one or more aliases (alternate names) for a macro."
  `(progn
     ,@(mapcar (lambda (a)
                 `(setf (macro-function ',a) (macro-function ',macro)))
               aliases)))

(behavior 'macro-alias
  ;; Alias is installed at load time, so call sites must EVAL (they cannot be
  ;; compiled as ordinary function calls before the macro exists).
  (macro-alias when macro-alias-behavior-when)
  (should-be-true (macro-function 'macro-alias-behavior-when))
  (should= 2 (eval '(macro-alias-behavior-when t 2)))
  (should-be-null (eval '(macro-alias-behavior-when nil 2))))

(defun function-alias-as-a-function (function &rest aliases)
  "This produces one or more aliases (alternate names) for a function.
For example, you might do something like:

> (function-alias-as-a-function 'that-guy-doesnt-know-when-to-stop-typing 'shorter)

This is the older DEFUN variant, the newer variant is a DEFMACRO below."
  (loop for alias in aliases
        do (setf (fdefinition alias) (fdefinition function))))

(function-alias-as-a-function 'function-alias-as-a-function
                              'function-aliases-as-a-function)

(behavior 'function-alias-as-a-function
  (flet ((sample (x) (* x 3)))
    (setf (fdefinition 'faafa-behavior-src) #'sample)
    (function-alias-as-a-function 'faafa-behavior-src
                                  'faafa-behavior-a
                                  'faafa-behavior-b)
    (should-be-true (fboundp 'faafa-behavior-a))
    (should-be-true (fboundp 'faafa-behavior-b))
    (should-eq (fdefinition 'faafa-behavior-a)
               (fdefinition 'faafa-behavior-src))
    (should-eq (fdefinition 'faafa-behavior-b)
               (fdefinition 'faafa-behavior-src))
    (should= 9 (faafa-behavior-a 3))
    (should= 9 (faafa-behavior-b 3))
    (fmakunbound 'faafa-behavior-src)
    (fmakunbound 'faafa-behavior-a)
    (fmakunbound 'faafa-behavior-b)))

(behavior 'function-aliases-as-a-function
  (should-eq (fdefinition 'function-aliases-as-a-function)
             (fdefinition 'function-alias-as-a-function)))

(defmacro function-alias (function &rest aliases)
  "This produces one or more aliases (alternate names) for a function.
For example, you might do something like:

> (function-alias 'that-guy-doesnt-know-when-to-stop-typing 'shorter)
> (function-alias 'long-name 'short-a 'short-b)"
  (let* ((the-function (if (consp function)
                           (second function)
                           function))
         (the-aliases (mapcar #'(lambda (a)
                                  (if (consp a)
                                      (second a)
                                      a))
                              aliases)))
    `(progn
       ,@(mapcar #'(lambda (a) `(declaim (ftype function ,a))) the-aliases)
       ,@(mapcar #'(lambda (a)
                     `(setf (fdefinition ',a)
                            (fdefinition ',the-function)))
                 the-aliases))))

(behavior 'function-alias
  (flet ((sample (x) (+ x 1)))
    (setf (fdefinition 'function-alias-behavior-sample) #'sample)
    (function-alias 'function-alias-behavior-sample
                    'function-alias-behavior-a
                    'function-alias-behavior-b)
    (should-be-true (fboundp 'function-alias-behavior-a))
    (should-be-true (fboundp 'function-alias-behavior-b))
    (should-eq (fdefinition 'function-alias-behavior-a)
               (fdefinition 'function-alias-behavior-sample))
    (should-eq (fdefinition 'function-alias-behavior-b)
               (fdefinition 'function-alias-behavior-sample))
    (should= (function-alias-behavior-a 41) 42)
    (should= (function-alias-behavior-b 41) 42)
    (fmakunbound 'function-alias-behavior-sample)
    (fmakunbound 'function-alias-behavior-a)
    (fmakunbound 'function-alias-behavior-b)))

(macro-alias function-alias function-aliases)

(function-alias 'juxtapose 'juxt)

(behavior 'juxt
  (should-eq (fdefinition 'juxt) (fdefinition 'juxtapose))
  (should-equal (funcall (juxt #'1+ #'1-) 10) '(11 9)))

(macro-alias thread-first ->)

(behavior '->
  ;; -> is installed at load time via MACRO-ALIAS, so call sites here must
  ;; EVAL (they cannot be compiled as ordinary function calls).
  (should-eq (macro-function '->) (macro-function 'thread-first))
  (should= 9 (eval '(-> 2 (* 3) (+ 3))))
  (should-equal (macroexpand-1 '(-> x (foo y) (bar z) (baz w)))
                '(baz (bar (foo x y) z) w)))

(macro-alias thread-last ->>)

(behavior '->>
  (should-eq (macro-function '->>) (macro-function 'thread-last))
  (should-equal '(2 4 6)
                (eval '(->> '(1 2 3 4 5)
                            (mapcar #'1+)
                            (remove-if-not #'evenp))))
  (should-equal (macroexpand-1 '(->> x (foo y) (bar z) (baz w)))
                '(baz w (bar z (foo y x)))))

(macro-alias thread-as as->)

(behavior 'as->
  (should-eq (macro-function 'as->) (macro-function 'thread-as))
  (should= 7 (eval '(as-> 0 n (1+ n) (* n 2) (+ n 5))))
  (should= 1 (eval '(as-> nil n (or n 0) (1+ n)))))

(macro-alias thread-some-first some->)

(behavior 'some->
  (should-eq (macro-function 'some->) (macro-function 'thread-some-first))
  (should-be-null (eval '(some-> nil 1+)))
  (should= 6 (eval '(some-> 5 1+))))

(macro-alias thread-some-last some->>)

(behavior 'some->>
  (should-eq (macro-function 'some->>) (macro-function 'thread-some-last))
  (should-be-null (eval '(some->> nil reverse)))
  (should-equal '(3 2 1)
                (eval '(some->> '(1 2 3) (copy-list) (reverse)))))

(macro-alias thread-cond-first cond->)

(behavior 'cond->
  (should-eq (macro-function 'cond->) (macro-function 'thread-cond-first))
  (should= 6 (eval '(cond-> 1 t 1+ t (* 3) nil (* 100)))))

(macro-alias thread-cond-last cond->>)

(behavior 'cond->>
  (should-eq (macro-function 'cond->>) (macro-function 'thread-cond-last))
  (should-equal '(2 4 6)
                (eval '(cond->> '(1 2 3 4 5)
                         t (mapcar #'1+)
                         t (remove-if-not #'evenp)
                         nil (cons 0)))))

(defmacro multicond (&rest clauses)
  "A macro much like COND, but where multiple clauses may be evaluated."
  (let ((whens '()))
    (mapcar #'(lambda (clause)
                (let ((conditional (first clause))
                      (body (rest clause)))
                  (push `(when ,conditional ,@body) whens)))
            clauses)
    (setf whens (nreverse whens))
    `(progn ,@whens)))

(behavior 'multicond
  (should-equalp '(positive even)
                 (let ((result '())
                       (x 12))
                   (multicond ((oddp x)  (push 'odd      result))
                              ((evenp x) (push 'even     result))
                              ((< x 0)   (push 'negative result))
                              ((< 0 x)   (push 'positive result)))
                   result)))

(defun operator-to-function (operator)
  "The OPERATOR-TO-FUNCTION function takes in any symbol and makes an
evaluatable function out of it.  The principle purpose for this is so that we
can treat macros and other non-function things like a function, for using them
with MAPCAR or similar."
  (lambda (&rest rest)
    (eval `(,operator ,@rest))))

(behavior 'operator-to-function
  (should= 6 (funcall (operator-to-function '+) 1 2 3))
  (should-equal '(1 2 3)
                (mapcar (operator-to-function '1+) '(0 1 2))))

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

(behavior 'opf
  (let ((x 0))
    (opf #'+ x 10)
    (should= x 10)
    (opf #'- x 10)
    (should= x 0))
  (let ((x 42))
    (should= (opf #'+ x 1) 43)))

(defmacro fop (operator variable &rest arguments)
  "FOP is like the OPF macro, but as a post-assignment variant.  The difference
is similar to the difference between x++ and ++x in the C Programming Language,
with opf being like ++x and fop being like x++."
  `(prog1 ,variable
     (setf ,variable
           (funcall ,operator ,variable ,@arguments))))

(behavior 'fop
  (let ((x 0))
    (fop #'+ x 10)
    (should= x 10)
    (fop #'- x 10)
    (should= x 0))
  (let ((x 42))
    (should= (fop #'+ x 1) 42)))

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

(behavior 'rcurry
          (loop for i from 1 to 100
             do (should= (funcall (rcurry #'- 1 2) i)
                         (- i 1 2))))

(defmacro swap (x y)
  "A simple SWAP macro.  The values of the first form and the second form are
swapped with each other."
  `(psetf ,x ,y
          ,y ,x))

(behavior 'swap
          (let ((x 15)
                (y 37))
            (swap x y)
            (should= y 15)
            (should= x 37)))

(defmacro swap-unless (predicate x y)
  "This macro calls SWAP unless the predicate evaluates to true."
  `(unless (funcall ,predicate ,x ,y)
     (swap ,x ,y)))

(behavior 'swap-unless
          (let ((smaller 1)
                (larger 2))
            (swap-unless #'< smaller larger)
            (should= smaller 1)
            (should= larger 2)))

(defmacro swap-when (predicate x y)
  "This macro calls SWAP only when the predicate evaluates to true."
  `(when (funcall ,predicate ,x ,y)
     (swap ,x ,y)))

(behavior 'swap-when
          (let ((smaller 2)
                (larger 1))
            (swap-when #'> smaller larger)
            (should= smaller 1)
            (should= larger 2)))

(defun unimplemented ()
  "Signal an error indicating that this code path is not yet implemented."
  (error "This is not yet implemented."))

(behavior 'unimplemented
  (handler-case
      (progn (unimplemented)
             (should-be-true nil)) ; must not reach here
    (error (e)
      (should-be-true
       (search "not yet implemented" (princ-to-string e)
               :test #'char-equal)))))

(behavior 'it
  ;; The default anaphor used by AIF / AWHEN / etc.
  (should= 42 (aif (+ 40 2) it))
  (should-be-null (aif nil it))
  (should= 10 (awhen 10 it)))

(behavior 'self
  ;; The recursive name bound by ALAMBDA.
  (should= 120
           (funcall (alambda (n)
                      (if (<= n 1)
                          1
                          (* n (self (1- n)))))
                    5))
  (should= 0
           (funcall (alambda (n)
                      (if (zerop n)
                          0
                          (1+ (self (1- n)))))
                    0)))

(defmacro while (conditional &body body)
  "A WHILE macro, similar to the while loop in C.  Returns the value of the
last form in BODY from the last iteration, or NIL if BODY never runs."
  (let ((result (gensym "RESULT")))
    `(let ((,result nil))
       (do ()
           ((not ,conditional) ,result)
         (setf ,result (progn ,@body))))))

(behavior 'while
          (let ((x 0))
            (should= 10 (while (< x 10)
                          (incf x)))
            (should= x 10))
          (let ((x 0))
            (should= 30 (while (< x 3)
                          (incf x)
                          (* x 10))))
          (should-be-null (while nil t)))

(defmacro do-while (conditional &body body)
  "The DO-WHILE macro operates like a do {BODY} while (CONDITIONAL) in the C
  programming language.  Returns the value of the last form in BODY from the
  last iteration (BODY always runs at least once)."
  (let ((result (gensym "RESULT")))
    `(let ((,result (progn ,@body)))
       (while ,conditional
         (setf ,result (progn ,@body)))
       ,result)))

(behavior 'do-while
          (let ((i 100))
            (should= -1 (do-while (<= 0 i) (decf i)))
            (should= i -1))
          (should= 1 (let ((i 0))
                       (do-while (< i 1) (incf i)))))

(defmacro do-until (conditional &body body)
  "A DO-UNTIL loop construct; it operates like do {BODY} while (! CONDITIONAL)
construct in the C programming language.  Returns the value of the last form
in BODY from the last iteration (BODY always runs at least once)."
  `(do-while (not ,conditional)
     ,@body))

(behavior 'do-until
          (let ((i 100))
            (should= 0 (do-until (<= i 0) (decf i)))
            (should= i 0)))

(defmacro until (conditional &body body)
  "An UNTIL loop construct.  It operates in the negative sense as WHILE.
Returns the value of the last form in BODY from the last iteration, or NIL if
BODY never runs."
  `(while (not ,conditional)
     ,@body))

(behavior 'until
          (let ((x 0))
            (should= 10 (until (<= 10 x)
                          (incf x)))
            (should= x 10))
          (should-be-null (until t t)))

;; FOR and FOREVER expand to WHILE; keep their behaviors after WHILE is defined.
(behavior 'for
  (let ((sum 0))
    (for ((i 0))
         (< i 5)
         (incf i)
      (incf sum i))
    ;; (prog1 body step): body runs with i = 0..4; sum = 0+1+2+3+4 = 10.
    (should= 10 sum))
  (let ((xs nil))
    (for ((i 1))
         (<= i 3)
         (incf i)
      (push i xs))
    (should-equal (reverse xs) '(1 2 3))))

(behavior 'forever
  (let ((n 0))
    (block done
      (forever
        (incf n)
        (when (>= n 5)
          (return-from done))))
    (should= 5 n)))
