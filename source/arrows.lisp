;;;; Copyright (C) 2005 -- 2026, Christopher Mark Gore,
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

(defpackage :sigma/arrows
  (:documentation
   "Threading / arrow macros for Common Lisp, including a port of
https://github.com/rplevy/swiss-arrows (Robert P. Levy et al.).

English names (thread-diamond-first, thread-branch-last, …) are primary.
Swiss-arrows-style symbols (-<>, -<, …) are aliases via MACRO-ALIAS, matching
sigma/control's THREAD-FIRST / -> pattern.

Uses sigma/control THREAD-FIRST (->) and THREAD-LAST (->>) for traditional
threading.

Note: swiss-arrows parallel variants are named -<:p etc.  In Common Lisp a
colon inside a symbol name is awkward, so the symbolic aliases are -<-p,
-<<-p, -<><-p, -<>><-p (hyphen before p).")
  (:use :common-lisp
        :sigma/behave
        :sigma/control)
  (:export :thread-diamond-first
           :thread-diamond-last
           :thread-some-diamond-first
           :thread-some-diamond-last
           :thread-last-reverse
           :thread-do-first
           :thread-do-last
           :thread-do-diamond-first
           :thread-do-diamond-last
           :thread-apply-first
           :thread-apply-last
           :thread-branch-first
           :thread-branch-first-parallel
           :thread-branch-last
           :thread-branch-last-parallel
           :thread-branch-diamond-first
           :thread-branch-diamond-first-parallel
           :thread-branch-diamond-last
           :thread-branch-diamond-last-parallel
           ;; Swiss-arrows symbolic aliases
           :-<>
           :-<>>
           :some-<>
           :some-<>>
           :<<-
           :-!>
           :-!>>
           :-!<>
           :-!<>>
           :apply->
           :apply->>
           :-<
           :-<-p
           :-<<
           :-<<-p
           :-<><
           :-<><-p
           :-<>><
           :-<>><-p
           ;; Hole marker
           :<>))
(in-package :sigma/arrows)

;;;; ---------------------------------------------------------------------------
;;;; Diamond insert helper (hole marker is the symbol <>)
;;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %top-level-diamond-count (form)
    "Count <> only at the top level of FORM (list or vector elements)."
    (cond ((and (listp form) (not (null form)))
           (count '<> form :test #'eq))
          ((and (vectorp form) (not (stringp form)))
           (loop for e across form count (eq e '<>)))
          (t 0)))

  (defun %diamond-expand (form x default-position)
    "Insert X into FORM per swiss-arrows diamond rules.
DEFAULT-POSITION is :FIRST or :LAST when no <> is present."
    (cond
      ((or (symbolp form) (keywordp form))
       `(,form ,x))
      ((and (vectorp form) (not (stringp form)))
       (let ((c (%top-level-diamond-count form)))
         (cond ((> c 1)
                (error "No more than one <> position per form is allowed."))
               ((= c 0)
                (if (eq default-position :first)
                    `(concatenate 'vector (vector ,x) ,form)
                    `(concatenate 'vector ,form (vector ,x))))
               (t
                `(vector ,@(map 'list (lambda (e) (if (eq e '<>) x e)) form))))))
      ((and (listp form) (not (null form)))
       (let ((c (%top-level-diamond-count form)))
         (cond ((> c 1)
                (error "No more than one <> position per form is allowed."))
               ((= c 0)
                (if (eq default-position :first)
                    `(,(first form) ,x ,@(rest form))
                    `(,(first form) ,@(rest form) ,x)))
               (t
                `(,(first form)
                  ,@(mapcar (lambda (e) (if (eq e '<>) x e))
                            (rest form)))))))
      (t form))))

;;;; ---------------------------------------------------------------------------
;;;; Diamond first / last
;;;; ---------------------------------------------------------------------------

(defmacro thread-diamond-first (x &rest forms)
  "Thread X through FORMS, inserting at a top-level <> hole when present;
otherwise as the first argument (thread-first style).  Also supports vectors.
The macro -<> is an alias for THREAD-DIAMOND-FIRST (swiss-arrows diamond wand)."
  (cond ((null forms) x)
        ((null (rest forms))
         (%diamond-expand (first forms) x :first))
        (t `(thread-diamond-first
             (thread-diamond-first ,x ,(first forms))
             ,@(rest forms)))))

(defmacro thread-diamond-last (x &rest forms)
  "Like THREAD-DIAMOND-FIRST, but default insertion is the last argument
(thread-last style).  The macro -<>> is an alias for THREAD-DIAMOND-LAST
(swiss-arrows diamond spear)."
  (cond ((null forms) x)
        ((null (rest forms))
         (%diamond-expand (first forms) x :last))
        (t `(thread-diamond-last
             (thread-diamond-last ,x ,(first forms))
             ,@(rest forms)))))

(behavior 'thread-diamond-first
  ;; Zero forms: identity.
  (should= 42 (thread-diamond-first 42))
  (should-equal '(a) (thread-diamond-first (list 'a)))
  ;; Single form without hole: first-arg default.
  (should= 1 (thread-diamond-first (first '(1 2 3))))
  (should-equal '(0 1 2 3) (thread-diamond-first 0 (list 1 2 3)))
  (should-equalp #(0 1 2 3) (thread-diamond-first 0 #(1 2 3)))
  (should-equal '(4 1 2 3) (thread-diamond-first 4 (cons '(1 2 3))))
  ;; Hole placement (list and vector).
  (should-equal '(a X b) (thread-diamond-first 'X (list 'a <> 'b)))
  (should-equalp #(1 2 10 4 5)
                 (thread-diamond-first 10 #(1 2 <> 4 5)))
  ;; Multi-step pipeline with bare symbol step and hole.
  (should-equalp #(1 2 0 3 4)
                 (thread-diamond-first 0
                   (* <> 5)
                   (vector 1 2 <> 3 4)))
  (should-equal '(4 3 2 5)
                (thread-diamond-first 4
                  (cons '(1 2 3))
                  reverse
                  (mapcar #'1+ <>)))
  ;; Nested <> inside a subform is not a top-level hole; default insert first.
  (should-equal '(0 (<> 9))
                (thread-diamond-first 0 (list (list '<> 9))))
  ;; More than one top-level <> is an error.
  (handler-case
      (progn (eval '(thread-diamond-first 1 (list <> <>)))
             (should-be-true nil))
    (error (e)
      (should-be-true
       (search "No more than one" (princ-to-string e) :test #'char-equal))))
  (handler-case
      (progn (eval '(thread-diamond-first 1 #(<> <>)))
             (should-be-true nil))
    (error (e)
      (should-be-true
       (search "No more than one" (princ-to-string e) :test #'char-equal)))))

(behavior 'thread-diamond-last
  ;; Zero forms: identity.
  (should= 7 (thread-diamond-last 7))
  ;; Default last insert (list and vector).
  (should-equal '(1 2 3 0) (thread-diamond-last 0 (list 1 2 3)))
  (should-equalp #(1 2 3 0) (thread-diamond-last 0 #(1 2 3)))
  ;; Explicit hole (including non-last position).
  (should-equal '(1 2 3 4) (thread-diamond-last 4 (list 1 2 3 <>)))
  (should-equal '(1 4 2 3) (thread-diamond-last 4 (list 1 <> 2 3)))
  ;; Multi-step.
  (should-equal '(5 4 3 2)
                (thread-diamond-last 4
                  (list 1 2 3 <>)
                  reverse
                  (mapcar #'1+ <>)))
  ;; Nested <> is not a hole; default insert last.
  (should-equal '((<> 9) 0)
                (thread-diamond-last 0 (list (list '<> 9))))
  ;; Multi-hole error.
  (handler-case
      (progn (eval '(thread-diamond-last 1 (list <> 2 <>)))
             (should-be-true nil))
    (error (e)
      (should-be-true
       (search "No more than one" (princ-to-string e) :test #'char-equal)))))

(macro-alias thread-diamond-first -<>)
(macro-alias thread-diamond-last -<>>)

(behavior '-<>
  (should-eq (macro-function '-<>) (macro-function 'thread-diamond-first))
  (should-equal '(0 1 2 3) (eval '(-<> 0 (list 1 2 3))))
  (should-equal '(a X b) (eval '(-<> 'X (list 'a <> 'b))))
  (should= 42 (eval '(-<> 42))))

(behavior '-<>>
  (should-eq (macro-function '-<>>) (macro-function 'thread-diamond-last))
  (should-equal '(1 2 3 0) (eval '(-<>> 0 (list 1 2 3))))
  (should-equal '(1 9 2) (eval '(-<>> 9 (list 1 <> 2)))))

;;;; ---------------------------------------------------------------------------
;;;; Nil-shortcutting diamond
;;;; ---------------------------------------------------------------------------

(defmacro thread-some-diamond-first (x &rest forms)
  "Like THREAD-DIAMOND-FIRST, but if any intermediate value is NIL, stop and
return NIL.  The macro SOME-<> is an alias for THREAD-SOME-DIAMOND-FIRST."
  (if (null forms)
      x
      (let ((i (gensym "I")))
        `(let ((,i ,x))
           (when ,i
             (thread-some-diamond-first
              (thread-diamond-first ,i ,(first forms))
              ,@(rest forms)))))))

(defmacro thread-some-diamond-last (x &rest forms)
  "Like THREAD-DIAMOND-LAST, but if any intermediate value is NIL, stop and
return NIL.  The macro SOME-<>> is an alias for THREAD-SOME-DIAMOND-LAST."
  (if (null forms)
      x
      (let ((i (gensym "I")))
        `(let ((,i ,x))
           (when ,i
             (thread-some-diamond-last
              (thread-diamond-last ,i ,(first forms))
              ,@(rest forms)))))))

(behavior 'thread-some-diamond-first
  ;; Zero forms: identity (including nil seed).
  (should= 5 (thread-some-diamond-first 5))
  (should-be-null (thread-some-diamond-first nil))
  ;; Nil seed short-circuits without evaluating further forms.
  (should-be-null (thread-some-diamond-first nil (list <> 1)))
  ;; Mid-chain nil short-circuits.
  (should-be-null
   (thread-some-diamond-first "abc"
     (if (stringp "adf") nil <>)
     (concatenate 'string <> " + more")))
  ;; Success path.
  (should-string=
   (thread-some-diamond-first "abc"
     (if (stringp "adf") "some" <>)
     (concatenate 'string <> " + more"))
   "some + more")
  ;; Default first insert still works when non-nil.
  (should-equal '(9 1 2)
                (thread-some-diamond-first 9 (list 1 2))))

(behavior 'thread-some-diamond-last
  (should= 3 (thread-some-diamond-last 3))
  (should-be-null (thread-some-diamond-last nil))
  (should-be-null
   (thread-some-diamond-last "abc"
     (if (stringp "adf") nil <>)
     (concatenate 'string <> "+more")))
  (should-string=
   (thread-some-diamond-last "abc"
     (if t "some" <>)
     (concatenate 'string <> "+more"))
   "some+more")
  (should-equal '(1 2 9)
                (thread-some-diamond-last 9 (list 1 2))))

(macro-alias thread-some-diamond-first some-<>)
(macro-alias thread-some-diamond-last some-<>>)

(behavior 'some-<>
  (should-eq (macro-function 'some-<>)
             (macro-function 'thread-some-diamond-first))
  (should-be-null (eval '(some-<> nil (list <> 1))))
  (should-string=
   (eval '(some-<> "abc"
            (if t "some" <>)
            (concatenate 'string <> " + more")))
   "some + more")
  (should= 5 (eval '(some-<> 5))))

(behavior 'some-<>>
  (should-eq (macro-function 'some-<>>)
             (macro-function 'thread-some-diamond-last))
  (should-be-null (eval '(some-<>> nil (list 1 <>))))
  (should-string=
   (eval '(some-<>> "abc"
            (if t "some" <>)
            (concatenate 'string <> "+more")))
   "some+more")
  (should-equal '(1 2 9) (eval '(some-<>> 9 (list 1 2)))))

;;;; ---------------------------------------------------------------------------
;;;; Reverse thread-last (back arrow)
;;;; ---------------------------------------------------------------------------

(defmacro thread-last-reverse (&rest forms)
  "Like THREAD-LAST / ->>, but FORMS are written in reverse order (last form
is the seed).  The macro <<- is an alias for THREAD-LAST-REVERSE
(swiss-arrows back arrow)."
  `(thread-last ,@(reverse forms)))

(behavior 'thread-last-reverse
  ;; Single form is just that form.
  (should= 42 (thread-last-reverse 42))
  ;; Forms written bottom-up: last form is seed for thread-last.
  (should-equal '(3 2 1)
                (thread-last-reverse
                 (reverse)
                 (list 1 2 3)))
  (should-equal 'blah
                (thread-last-reverse
                 (let ((x 'nonsense)) x)
                 (if (not x) 'foo
                     (let ((more 'blah)) more))))
  ;; Written bottom-up: seed, then 1+, then keep evens => (2 4 6).
  (should-equal '(2 4 6)
                (thread-last-reverse
                 (remove-if-not #'evenp)
                 (mapcar #'1+)
                 (list 1 2 3 4 5))))

(macro-alias thread-last-reverse <<-)

(behavior '<<-
  (should-eq (macro-function '<<-) (macro-function 'thread-last-reverse))
  (should-equal '(3 2 1)
                (eval '(<<- (reverse) (list 1 2 3))))
  (should= 42 (eval '(<<- 42))))

;;;; ---------------------------------------------------------------------------
;;;; Non-updating (side effects only; return original value)
;;;; ---------------------------------------------------------------------------

(defmacro thread-do-first (form &rest forms)
  "Run FORM through FORMS as THREAD-FIRST for side effects only; return the
value of FORM.  The macro -!> is an alias for THREAD-DO-FIRST."
  (let ((x (gensym "X")))
    `(let ((,x ,form))
       (thread-first ,x ,@forms)
       ,x)))

(defmacro thread-do-last (form &rest forms)
  "Run FORM through FORMS as THREAD-LAST for side effects only; return the
value of FORM.  The macro -!>> is an alias for THREAD-DO-LAST."
  (let ((x (gensym "X")))
    `(let ((,x ,form))
       (thread-last ,x ,@forms)
       ,x)))

(defmacro thread-do-diamond-first (form &rest forms)
  "Run FORM through FORMS as THREAD-DIAMOND-FIRST for side effects only;
return the value of FORM.  The macro -!<> is an alias for
THREAD-DO-DIAMOND-FIRST."
  (let ((x (gensym "X")))
    `(let ((,x ,form))
       (thread-diamond-first ,x ,@forms)
       ,x)))

(defmacro thread-do-diamond-last (form &rest forms)
  "Run FORM through FORMS as THREAD-DIAMOND-LAST for side effects only;
return the value of FORM.  The macro -!<>> is an alias for
THREAD-DO-DIAMOND-LAST."
  (let ((x (gensym "X")))
    `(let ((,x ,form))
       (thread-diamond-last ,x ,@forms)
       ,x)))

(behavior 'thread-do-first
  ;; Return seed; pipeline runs for effect.
  (let ((side 0))
    (should-equal '(a b)
                  (thread-do-first (list 'a 'b)
                    ((lambda (x) (incf side) x))))
    (should= 1 side))
  ;; Multi-step pipeline; seed unchanged even if steps transform.
  (let ((seen '()))
    (should= 2
             (thread-do-first 2
               ((lambda (x) (push x seen) (* x 10)))
               ((lambda (x) (push x seen) (+ x 1)))))
    ;; First-style: each step receives previous result as first arg
    ;; (pushes happen before the step's return value).
    (should-equal '(20 2) seen))
  ;; Zero extra forms: just the seed.
  (should= 9 (thread-do-first 9)))

(behavior 'thread-do-last
  (let ((side 0))
    (should-equal '(1 2 3)
                  (thread-do-last (list 1 2 3)
                    ((lambda (x) (incf side) x))))
    (should= 1 side))
  ;; Last-style insertion: (list 'z) with seed 5 => (list 'z 5).
  (let ((seen '()))
    (should= 5
             (thread-do-last 5
               ((lambda (extra x)
                  (push (list extra x) seen)
                  (+ x 100))
                'z)))
    (should-equal '((z 5)) seen))
  (should= 9 (thread-do-last 9)))

(behavior 'thread-do-diamond-first
  (let ((side 0))
    (should= 10
             (thread-do-diamond-first 10
               ((lambda (x) (incf side) (* x 2)))))
    (should= 1 side))
  ;; Diamond hole placement for effect; seed still returned.
  (let ((seen '()))
    (should-eq 'X
               (thread-do-diamond-first 'X
                 ((lambda (a b c)
                    (push (list a b c) seen)
                    'ignored)
                  'a <> 'c)))
    (should-equal '((a X c)) seen))
  ;; Default first insert when no hole.
  (let ((seen '()))
    (should= 3
             (thread-do-diamond-first 3
               ((lambda (x y)
                  (push (list x y) seen)
                  0)
                9)))
    (should-equal '((3 9)) seen)))

(behavior 'thread-do-diamond-last
  (let ((side 0))
    (should= 10
             (thread-do-diamond-last 10
               ((lambda (x) (incf side) (* x 2)))))
    (should= 1 side))
  ;; Default last insert when no hole: (list 1 2) + seed 9 => (1 2 9).
  (let ((seen '()))
    (should= 9
             (thread-do-diamond-last 9
               ((lambda (&rest args)
                  (push args seen)
                  'nope)
                1 2)))
    (should-equal '((1 2 9)) seen))
  ;; Explicit hole.
  (let ((seen '()))
    (should-eq 'Y
               (thread-do-diamond-last 'Y
                 ((lambda (a b c)
                    (push (list a b c) seen)
                    nil)
                  'a <> 'c)))
    (should-equal '((a Y c)) seen))
  (should= 4 (thread-do-diamond-last 4)))

(macro-alias thread-do-first -!>)
(macro-alias thread-do-last -!>>)
(macro-alias thread-do-diamond-first -!<>)
(macro-alias thread-do-diamond-last -!<>>)

(behavior '-!>
  (should-eq (macro-function '-!>) (macro-function 'thread-do-first))
  (should= 9 (eval '(-!> 9)))
  (should-equal
   '((a b) 1)
   (eval '(let ((n 0))
            (list
             (-!> (list 'a 'b)
               ((lambda (x) (incf n) x)))
             n)))))

(behavior '-!>>
  (should-eq (macro-function '-!>>) (macro-function 'thread-do-last))
  (should= 9 (eval '(-!>> 9)))
  (should-equal
   '(5 (z 5))
   (eval '(let ((seen nil))
            (list
             (-!>> 5
               ((lambda (extra x)
                  (setf seen (list extra x))
                  0)
                'z))
             seen)))))

(behavior '-!<>
  (should-eq (macro-function '-!<>) (macro-function 'thread-do-diamond-first))
  (should= 10 (eval '(-!<> 10 ((lambda (x) (* x 2))))))
  (should-equal
   '(X (a X c))
   (eval '(let ((seen nil))
            (list
             (-!<> 'X
               ((lambda (a b c)
                  (setf seen (list a b c))
                  'ignored)
                'a <> 'c))
             seen)))))

(behavior '-!<>>
  (should-eq (macro-function '-!<>>) (macro-function 'thread-do-diamond-last))
  (should= 10 (eval '(-!<>> 10 ((lambda (x) (* x 2))))))
  (should-equal
   '(9 (1 2 9))
   (eval '(let ((seen nil))
            (list
             (-!<>> 9
               ((lambda (&rest args)
                  (setf seen args)
                  'nope)
                1 2))
             seen)))))

;;;; ---------------------------------------------------------------------------
;;;; Applicative arrows
;;;; ---------------------------------------------------------------------------

(defmacro thread-apply-last (&rest forms)
  "Thread with APPLY at each step (last-arg style).  A bare function
designator F becomes (apply #'F <threaded>).  A list (F . EXTRAS) becomes
(apply #'F (list* EXTRAS... <threaded>)).  The macro APPLY->> is an alias
for THREAD-APPLY-LAST."
  (reduce (lambda (acc form)
            (if (consp form)
                `(apply (function ,(first form)) (list* ,@(rest form) ,acc))
                `(apply (function ,form) ,acc)))
          (rest forms)
          :initial-value (first forms)))

(defmacro thread-apply-first (&rest forms)
  "Thread with APPLY at each step (first-arg style).  A bare F becomes
(apply #'F <threaded>); a list (F . EXTRAS) becomes
(apply #'F (cons <threaded> EXTRAS)).  The macro APPLY-> is an alias for
THREAD-APPLY-FIRST."
  (reduce (lambda (acc form)
            (if (consp form)
                `(apply (function ,(first form)) (cons ,acc (list ,@(rest form))))
                `(apply (function ,form) ,acc)))
          (rest forms)
          :initial-value (first forms)))

(behavior 'thread-apply-last
  ;; Single form: identity.
  (should-equal '(1 2) (thread-apply-last '(1 2)))
  (should= 10 (thread-apply-last '((1 2) (3 4)) append +))
  (should-equal '(5 6 1 2 3 4)
                (thread-apply-last '((1 2) (3 4)) (append '(5 6))))
  (should= 21 (thread-apply-last '((1 2) (3 4)) (append '(5 6)) +))
  ;; Bare designator only (no extras).
  (should-equal '(1 2 3 4)
                (thread-apply-last '((1 2) (3 4)) append)))

(behavior 'thread-apply-first
  (should-equal '(1 2) (thread-apply-first '(1 2)))
  (should= 10 (thread-apply-first '((1 2) (3 4)) append +))
  (should-equal '(1 2 3 4 5 6)
                (thread-apply-first '(1 2 3 4) (append '(5 6))))
  (should= 21 (thread-apply-first '(1 2 3 4) (append '(5 6)) +))
  ;; cons style: threaded value is first apply arg (not spliced).
  (should-equal '((1 2 3) a b)
                (thread-apply-first '(1 2 3) (list 'a 'b))))

(macro-alias thread-apply-first apply->)
(macro-alias thread-apply-last apply->>)

(behavior 'apply->
  (should-eq (macro-function 'apply->) (macro-function 'thread-apply-first))
  (should= 10 (eval '(apply-> '((1 2) (3 4)) append +)))
  (should-equal '(1 2 3 4 5 6)
                (eval '(apply-> '(1 2 3 4) (append '(5 6))))))

(behavior 'apply->>
  (should-eq (macro-function 'apply->>) (macro-function 'thread-apply-last))
  (should= 10 (eval '(apply->> '((1 2) (3 4)) append +)))
  (should-equal '(5 6 1 2 3 4)
                (eval '(apply->> '((1 2) (3 4)) (append '(5 6))))))

;;;; ---------------------------------------------------------------------------
;;;; Branching (furculae)
;;;; ---------------------------------------------------------------------------

(defun %parallel-call (thunks)
  "Run THUNKS (zero-arg functions) in parallel on SBCL; else sequentially."
  #+sbcl
  (let ((threads (mapcar (lambda (th) (sb-thread:make-thread th)) thunks)))
    (mapcar #'sb-thread:join-thread threads))
  #-sbcl
  (mapcar #'funcall thunks))

(defmacro %furcula (operator parallel form &rest branches)
  "Expand a branching arrow using OPERATOR (threading macro name)."
  (let ((base (gensym "BASE")))
    (cond
      ((null branches)
       ;; Still evaluate FORM once; return no branch results.
       `(progn ,form '()))
      (parallel
       `(let ((,base ,form))
          (%parallel-call
           (list ,@(mapcar (lambda (branch)
                             `(lambda ()
                                (,operator ,base ,branch)))
                           branches)))))
      (t
       `(let ((,base ,form))
          (list ,@(mapcar (lambda (branch)
                            `(,operator ,base ,branch))
                          branches)))))))

(defmacro thread-branch-first (form &rest branches)
  "Evaluate FORM once, then thread it into each of BRANCHES with
THREAD-FIRST; return a list of results.  The macro -< is an alias for
THREAD-BRANCH-FIRST (swiss-arrows furcula)."
  `(%furcula thread-first nil ,form ,@branches))

(defmacro thread-branch-first-parallel (form &rest branches)
  "Parallel THREAD-BRANCH-FIRST.  The macro -<-p is an alias (swiss-arrows
-<:p)."
  `(%furcula thread-first t ,form ,@branches))

(defmacro thread-branch-last (form &rest branches)
  "Like THREAD-BRANCH-FIRST, but each branch uses THREAD-LAST.  The macro -<<
is an alias for THREAD-BRANCH-LAST (swiss-arrows trystero furcula)."
  `(%furcula thread-last nil ,form ,@branches))

(defmacro thread-branch-last-parallel (form &rest branches)
  "Parallel THREAD-BRANCH-LAST.  The macro -<<-p is an alias."
  `(%furcula thread-last t ,form ,@branches))

(defmacro thread-branch-diamond-first (form &rest branches)
  "Like THREAD-BRANCH-FIRST, but each branch uses THREAD-DIAMOND-FIRST.
The macro -<>< is an alias for THREAD-BRANCH-DIAMOND-FIRST."
  `(%furcula thread-diamond-first nil ,form ,@branches))

(defmacro thread-branch-diamond-first-parallel (form &rest branches)
  "Parallel THREAD-BRANCH-DIAMOND-FIRST.  The macro -<><-p is an alias."
  `(%furcula thread-diamond-first t ,form ,@branches))

(defmacro thread-branch-diamond-last (form &rest branches)
  "Like THREAD-BRANCH-FIRST, but each branch uses THREAD-DIAMOND-LAST.
The macro -<>>< is an alias for THREAD-BRANCH-DIAMOND-LAST."
  `(%furcula thread-diamond-last nil ,form ,@branches))

(defmacro thread-branch-diamond-last-parallel (form &rest branches)
  "Parallel THREAD-BRANCH-DIAMOND-LAST.  The macro -<>><-p is an alias."
  `(%furcula thread-diamond-last t ,form ,@branches))

(behavior 'thread-branch-first
  ;; Empty branches: just an empty result list; form still evaluated once.
  (let ((n 0))
    (should-equal '()
                  (thread-branch-first (progn (incf n) 1)))
    (should= 1 n))
  (should-equal '((3 2) (3 3) (3 4))
                (thread-branch-first (+ 1 2)
                                     (list 2)
                                     (list 3)
                                     (list 4)))
  (should-equal '((6) (3 4))
                (thread-branch-first (+ 1 2)
                                     (thread-first (* 2) list)
                                     (list 4)))
  ;; FORM evaluated once, then shared across branches.
  (let ((n 0))
    (should-equal '(9 20 15)
                  (thread-branch-first (progn (incf n) 10)
                                       1-
                                       (* 2)
                                       (+ 5)))
    (should= 1 n)))

(behavior 'thread-branch-last
  (should-equal '() (thread-branch-last 1))
  (should-equal '((2 1 3) (5 7 3) (9 4 3))
                (thread-branch-last (+ 1 2)
                                    (list 2 1)
                                    (list 5 7)
                                    (list 9 4)))
  ;; Shared base evaluation once.
  (let ((n 0))
    (should-equal '((a 7) (b 7))
                  (thread-branch-last (progn (incf n) 7)
                                      (list 'a)
                                      (list 'b)))
    (should= 1 n)))

(behavior 'thread-branch-diamond-first
  (should-equal '() (thread-branch-diamond-first 1))
  (should-equal '((3 2 1) (5 3 7) (9 4 3))
                (thread-branch-diamond-first (+ 1 2)
                                             (list <> 2 1)
                                             (list 5 <> 7)
                                             (list 9 4 <>)))
  ;; Default first insert in a branch.
  (should-equal '((3 9))
                (thread-branch-diamond-first 3 (list 9))))

(behavior 'thread-branch-diamond-last
  (should-equal '() (thread-branch-diamond-last 1))
  (should-equal '((3 2 1) (5 3 7) (9 4 3) (10 11 3))
                (thread-branch-diamond-last (+ 1 2)
                                            (list <> 2 1)
                                            (list 5 <> 7)
                                            (list 9 4 <>)
                                            (list 10 11)))
  ;; Default last insert in a branch.
  (should-equal '((9 3))
                (thread-branch-diamond-last 3 (list 9))))

(behavior 'thread-branch-first-parallel
  (should-equal '() (thread-branch-first-parallel 1))
  (should-equal '((3 2) (3 3) (3 4))
                (thread-branch-first-parallel (+ 1 2)
                                              (list 2)
                                              (list 3)
                                              (list 4)))
  ;; FORM evaluated once even when branches run (possibly) in parallel.
  (let ((n 0))
    (should-equal '(9 20)
                  (thread-branch-first-parallel (progn (incf n) 10)
                                                1-
                                                (* 2)))
    (should= 1 n)))

(behavior 'thread-branch-last-parallel
  (should-equal '() (thread-branch-last-parallel 1))
  (should-equal '((2 1 3) (5 7 3) (9 4 3))
                (thread-branch-last-parallel (+ 1 2)
                                             (list 2 1)
                                             (list 5 7)
                                             (list 9 4)))
  (let ((n 0))
    (should-equal '((a 7) (b 7))
                  (thread-branch-last-parallel (progn (incf n) 7)
                                               (list 'a)
                                               (list 'b)))
    (should= 1 n)))

(behavior 'thread-branch-diamond-first-parallel
  (should-equal '() (thread-branch-diamond-first-parallel 1))
  (should-equal '((3 2 1) (5 3 7) (9 4 3))
                (thread-branch-diamond-first-parallel
                 (+ 1 2)
                 (list <> 2 1)
                 (list 5 <> 7)
                 (list 9 4 <>)))
  (let ((n 0))
    (should-equal '((0 x) (y 0))
                  (thread-branch-diamond-first-parallel
                   (progn (incf n) 0)
                   (list <> 'x)
                   (list 'y <>)))
    (should= 1 n)))

(behavior 'thread-branch-diamond-last-parallel
  (should-equal '() (thread-branch-diamond-last-parallel 1))
  (should-equal '((3 2 1) (5 3 7) (9 4 3) (10 11 3))
                (thread-branch-diamond-last-parallel
                 (+ 1 2)
                 (list <> 2 1)
                 (list 5 <> 7)
                 (list 9 4 <>)
                 (list 10 11)))
  (let ((n 0))
    (should-equal '((x 0) (0 y))
                  (thread-branch-diamond-last-parallel
                   (progn (incf n) 0)
                   (list 'x <>)
                   (list <> 'y)))
    (should= 1 n)))

(macro-alias thread-branch-first -<)
(macro-alias thread-branch-first-parallel -<-p)
(macro-alias thread-branch-last -<<)
(macro-alias thread-branch-last-parallel -<<-p)
(macro-alias thread-branch-diamond-first -<><)
(macro-alias thread-branch-diamond-first-parallel -<><-p)
(macro-alias thread-branch-diamond-last -<>><)
(macro-alias thread-branch-diamond-last-parallel -<>><-p)

(behavior '-<
  (should-eq (macro-function '-<) (macro-function 'thread-branch-first))
  (should-equal '((3 2) (3 3) (3 4))
                (eval '(-< (+ 1 2) (list 2) (list 3) (list 4))))
  (should-equal '() (eval '(-< 1))))

(behavior '-<<
  (should-eq (macro-function '-<<) (macro-function 'thread-branch-last))
  (should-equal '((2 1 3) (5 7 3))
                (eval '(-<< (+ 1 2) (list 2 1) (list 5 7)))))

(behavior '-<><
  (should-eq (macro-function '-<><)
             (macro-function 'thread-branch-diamond-first))
  (should-equal '((3 2 1) (5 3 7))
                (eval '(-<>< (+ 1 2) (list <> 2 1) (list 5 <> 7)))))

(behavior '-<>><
  (should-eq (macro-function '-<>><)
             (macro-function 'thread-branch-diamond-last))
  (should-equal '((3 2 1) (10 11 3))
                (eval '(-<>>< (+ 1 2) (list <> 2 1) (list 10 11)))))

(behavior '-<-p
  (should-eq (macro-function '-<-p)
             (macro-function 'thread-branch-first-parallel))
  (should-equal '((3 2) (3 3) (3 4))
                (eval '(-<-p (+ 1 2) (list 2) (list 3) (list 4)))))

(behavior '-<<-p
  (should-eq (macro-function '-<<-p)
             (macro-function 'thread-branch-last-parallel))
  (should-equal '((2 1 3) (5 7 3))
                (eval '(-<<-p (+ 1 2) (list 2 1) (list 5 7)))))

(behavior '-<><-p
  (should-eq (macro-function '-<><-p)
             (macro-function 'thread-branch-diamond-first-parallel))
  (should-equal '((3 2 1) (5 3 7))
                (eval '(-<><-p (+ 1 2) (list <> 2 1) (list 5 <> 7)))))

(behavior '-<>><-p
  (should-eq (macro-function '-<>><-p)
             (macro-function 'thread-branch-diamond-last-parallel))
  (should-equal '((3 2 1) (10 11 3))
                (eval '(-<>><-p (+ 1 2) (list <> 2 1) (list 10 11)))))
