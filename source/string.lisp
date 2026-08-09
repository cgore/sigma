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

(defpackage :sigma/string
  (:use :common-lisp
        :sigma/behave
        :sigma/control
        :sigma/numeric
        :sigma/sequence)
  (:export :+whitespace+
           :character-range
           :character-ranges
           :escape-tildes
           :replace-char
           :strcat
           :string-concatenate
           :stringify
           :string-join
           :string-trim-whitespace
           :string-left-trim-whitespace
           :string-right-trim-whitespace
           :split
           :strmult
           :to-string))
(in-package :sigma/string)

(defconstant-once +whitespace+
                  '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))

(behavior '+whitespace+
  (should-be-true (boundp '+whitespace+))
  (should-be-true (listp +whitespace+))
  (should-be-true (member #\Space +whitespace+ :test #'char=))
  (should-be-true (member #\Tab +whitespace+ :test #'char=))
  (should-be-true (member #\Newline +whitespace+ :test #'char=))
  (should-string= (string-trim +whitespace+ (format nil " ~A " #\Tab))
                  ""))

(defun character-range (start end)
  "The CHARACTER-RANGE function returns a list of the characters from START to
END."
  (let* ((endpoints (sort (list start end) #'char-lessp))
         (start (first endpoints))
         (end (second endpoints)))
    (loop for i from (char-code start) to (char-code end)
       collect (code-char i))))

(behavior 'character-range
  (should-equal (character-range #\a #\z)
                '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
                  #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
  (should-equal (character-range #\a #\z)
                (character-range #\z #\a))
  (should-equal (character-range #\a #\a)
                '(#\a)))

(defun character-ranges (&rest rest)
  "Return a sorted list of unique characters covering one or more inclusive
ranges.  REST is a flat sequence of endpoint pairs (START END START END ...);
a single leftover argument is included as-is.  Overlapping ranges are merged
via REMOVE-DUPLICATES."
  (sort (remove-duplicates
         (cond ((<= (length rest) 1)
                rest)
               ((= 2 (length rest))
                (character-range (car rest) (cadr rest)))
               ((< 2 (length rest))
                (concatenate 'list
                             (character-range (car rest) (cadr rest))
                             (apply #'character-ranges (cddr rest))))))
        #'char-lessp))

(behavior 'character-ranges
  (should-equal (character-ranges #\a #\z
                                  #\1 #\9)
                (sort (concatenate 'list
                                   (character-range #\a #\z)
                                   (character-range #\1 #\9))
                      #'char-lessp))
  (should-equal (character-ranges #\a #\z #\q #\t)
                (character-range #\a #\z))
  (should-equal (character-ranges #\a #\z)
                (character-ranges #\z #\a)))

(defun replace-char (string from-char to-char)
  "Replaces every instance of FROM-CHAR with TO-CHAR."
  (assert (stringp string))
  (loop for i from 0 to (1- (length string)) do
        (if (char= (char string i) from-char)
          (setf (char string i) to-char)))
  string)

(behavior 'replace-char
  ;; Destructive: use a fresh string, not a literal constant.
  (let ((s (copy-seq "banana")))
    (should-string= (replace-char s #\a #\o) "bonono")
    (should-string= s "bonono"))
  (let ((s (copy-seq "xyz")))
    (should-string= (replace-char s #\a #\b) "xyz")))

(defmethod split ((string string)
                   separators
                   &key
                   (key #'identity)
                   (test #'string=)
                   (remove-separators? t))
  "Split STRING into a list of substrings on SEPARATORS.  SEPARATORS may be a
single separator or a sequence of them; KEY and TEST control membership
tests, and REMOVE-SEPARATORS? controls whether separator text is kept."
  (mapcar (rcurry #'coerce 'string)
          (split (coerce string 'list) separators
                 :key key :test test :remove-separators? remove-separators?)))

(behavior 'split
  (should-equal (split "a,b,c" #\,) '("a" "b" "c"))
  (should-equal (split "a::b::c" #\:) '("a" "" "b" "" "c"))
  (should-equal (split "one two three" #\Space) '("one" "two" "three")))

(defun string-join (strings &optional (connecting-string ""))
  "Join STRINGS into one string, inserting CONNECTING-STRING between each
adjacent pair.  STRINGS may be a list of strings or a single string (returned
unchanged aside from the single-element join path).  CONNECTING-STRING
defaults to the empty string."
  (assert (or (stringp strings)
              (and (listp strings)
                   (every #'stringp strings))))
  (if (stringp strings)
    (string-join (list strings))
    (apply #'concatenate 'string
           (first strings)
           (mapcar (lambda (string)
                     (concatenate 'string connecting-string string))
                   (rest strings)))))

(behavior 'string-join
  (should-string= (string-join "solo") "solo")
  (should-string= (string-join '("a" "b" "c")) "abc")
  (should-string= (string-join '("a" "b" "c") ",") "a,b,c")
  (should-string= (string-join '("x") "-") "x"))

(defun string-trim-whitespace (string)
  "Removes whitespace from the left side and the right side of a string."
  (string-trim +whitespace+ string))

(behavior 'string-trim-whitespace
  (should-string= (string-trim-whitespace "   foo   ") "foo")
  (should-string= (string-trim-whitespace "foo   ")    "foo")
  (should-string= (string-trim-whitespace "   foo")    "foo"))

(defun string-left-trim-whitespace (string)
  "Removes whitespace from the left side only of a string."
  (string-left-trim +whitespace+ string))

(behavior 'string-left-trim-whitespace
  (should-string= (string-left-trim-whitespace "   foo   ") "foo   ")
  (should-string= (string-left-trim-whitespace "foo   ")    "foo   ")
  (should-string= (string-left-trim-whitespace "   foo")    "foo"))

(defun string-right-trim-whitespace (string)
  "Removes whitespace from the right side only of a string."
  (string-right-trim +whitespace+ string))

(behavior 'string-right-trim-whitespace
  (should-string= (string-right-trim-whitespace "   foo   ") "   foo")
  (should-string= (string-right-trim-whitespace "foo   ")    "foo")
  (should-string= (string-right-trim-whitespace "   foo")    "   foo"))

(defun stringify (argument)
  "The STRINGIFY function takes in an argument of any type and converts it to a
string using FORMAT's ~A directive under WITH-STANDARD-IO-SYNTAX, so the result
does not depend on the caller's *PRINT-CASE*, *PRINT-BASE*, or other printer
controls.  Also see TO-STRING."
  (with-standard-io-syntax
    (format nil "~A" argument)))

(behavior 'stringify
  (should-string= "12" (stringify 12))
  (should-string= "FOO" (stringify :foo))
  (should-string= "FOO" (stringify 'foo))
  ;; Stable under ambient printer settings (GitHub issue #3).
  (let ((*print-case* :downcase)
        (*print-base* 16)
        (*print-radix* t))
    (should-string= "FOO" (stringify :foo))
    (should-string= "255" (stringify 255))
    (should-string= "12" (stringify 12))))

(defun to-string (s)
  "The TO-STRING function converts common types of things into a string.  It
handles some special cases more usefully than STRINGIFY for most user-facing
output: NIL becomes the empty string, symbols use a downcased SYMBOL-NAME, and
strings are returned unchanged.  All other values are printed with ~A under
WITH-STANDARD-IO-SYNTAX so ambient printer controls do not affect the result."
  (cond ((null s) "")
        ((symbolp s) (string-downcase (symbol-name s)))
        ((stringp s) s)
        (t (with-standard-io-syntax
             (format nil "~A" s)))))

(behavior 'to-string
  (should-equal (to-string nil) "")
  (should-equal (to-string :foo) "foo")
  (should-equal (to-string 'BAR) "bar")
  (should-equal (to-string "hello") "hello")
  (should-equal (to-string "Hello, world!") "Hello, world!")
  (should-equal (to-string 12) "12")
  ;; Special cases and fallback stay stable under weird printer settings.
  (let ((*print-case* :upcase)
        (*print-base* 16)
        (*print-radix* t))
    (should-equal (to-string :foo) "foo")
    (should-equal (to-string nil) "")
    (should-equal (to-string 255) "255")))

(defun strcat (&rest rest)
  "The STRCAT function takes in a list of things concatenates their string
versions."
  (apply #'concatenate 'string (mapcar #'to-string rest)))

(behavior 'strcat
          (should-string= "foobar" (strcat "foo" "bar"))
          (should-string= "foobarbaz" (strcat "foo" "bar" "baz"))
          (should-string= "foo123bar" (strcat "foo" 123 "bar"))
          (should-string= "" (strcat))
          (should-string= "foo" (strcat "foo"))
          (should-string= "1234" (strcat 1 2 3 4))
          (should-string= "1" (strcat 1)))

(function-alias 'strcat 'string-concatenate)

(behavior 'string-concatenate
  (should-string= "ab" (string-concatenate "a" "b"))
  (should-eq (fdefinition 'string-concatenate) (fdefinition 'strcat)))

(defun escape-tildes (string)
  "Return a copy of STRING in which every tilde (#\\~) is doubled, suitable
for use as a FORMAT control string that should print tildes literally."
  (let ((input (vector-to-list string))
        (result nil)
        (current nil))
    (while (not (null input))
           (setf current (pop input))
           (when (eq current #\~)
             (push #\~ result))
           (push current result))
    (apply 'strcat (reverse result))))

(behavior 'escape-tildes
  (should-equal (escape-tildes "foo bar")
                "foo bar")
  (should-equal (escape-tildes "foo~bar")
                "foo~~bar"))

(defun strmult (count &rest strings)
  "Concatenate STRINGS (via STRCAT) and repeat that result COUNT times,
returning one combined string.  When COUNT is less than 1, returns the empty
string."
  (apply #'strcat (loop for i from 1 to count
                     collect (apply #'strcat strings))))

(behavior 'strmult
  (should-string= (strmult 3 "ab") "ababab")
  (should-string= (strmult 2 "x" "y") "xyxy")
  (should-string= (strmult 1 "z") "z")
  (should-string= (strmult 0 "nope") "")
  (should-string= (strmult -1 "nope") ""))
