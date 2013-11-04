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

(defpackage :sigma/string
  (:nicknames :string)
  (:use :common-lisp
        :sigma/behave
        :sigma/control
        :sigma/numeric
        :sigma/sequence)
  (:export :character-range
           :character-ranges
           :escape-tildes
           :replace-char
           :strcat
           :stringify
           :string-join
           :split
           :strmult
           :to-string))
(in-package :sigma/string)

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

(defun escape-tildes (string)
  (let ((input (vector-to-list string))
        (result nil)
        (current nil))
    (while (not (null input))
           (setf current (pop input))
           (when (eq current #\~)
             (push #\~ result))
           (push current result))
    (strcat (reverse result))))

(defun replace-char (string from-char to-char)
  "Replaces every instance of FROM-CHAR with TO-CHAR."
  (assert (stringp string))
  (loop for i from 0 to (1- (length string)) do
        (if (char= (char string i) from-char)
          (setf (char string i) to-char)))
  string)

(defmethod split ((string string)
                   separators
                   &key
                   (key #'identity)
                   (test #'string=)
                   (remove-separators? t))
  (mapcar (rcurry #'coerce 'string)
          (split (coerce string 'list) separators
                 :key key :test test :remove-separators? remove-separators?)))

(defun string-join (strings &optional (connecting-string ""))
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

(defun stringify (argument)
  "The STRINGIFY function takes in an argument of any type and converts it to a
string.  This produces the string as from the ~A directive to FORMAT.  Also see
TO-STRING."
  (format nil "~A" argument))

(behavior 'stringify
          (should-string= "12" (stringify 12)))

(defun to-string (s)
  "The TO-STRING function converts common types of things into a string.  It
handles some special cases more usefully than STRINGIFY for most user-facing
output."
  (cond ((null s) "")
        ((symbolp s) (string-downcase (symbol-name s)))
        ((stringp s) s)
        (t (format nil "~A" s))))

(behavior 'to-string
          (should-equal (to-string nil) "")
          (should-equal (to-string :foo) "foo")
          (should-equal (to-string "hello") "hello")
          (should-equal (to-string "Hello, world!") "Hello, world!"))

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

(defun strmult (count &rest strings)
  (apply #'strcat (loop for i from 1 to count
                     collect (apply #'strcat strings))))
