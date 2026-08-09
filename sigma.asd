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

(defpackage :sigma/system
  (:use :common-lisp
        :asdf)
  (:export :version-string
           :version-list
           :version-major
           :version-minor
           :version-revision))

(in-package :sigma/system)

(defparameter version-major 3)
(defparameter version-minor 9)
(defparameter version-revision 0)

(defun version-list ()
  (list version-major version-minor version-revision))

(defun version-string ()
  (format nil "~{~A.~A.~A~}" (version-list)))

(defsystem "sigma"
  :description "This is a set of generic utility functions and macros that I
           use throughout my Common Lisp code pretty much everywhere.  I find
           them useful, and hopefully you do too."
  :version #.(version-string)
  :author "Christopher Mark Gore <cgore@cgore.com>"
  :license "BSD-3-Clause"
  :homepage "https://github.com/cgore/sigma"
  :source-control (:git "https://github.com/cgore/sigma.git")
  :bug-tracker "https://github.com/cgore/sigma/issues"

  ;; Specs live in the sources as BEHAVIOR/SHOULD forms and run at load time.
  ;; TEST-OP reloads every source file so those top-level assertions run again.
  ;; (We LOAD sources directly rather than LOAD-SYSTEM :FORCE T, which ASDF
  ;; forbids inside a nested OPERATE.)
  :perform (test-op (operation system)
                    (declare (ignore operation))
                    (labels ((reload (component)
                               (typecase component
                                 (cl-source-file
                                  (load (component-pathname component)))
                                 (parent-component
                                  (map nil #'reload (component-children component))))))
                      (reload system)))

  :components ((:module "source"
                :components ((:file "behave")
                             (:file "control"
                              :depends-on ("behave"))
                             (:file "hash"
                              :depends-on ("behave"
                                           "control"))
                             (:file "numeric"
                              :depends-on ("behave"
                                           "control"
                                           "sequence"))
                             (:file "os"
                              :depends-on ("control"
                                           "string"))
                             (:file "probability"
                              :depends-on ("control"
                                           "numeric"))
                             (:file "random"
                              :depends-on ("control"
                                           "sequence"))
                             (:file "sequence"
                              :depends-on ("behave"
                                           "control"))
                             (:file "string"
                              :depends-on ("behave"
                                           "control"
                                           "numeric"
                                           "sequence"))
                             (:file "time-series"
                              :depends-on ("control"
                                           "numeric"
                                           "sequence"))
                             (:file "truth"
                              :depends-on ("behave"
                                           "control"))
                             (:file "sigma"
                              :depends-on ("behave"
                                           "control"
                                           "hash"
                                           "numeric"
                                           "os"
                                           "probability"
                                           "random"
                                           "sequence"
                                           "string"
                                           "time-series"
                                           "truth"))))))
