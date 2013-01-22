;;;; Copyright (c) 2005 -- 2013, Christopher Mark Gore,
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

(in-package :asdf)

(defsystem "cgore-utilities"
  :description "This is a set of generic utility functions and macros that I
           use throughout my Common Lisp code pretty much everywhere.  I find
           them useful, and hopefully you do too."
  :version "2.1.2"
  :author "Christopher Mark Gore <cgore@cgore.com>"
  :license "BSD-style"
  :components ((:file "control")
               (:file "numeric"
                      :depends-on ("control"))
               (:file "os"
                      :depends-on ("control"
                                   "string"))
               (:file "probability"
                      :depends-on ("control"
                                   "numeric"))
               (:file "random"
                      :depends-on ("control"))
               (:file "sequence"
                      :depends-on ("control"))
               (:file "string"
                      :depends-on ("control"
                                   "numeric"
                                   "sequence"))
               (:file "time-series"
                      :depends-on ("control"
                                   "numeric"
                                   "sequence"))
               (:file "truth"
                      :depends-on ("control"))
               (:file "utilities"
                      :depends-on ("control"
                                   "numeric"
                                   "sequence"
                                   "string"
                                   "time-series"
                                   "truth"))))
