;;;; Copyright (c) 2005 -- 2014, Christopher Mark Gore,
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

(in-package :asdf)

(defsystem "sigma"
  :description "This is a set of generic utility functions and macros that I
           use throughout my Common Lisp code pretty much everywhere.  I find
           them useful, and hopefully you do too."
  :version "3.3.1"
  :author "Christopher Mark Gore <cgore@cgore.com>"
  :license "
Copyright (c) 2005 -- 2014, Christopher Mark Gore,
All rights reserved.

2317 South River Road, Saint Charles, Missouri 63303 USA.
Web: http://cgore.com
Email: cgore@cgore.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of Christopher Mark Gore nor the names of other
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
"
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
                                            :depends-on ("control"))
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
                                            :depends-on ("control"))
                                     (:file "sigma"
                                            :depends-on ("behave"
                                                         "control"
                                                         "hash"
                                                         "numeric"
                                                         "os"
                                                         "random"
                                                         "sequence"
                                                         "string"
                                                         "time-series"
                                                         "truth"))))))
