#|
  This file is a part of validate project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(defsystem validate
  :version "0.6.0"
  :author "Matt Novenstern"
  :license "LLGPLv3"
  :depends-on ("iterate"
               "cl-ppcre"
               "alexandria"
               "local-time")
  :pathname #P"src/"
  :components ((:file "validate"))
  :description "A data validation library for common lisp."
  :in-order-to ((test-op (test-op validate-test))))
