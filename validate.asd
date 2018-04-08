#|
  This file is a part of validate project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(defsystem validate
  :version "0.2.2"
  :author "Matt Novenstern"
  :license "LLGPLv3"
  :depends-on ("iterate"
               "cl-ppcre"
               "alexandria"
               "local-time"
               "jonathan")
  :pathname #P"src/"
  :components ((:file "validate"))

  :description "Data validation library."

  :in-order-to ((test-op (test-op validate-test))))
