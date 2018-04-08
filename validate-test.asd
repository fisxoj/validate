#|
  This file is a part of validate project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(defsystem validate-test
  :author "Matt Novenstern"
  :license ""
  :depends-on ("validate"
               "prove")
  :pathname #P"t/"
  :components ((:test-file "validate"))
  :description "Test system for validate"

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (op c)
                    (symbol-call :prove-asdf :run-test-system c)))
