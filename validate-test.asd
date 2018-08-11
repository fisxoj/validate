#|
  This file is a part of validate project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(defsystem validate-test
  :author "Matt Novenstern"
  :license ""
  :depends-on ("validate"
               "prove")
  :pathname "t"
  :components ((:test-file "validate"))
  :description "Test system for validate"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op (op c)
		    (funcall (read-from-string "prove:run")
			     (system-relative-pathname :validate-test #P"t/"))))
