#|
  This file is a part of validate project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(in-package :cl-user)
(defpackage validate-test-asd
  (:use :cl :asdf))
(in-package :validate-test-asd)

(defsystem validate-test
  :author "Matt Novenstern"
  :license ""
  :depends-on (:validate
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "validate"))))
  :description "Test system for validate"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
