(in-package :cl-user)
(defpackage validate-test
  (:use :cl
        :validate
        :prove))
(in-package :validate-test)

(plan nil)

;; Test parsing
(is (validate:parse "a string" 'string) "a string")
(is (validate:parse "12" 'integer) 12)
(is-error (validate:parse "a pizza" 'integer)
          'validate:<parse-error>
          "raise a parse error on strings that aren't integers")

;; Test schema validation

(finalize)
