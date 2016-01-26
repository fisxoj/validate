(in-package :cl-user)
(defpackage validate-test
  (:use :cl
        :prove))
(in-package :validate-test)

(plan nil)

;; Test parsing
;; (is (validate:parse "a string" 'string) "a string")
;; (is (validate:parse "12" 'integer) 12)
;; (is-error (validate:parse "a pizza" 'integer)
;;           'validate:<parse-error>
;;           "raise a parse error on strings that aren't integers")
;; (is (validate:parse "true" 'boolean) t)
;; (is (validate:parse "no" 'boolean) nil)
;; (is (validate:parse "y" 'boolean) t)
;; (is (validate:parse "disable" 'boolean) nil)

;; Test schema validation

(let ((schema (list
               :name (lambda (value) (v:str value :min-length 1))
               :age  #'v:int
               :has-dog #'v:bool))
      (data (list
             :name    "matt"
             :age     "27"
             :has-dog "no")))

  (let ((validated (validate:schema schema data)))
    (is (getf validated :name)    "matt")
    (is (getf validated :age)     27)
    (is (getf validated :has-dog) nil)))

(finalize)
