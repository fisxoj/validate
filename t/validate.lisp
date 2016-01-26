(in-package :cl-user)
(defpackage validate-test
  (:use :cl
        :prove))
(in-package :validate-test)

(plan nil)

;; Test parsing
(is-error (v:email "me@here") 'v:<validation-error> "bad email throws an error")
(is (v:str "a string") "a string")
(is (v:int "12") 12)
(is-error (v:int "a pizza")
          'validate:<parse-error>
          "raise a parse error on strings that aren't integers")

(is (v:bool "true") t)
(is (v:bool "no") nil)
(is (v:bool "y") t)
(is (v:bool "disable") nil)

;; Test schema validation

(let ((schema (list
               :name (lambda (value) (v:str value :min-length 1))
               :age  #'v:int
               :has-dog #'v:bool
               :email #'v:email))
      (data (list
             :name    "matt"
             :age     "27"
             :has-dog "no"
             :email "something@example.com")))

  (let ((validated (validate:schema schema data)))
    (is (getf validated :name)    "matt")
    (is (getf validated :age)     27)
    (is (getf validated :has-dog) nil)
    (is (getf validated :email)   "something@example.com")))



(finalize)
