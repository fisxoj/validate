(in-package :cl-user)
(defpackage validate-test
  (:use :cl
        :prove))
(in-package :validate-test)

(plan nil)

;; Test parsing

;;; Email
(is-error (v:email "me@here") 'v:<validation-error> "bad email throws an error")

;;; String
(is-error (v:str "me" :min-length 3) 'v:<validation-error> "short string throws an error")
(is (v:str "a string") "a string")

;;; Integer
(is (v:int "12") 12)
(is-error (v:int "a pizza")
          'validate:<parse-error>
          "raise a parse error on strings that aren't integers")

;;; Booleans
(is (v:bool "true") t)
(is (v:bool "no") nil)
(is (v:bool "y") t)
(is (v:bool "disable") nil)
(is-error (v:bool "2")
          'validate:<parse-error>
          "raise a parse error on strings that aren't boolean values")

;; Test schema validation

(let ((schema `(:name    ,(lambda (value) (v:str value :min-length 1))
                :age     v:int
                :has-dog v:bool
                :email   v:email))
      (data '(
             :name    "matt"
             :age     "27"
             :has-dog "no"
             :email   "something@example.com")))

  (v:with-validated-values (name age has-dog email) (schema data)
    (is name     "matt")
    (is age      27)
    (is has-dog  nil)
    (is email    "something@example.com")))

(finalize)
