(in-package :cl-user)
(defpackage validate-test
  (:use :cl
        :prove))
(in-package :validate-test)

(plan nil)

;; Test parsing

;;; Email
(subtest "Email"
  (is (v:email "me@here.xyz") "me@here.xyz" "Valid emails are ok.")
  (is-error (v:email "me@here") 'v:<validation-error> "Email without a TLD throws an error.")
  (is-error (v:email "me@here.a") 'v:<validation-error> "Email with an invalid TLD throws an error.")
  (is-error (v:email "mehere.xyz") 'v:<validation-error> "Email with no @ throws an error."))

;;; String
(subtest "Strings"
  (is-error (v:str "me" :min-length 3) 'v:<validation-error> "A too-short string throws an error.")
  (is-error (v:str "pizza" :max-length 3) 'v:<validation-error> "A too-long string throws an error.")
  (is (v:str "a string") "a string" "Validated strings match the original string."))

;;; Integer
(subtest "Integers"
  (is (v:int "12") 12 "An integer gets converted to a string.")
  (is-error (v:int "a pizza")
	    'validate:<parse-error>
	    "A parse error is raised on strings that aren't integers."))

;;; Booleans
(subtest "Boolean values"
  (subtest "True/False"
    (is (v:bool "true") t "'true'")
    (is (v:bool "True") t "'True")
    (is (v:bool "false") nil "'false'")
    (is (v:bool "FALSE") nil "'FALSE'")
    (is (v:bool "f") nil "'f'")
    (is (v:bool "t") t "'t'"))

  (subtest "Yes/No"
    (is (v:bool "no") nil "'No'")
    (is (v:bool "n") nil "'n")
    (is (v:bool "Yes") t "'Yes'")
    (is (v:bool "YES") t "'YES")
    (is (v:bool "y") t "'y'"))

  (subtest "On/Off"
    (is (v:bool "on") t "'on'")
    (is (v:bool "off") nil "'off'"))

  (subtest "Enable/Disable"
    (is (v:bool "disable") nil "'disable'")
    (is (v:bool "enable") t "'enable'"))

  (is-error (v:bool "2")
	    'validate:<parse-error>
	    "raise a parse error on strings that aren't boolean values"))

;;; Timestamps
(subtest "Timestamp"
  (ok (v:timestamp "2016-10-18T23:20:18.594Z")
      "Parses a timestamp.")
  (is-error (v:timestamp "potato") 'v:<validation-error>
            "Raises an error on an invalid timestamp."))

;; Default values
(subtest "Default"
  (is (v:default nil "potato") "potato" "Supplies a value instead of nil.")
  (is (v:default "" "potato") "potato" "Supplies a value for an empty string."))

;; Test schema validation
(subtest "Schemas"
  (let ((schema '(:name          ((v:str :min-length 1))
		  :age           ((v:int))
		  :has-dog       ((v:bool))
		  :email         ((v:email))
		  :favorite-food ((v:default "pizza") (v:str :min-length 3))))

	(data   '(:name    "matt"
		  :age     "27"
		  :has-dog "no"
		  :email   "something@example.com")))

    (v:with-validated-values (name
			      age
			      (dog has-dog) ;; Value bound to a different name
			      email
			      favorite-food)
	(schema data)

      (is name          "matt")
      (is age           27)
      (is dog           nil)
      (is email         "something@example.com")
      (is favorite-food "pizza"))))

(finalize)
