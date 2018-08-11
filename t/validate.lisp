(defpackage validate-test
  (:use #:cl
        #:prove))

(in-package :validate-test)

(plan 8)

;; Test parsing

;;; Email
(subtest "Email"
  (is (v:email "me@here.xyz") "me@here.xyz" "Valid emails are ok.")
  (is-error (v:email "me@here") 'v:<validation-error> "Email without a TLD throws an error.")
  (is-error (v:email "me@here.a") 'v:<validation-error> "Email with an invalid TLD throws an error.")
  (is-error (v:email "mehere.xyz") 'v:<validation-error> "Email with no @ throws an error."))

(subtest "regex"
  (ok (v:regex "aaaaaa" "[a]{5}")
      "Works for simple regexes.")
  (ok (v:regex "cat" "^(cat|dog)$")
      "Allows another simple regex.")
  (is-error (v:regex "potato" "\\d+")
	    'v:<validation-error>
	    "An non-matching string raises a validation error."))

;;; String
(subtest "Strings"
  (is-error (v:str "me" :min-length 3) 'v:<validation-error> "A too-short string throws an error.")
  (is-error (v:str "pizza" :max-length 3) 'v:<validation-error> "A too-long string throws an error.")
  (ok (v:str "three" :min-length 5 :max-length 5) "A string that matches the constraints is fine (constraints are inclusive)")
  (is (v:str "a string") "a string" "Validated strings match the original string."))

;;; Integer
(subtest "Integers"
  (is (v:int "12") 12 "An integer gets converted to a string.")
  (is (v:int "A" :radix 16) 10 "Integers in hex work.")
  (is (v:int "10" :radix 2) 2 "Binary integers work.")
  (is (v:int 34) 34 "An int is passed through.")
  (is (v:int 34 :max 100) 34 "An int below a maximum is allowed.")
  (is (v:int 34 :min 3) 34 "An int above a minimum is allowed.")
  (is (v:int 34 :min 34) 34 "An int equal to the minimum value is allowed.")
  (is (v:int 34 :max 34) 34 "An int equal to the maximum value is allowed.")
  (is-error (v:int "a pizza")
	    'validate:<validation-error>
	    "A validate error is raised on strings that aren't integers.")
  (is-error (v:int "127" :min 200)
	    'validate:<validation-error>
	    "A validate error is raised on numbers below a minimum.")
  (is-error (v:int "127" :max 20)
	    'validate:<validation-error>
	    "A validate error is raised on numbers above a maximum."))

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
	    'validate:<validation-error>
	    "raise a validation error on strings that aren't boolean values"))

;;; List

(subtest "List"
  (ok (v:list (list 1 2))
      "validates a simple json list.")
  (is-error (v:list (list 1 2 3 4) :length 2)
            'v:<validation-error>
            "Too long a list generates an error.")
  (is (v:list (list 1 2 3) :length 2 :truncate t)
      '(1 2)
      "Truncate truncates lists.")
  (is-error (v:list (list "a" "bc") :element-type 'v:int)
            'v:<validation-error>
            "Raises an error on invalid element types.")
  (is-error (v:list "12")
	    'v:<validation-error>
	    "Rases an error when value is not a list at all!"))

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

(finalize)
