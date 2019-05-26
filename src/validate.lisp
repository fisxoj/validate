(defpackage validate
  (:use :iterate :cl)
  (:nicknames #:v)
  (:shadow #:list)
  (:export #:parse
           #:schema
           #:<validation-error>
           #:with-validated-values
           #:str
           #:int
           #:bool
           #:email
           #:list
	   #:regex
           #:timestamp)
  (:documentation "Validate is a pretty simple library that is meant to be used in situations where you want to throw errors when data isn't quite what you expected.  For example, when you receive user input, because who knows what a user will really do!

You will likely use validate like this::
    (handler-case
       (let ((my-int (v:int some-user-data)))
         (code-expecting-an-int my-int))
       (v:<validation-error> (e)
          (report-error e)))"))

(in-package :validate)

;;; Conditions

(define-condition <validation-error> (simple-error)
  ((value :initarg :value)
   (rule :initarg :rule))

  (:report (lambda (condition stream)
             (with-slots (rule value) condition
	       (format stream "Value ~a didn't satisfy condition ~S" value rule))))

  (:documentation "Error to signal a validation condition wasn't met.
  e.g. Value 'a' didn't satisfy contition 'length at least 3 characters'"))

(defun fail (value reason-str &rest args)
  "Throw a validation error."

  (error '<validation-error>
	 :value value
	 :rule (apply #'format nil reason-str args)))

;;; Validators

(defun int (value &key (radix 10) max min)
  "Accepts a :param:`value` as an integer or string.

If :param:`value` is a string, interpret it as a value with base- :param:`radix`.

If specified, make sure :param:`min` <= :param:`value` <= :param:`max`."

  (handler-case
      (let ((number (etypecase value
                      (string (parse-integer value :radix radix))
                      (integer value))))

        (when (and max (> number max))
          (fail value "must be larger smaller than ~a" max))

        (when (and min (< number min))
          (fail value "must be larger than ~a" min))

        number)
    (parse-error (e)
      (declare (ignore e))
      (fail value "must be parseable as an integer"))))


;; https://github.com/alecthomas/voluptuous/blob/master/voluptuous.py#L1265
(defun bool (value)
  "Attempts to convert :param:`value` to a ``t`` or ``nil`` by comparing it to a list of stringy true or false values.  Throws a :class:`<validation-error>` if it can't figure out if :param:`value` is truthy or falsey."

  (cond
    ((member value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) t)
    ((member value '("n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
    (t (fail value "is not a valid boolean value"))))


(defun str (value &key min-length max-length)
  "Checks that :param:`value` is a string.

If specified, checks that :param:`min-length` <= ``(length value)`` <= :param:`max-length`."

  (unless (typep value 'string)
    (fail value "must be a string."))
  (when min-length
    (unless (>= (length value) min-length)
      (fail value "string length must be > ~d" min-length)))
  (when max-length
    (unless (<= (length value) max-length)
      (fail value "string length must be < ~d" max-length)))

  value)


(defun email (value)
  "Checks that :param:`value` resembles an email address.

This is not an exhaustive check - some characters between an ``@`` and a ``.`` will suffice."

  (str value)
  (unless (ppcre:scan ".+@.+\\\..{2,}" value)
    (fail value "string doesn't contain an email address."))
  value)


(defun regex (value regex)
  "Checks if :param:`value` matches :param:`regex`."

  (if (ppcre:scan regex value)
      value
      (fail value "string doesn't match regex ~s" regex)))


(defun list (value &key length truncate element-type)
  "Checks that :param:`value` is a list.

If :param:`length` is specified, check that the list has that length.

If :param:`length` is given and :param:`truncate` is true, truncate the list to that length.

If :param:`element-type` is given, it should be another validation function that will be called on every element of :param:`value`."

  (unless (consp value)
    (fail value "value is not a list."))
  (let ((maybe-truncated-list
	  (cond
	    ((and length truncate) (subseq value 0 length))
	    (length (if (= (length value) length)
			value
			(fail value "list length is ~d, not ~d." (length value) length)))
	    (t value))))

    (if element-type
	(handler-case
	    (mapcar element-type maybe-truncated-list)
	  (<validation-error> (e)
	    (with-slots (rule (subval value)) e
	      (fail value "Element ~a of list ~a failed rule ~S" subval value rule))))
	maybe-truncated-list)))


(defun timestamp (value)
  "Check that :param:`value` is a timestamp as recognized by `local-time`_.

.. _local-time: https://common-lisp.net/project/local-time/"


  (etypecase value
    (local-time:timestamp value)
    (string (handler-case
                (local-time:parse-timestring value)
              (local-time::invalid-timestring (c)
                (declare (ignore c))
                (fail value "value is not a valid timestamp."))))))
