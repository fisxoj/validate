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
           #:timestamp
	   #:default))

(in-package :validate)

;;; Conditions

(define-condition <validation-error> (simple-error)
  ((value :initarg :value)
   (rule :initarg :rule))

  (:report (lambda (condition stream)
             (with-slots (rule value) condition
	       (format stream "Value ~a didn't satisfy condition ~S" value rule))))

  (:documentation "Error to signal a validation condition wasn't met.
  e.g. Value 'a' didn't satisfy contition 'length at least 3 characters"))

(defun fail (value reason-str &rest args)
  "Throw a validation error."

  (error '<validation-error>
	 :value value
	 :rule (apply #'format nil reason-str args)))

;;; Validators

(defun int (value &key (radix 10) max min)
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
  (cond
    ((member value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) t)
    ((member value '("n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
    (t (fail value "is not a valid boolean value"))))

(defun str (value &key min-length max-length)
  (when min-length
    (unless (>= (length value) min-length)
      (fail value "string length must be > ~d" min-length)))
  (when max-length
    (unless (<= (length value) max-length)
      (fail value "string length must be < ~d" max-length)))

  value)

(defun email (value)
  (unless (ppcre:scan ".+@.+\\\..{2,}" value)
    (fail value "string doesn't contain an email address."))
  value)

(defun regex (value regex)
  (if (ppcre:scan regex value)
      value
    (fail value "string doesn't match regex ~s" regex)))

(defun list (value &key length truncate element-type)
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
  (handler-case
      (local-time:parse-timestring value)
    (local-time::invalid-timestring (c)
      (declare (ignore c))
      (fail value "value is not a valid timestamp."))))

(defun default (value &optional (default-value ""))
  "Provides a value if none is present."
  (if (alexandria:emptyp value)
      default-value
      value))
