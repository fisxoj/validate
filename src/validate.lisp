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

;;; Validators

(defun int (value &key (radix 10) max min)
  (handler-case
      (let ((number (etypecase value
                      (string (parse-integer value :radix radix))
                      (integer value))))

        (when (and max (> number max))
          (error '<validation-error> :rule "Value too small" :value value))

        (when (and min (< number min))
          (error '<validation-error> :rule "Value too large" :value value))

        number)
    (parse-error (e)
      (declare (ignore e))
      (error '<validation-error> :rule "Invalid integer" :value value))))

;; https://github.com/alecthomas/voluptuous/blob/master/voluptuous.py#L1265
(defun bool (value)
  (cond
    ((member value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) t)
    ((member value '("n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
    (t (error '<validation-error>  :rule "Invalid boolean value" :value value))))

(defun str (value &key min-length max-length)
  (when min-length
    (unless (>= (length value) min-length)
      (error '<validation-error> :rule (format nil "length must be > ~d" min-length) :value value)))
  (when max-length
    (unless (<= (length value) max-length)
      (error '<validation-error> :rule (format nil "length must be < ~d" max-length) :value value)))

  value)

(defun email (value)
  (unless (ppcre:scan ".+@.+\\\..{2,}" value)
    (error '<validation-error>
	   :rule "string doesn't contain an email address."
	   :value value))
  value)

(defun regex (value regex)
  (if (ppcre:scan regex value)
      value
    (error '<validation-error> :rule (format nil "string doesn't match regex ~s" regex))))

(defun list (value &key length truncate element-type)
  (let ((list (jojo:parse value)))
    (unless (consp list)
      (error '<validation-error>
             :rule "value is not a list."
             :value value))
    (let ((maybe-truncated-list
           (cond
             ((and length truncate) (subseq list 0 length))
             (length (if (= (length list) length)
                         list
                         (error '<validation-error>
                                :rule (format nil "list length is ~d, not ~d."
                                              (length list) length)
                                :value value)))
             (t list))))

      (if element-type
          (handler-case
              (mapcar element-type maybe-truncated-list)
            (<validation-error> (e)
              (with-slots (rule (subval value)) e
                (error '<validation-error>
                       :rule (format nil "Element ~a of list ~a failed rule ~S" subval value rule)
                       :value value))))
          maybe-truncated-list))))

(defun timestamp (value)
  (handler-case
      (local-time:parse-timestring value)
    (local-time::invalid-timestring (c)
      (declare (ignore c))
      (error '<validation-error>
             :rule "parameter doesn't contain a valid timestamp."
             :value value))))

(defun default (value &optional (default-value ""))
  "Provides a value if none is present."
  (if (alexandria:emptyp value)
      default-value
      value))
