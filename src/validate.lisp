(in-package :cl-user)
(defpackage validate
  (:use :cl)
  (:export #:parse
           #:validate
           #:<parse-error>))
(in-package :validate)

(defun validate (data &key (as :plist))

  )

(defun validate-field (object definiton)

  )


(define-condition <parse-error> (simple-error)
  ((value :initarg :value)
   (type :initarg :type))
  (:report (lambda (condition stream)
             (with-slots (value type) condition
                 (format stream "Value ~a is not of type ~A" value type))))
  (:documentation "An error for conditions where a submitted value cannot be interpreted as the given type."))

;; https://github.com/alecthomas/voluptuous/blob/master/voluptuous.py#L1265
(defgeneric parse (value type)
  (:method (value type)
    value)

  (:method (value (type (eql 'integer)))
    (handler-case
        (parse-integer value)
      (parse-error (e)
        (declare (ignore e))
        (error '<parse-error> :type type :value value))))

  (:method (value (type (eql 'boolean)))
    (let ((downcased-value (string-downcase value)))
      (cond
        ((member downcased-value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string=) t)
        ((member downcased-value '("n" "no"  "f" "false" "off" "disable") :test #'string=) nil)
        (t (error '<parse-error>  :type type :value value))))))
