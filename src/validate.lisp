(in-package :cl-user)
(defpackage validate
  (:use :iterate :cl)
  (:nicknames #:v)
  (:export #:parse
           #:schema
           #:<parse-error>
           #:<validation-error>
           #:str
           #:int
           #:bool
           #:email))
(in-package :validate)


(defun schema (schema data &key (from :plist) (as :plist) allow-other-fields)
  (declare (optimize debug))
  (let ((schema (funcall
                 (ecase from
                   (:plist #'alexandria:plist-alist)
                   (:alist #'identity)
                   (:hash-table #'alexandria:hash-table-alist))
                 schema)))

    (funcall
     (ecase as
       (:plist #'alexandria:alist-plist)
       (:alist #'identity)
       (:hash-table #'alexandria:alist-hash-table))

     ;; Run validation function on every entry in the schema
     (iterate (for (field . validation) in schema)
              (for value = (getf data field))

              (collect (cons field (funcall validation value)))))))


(define-condition <parse-error> (simple-error)
  ((value :initarg :value)
   (type :initarg :type))
  (:report (lambda (condition stream)
             (with-slots (value type) condition
               (format stream "Value ~a is not of type ~A" value type))))
  (:documentation "An error for conditions where a submitted value cannot be interpreted as the given type."))

(define-condition <validation-error> (simple-error)
  ((value :initarg :value)
   (rule :initarg :rule))
  (:report (lambda (condition stream)
             (with-slots (rule value) condition
                 (format stream "Value ~a didn't satisfy condition ~S" value rule))))
  (:documentation "Error to signal a validation condition wasn't met.
  e.g. Value 'a' didn't satisfy contition 'length at least 3 characters"))

(defun int (value)
  (handler-case
        (parse-integer value)
      (parse-error (e)
        (declare (ignore e))
        (error '<parse-error> :type 'integer :value value))))

;; https://github.com/alecthomas/voluptuous/blob/master/voluptuous.py#L1265
(defun bool (value)
  (let ((downcased-value (string-downcase value)))
      (cond
        ((member downcased-value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string=) t)
        ((member downcased-value '("n" "no"  "f" "false" "off" "disable") :test #'string=) nil)
        (t (error '<parse-error>  :type type :value value)))))

(defun str (value &key min-length max-length)
  (when min-length
    (unless (> (length value) min-length)
      (error '<validation-error> :rule (format nil "length must be > ~d" min-length))))
  (when max-length
    (unless (< (length value) max-length)
      (error '<validation-error> :rule (format nil "length must be < ~d" max-length))))

  value)

(defun email (value)
  (unless (ppcre:scan ".+@.+\\\..+" value)
    (error '<validation-error>
              :rule (format nil "string doesn't contain an email address")
              :value value))
  value)
