(in-package :cl-user)
(defpackage validate
  (:use :iterate :cl)
  (:nicknames #:v)
  (:export #:parse
           #:schema
           #:<validation-error>
           #:with-validated-values
           #:str
           #:int
           #:bool
           #:email
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


;;; Helpers/Public API

(defmacro with-validated-values (bindings (schema data &optional (from :plist)) &body body)
  "Macro for extracting values from some map/hash like object and binding them lexically.

Applies `schema` to `data` and binds to bindings."

  (alexandria:with-gensyms (validated)

    `(let* ((,validated (schema ,schema ,data :from ,from :as :hash-table))

            ,@(mapcar (lambda (binding)
                        (if (listp binding)
                            `(,(first binding)
                               (gethash ,(alexandria:make-keyword (second binding))
                                        ,validated))
                            `(,binding
                              (gethash ,(alexandria:make-keyword binding)
                                       ,validated))))
                      bindings))
       ,@body)))


(defun schema (schema data &key (from :plist) (as :plist) allow-other-fields)
  "Run a set of data through a schema.  Return some associative structure with the validated fields in it."

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

	      (collect
		  (iterate (for (validation-function . args) in (alexandria:ensure-list validation))
			   (for validated-value initially value then (apply validation-function validated-value args))
			   (finally (return (cons field validated-value)))))))))

;;; Validators

(defun int (value &key (radix 10))
  (handler-case
        (parse-integer value :radix radix)
      (parse-error (e)
        (declare (ignore e))
        (error '<validation-error> :rule "Invalid integer" :value value))))

;; https://github.com/alecthomas/voluptuous/blob/master/voluptuous.py#L1265
(defun bool (value)
  (let ((downcased-value (string-downcase value)))
      (cond
        ((member downcased-value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string=) t)
        ((member downcased-value '("n" "no"  "f" "false" "off" "disable") :test #'string=) nil)
        (t (error '<validation-error>  :rule "Invalid boolean value" :value value)))))

(defun str (value &key min-length max-length)
  (when min-length
    (unless (> (length value) min-length)
      (error '<validation-error> :rule (format nil "length must be > ~d" min-length) :value value)))
  (when max-length
    (unless (< (length value) max-length)
      (error '<validation-error> :rule (format nil "length must be < ~d" max-length) :value value)))

  value)

(defun email (value)
  (unless (ppcre:scan ".+@.+\\\..{2,}" value)
    (error '<validation-error>
	   :rule "string doesn't contain an email address."
	   :value value))
  value)

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

  (if (and value (not (zerop (length value))))
      value
      default-value))
