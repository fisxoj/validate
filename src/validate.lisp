(in-package :cl-user)
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

  (let ((data (funcall
		 (ecase from
		   (:plist #'identity)
		   (:alist #'alexandria:alist-plist)
		   (:hash-table #'alexandria:hash-table-alist))
		 data)))

    (funcall
     (ecase as
       (:plist #'alexandria:alist-plist)
       (:alist #'identity)
       (:hash-table #'alexandria:alist-hash-table))

     ;; Run validation function on every entry in the schema
     (iterate (for (field validations) on schema by #'cddr)
	      (for value = (getf data field))
	      (collect
		  (iterate (for validation in validations)
			   (with validated-value = value)
                           (setf validated-value
                                 (if (consp validation)
                                     (destructuring-bind (func &rest args) validation
                                       ;; (format t "~&Calling ~a with args ~a~%" func args)
                                         (apply func validated-value args))
                                     (funcall validation validated-value)))
			   (finally (return (cons field validated-value)))))))))

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
  (let ((downcased-value (string-downcase value)))
      (cond
        ((member downcased-value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) t)
        ((member downcased-value '("n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
        (t (error '<validation-error>  :rule "Invalid boolean value" :value value)))))

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

  (if (and value (not (alexandria:emptyp value)))
      value
      default-value))
