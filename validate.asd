#|
  This file is a part of validate project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(in-package :cl-user)
(defpackage validate-asd
  (:use :cl :asdf))
(in-package :validate-asd)

(defsystem validate
  :version "0.1"
  :author "Matt Novenstern"
  :license "LLGPLv3"
  :depends-on (:iterate :cl-ppcre :alexandria)
  :components ((:module "src"
                :components
                ((:file "validate"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op validate-test))))
