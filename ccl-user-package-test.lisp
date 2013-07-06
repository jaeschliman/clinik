(defpackage :ccl-user-package-test
  (:use :cl)
  (:import-from :ccl :user-package :user-package-class))

(in-package :ccl-user-package-test)


(defclass a (user-package)
  (a)
  (:metaclass user-package-class))

(defparameter *a* (make-instance 'a :name "A package"))

(assert (null (slot-boundp *a* 'a)))

(defclass a (user-package)
  ((a :initform 42) b)
  (:metaclass user-package-class))

(assert (null (slot-boundp *a* 'b)))

(assert (= 42 (slot-value *a* 'a)))

