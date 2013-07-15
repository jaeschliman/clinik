(in-package :ccl)

;; make built-in-packages subclassable, create user-package class,
;; which supports package-local-nicknames.
;;
;; TODO: slot support, class-redef, etc.

(defun %simple-package-p (package)
  (= (uvsize package) 8))
(declaim (inline %simple-package-p))

(defmacro %user-package-class-wrapper (package)
  `(%svref ,package 8))
(defmacro %user-package-slots (package)
  `(%svref ,package 9))
(defmacro %user-package-local-nicknames (package)
  `(%svref ,package 10))

(defun %class-of-package (package)
  (declare (optimize (speed 3) (safety 0)))
  (if (%simple-package-p package)
      #.(find-class 'package)
      (%wrapper-class (%user-package-class-wrapper package))))

(defun %%install-package-lookup ()
  (let ((ofs (type-keyword-code :package)))
    (setf (svref *class-table* ofs)
          #'%class-of-package)))

(%%install-package-lookup)

;; (def-accessors (package) %svref
;;   pkg.itab
;;   pkg.etab
;;   pkg.used
;;   pkg.used-by
;;   pkg.names
;;   pkg.shadowed
;;   pkg.lock
;;   pkg.intern-hook
;;   )

(defun %alloc-user-package (class)
  ;;cribbing from %make-package and %allocate-std-instance
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((wrapper (%class.own-wrapper class))
         (len (length (%wrapper-instance-slots wrapper)))
         slots instance)
    (declare (fixnum len))
    (setf instance
          (gvector :package
                   (%new-package-hashtable 60)
                   (%new-package-hashtable 10)
                   nil
                   nil
                   nil
                   nil
                   (make-read-write-lock)
                   nil
                   wrapper nil nil))
    (setf slots
          (apply #'%gvector (type-keyword-code :slot-vector)
                 instance (make-list len :initial-element (%slot-unbound-marker))))
    (setf (%user-package-slots instance) slots)
    instance))

(defclass user-package-class (standard-class) ())

(defmethod validate-superclass ((c user-package-class) other)
  (member #.(find-class 'package) (class-precedence-list other)))

(defmethod allocate-instance ((class user-package-class) &rest initargs)
  (declare (ignore initargs))
  (%alloc-user-package class))

(defclass user-package (package) ()
  (:metaclass user-package-class))

(defmethod shared-initialize  ((instance user-package) slot-names &rest initargs)
  ;;redefined in clos-integration.lisp
  (declare (dynamic-extent initargs)
           (ignore instance slot-names initargs)))

(defmethod initialize-instance ((instance user-package)
                                &rest initargs
                                &key
                                  (name (string (gensym "anonymous-package:")))
                                  (nicknames (list))
                                  (local-nicknames (list))
                                  &allow-other-keys)

  (apply #'shared-initialize instance t initargs)
  (setf (pkg.names instance) (cons name nicknames)
        (%user-package-local-nicknames instance) local-nicknames))

(defmethod print-object ((a user-package) stream)
  (print-unreadable-object (a stream :type t)
    (prin1 (package-name a) stream)))


(defmethod slot-value-using-class ((class user-package-class)
				   instance
				   (slotd standard-effective-slot-definition))
  (ecase (standard-slot-definition.allocation slotd)
    ((:instance :class)
     (%std-slot-vector-value (%user-package-slots instance) slotd))))


(defmethod (setf slot-value-using-class)
    (new
     (class user-package-class)
     instance
     (slotd standard-effective-slot-definition))
  (ecase (standard-slot-definition.allocation slotd)
    ((:instance :class)
     (%set-std-slot-vector-value (%user-package-slots instance) slotd new))))

(defmethod slot-boundp-using-class ((class user-package-class)
                                    instance
				    (slotd standard-effective-slot-definition))
  (ecase (standard-slot-definition.allocation slotd)
    ((:instance :class)
     (%std-slot-vector-boundp (%user-package-slots instance) slotd))))


;;; don't actually obsolete instances. support for this in ccl-user-package-clos-integration.lisp
(defmethod make-instances-obsolete ((class user-package-class))
  class)
