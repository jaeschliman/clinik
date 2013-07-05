(in-package :ccl)

;; make built-in-packages subclassable, create user-package class,
;; which supports package-local-nicknames.
;;
;; TODO: slot support, class-redef, etc.

(defun %simple-package-p (package)
  (= (uvsize package) 8))
(declaim (inline %simple-package-p))

(defmacro %user-package-class (package)
  `(%svref ,package 8))
(defmacro %user-package-slots (package)
  `(%svref ,package 9))
(defmacro %user-package-local-nicknames (package)
  `(%svref ,package 10))

(defun %class-of-package (package)
  (declare (optimize (speed 3) (safety 0)))
  (if (%simple-package-p package)
      #.(find-class 'package)
      (%user-package-class package)))

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
  (gvector :package
           (%new-package-hashtable 60)
           (%new-package-hashtable 10)
           nil
           nil
           nil
           nil
           (make-read-write-lock)
           nil
           class nil nil))

(defclass user-package-class (standard-class) ())

(defmethod validate-superclass ((c user-package-class) other)
  (member #.(find-class 'package) (class-precedence-list other)))

(defmethod allocate-instance ((class user-package-class) &rest initargs)
  (declare (ignore initargs))
  (%alloc-user-package class))

(defclass user-package (package) ()
  (:metaclass user-package-class))

(defmethod initialize-instance ((instance user-package)
                                &key
                                  (name (string (gensym "anonymous-package:")))
                                  (nicknames (list))
                                  (local-nicknames (list)))
  (setf
   (pkg.names instance) (cons name nicknames)
   (%user-package-local-nicknames instance) local-nicknames))

(defmethod print-object ((a user-package) stream)
  (print-unreadable-object (a stream :type t)
    (prin1 (package-name a) stream)))



