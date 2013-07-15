(in-package :ccl)

#|

basic proof of concept of extending CCL's
package system to allow subclassing to create
special user package classes.

works by creating a metaclass for subclassing
the built in package class, which allocates
two more fields than the built in package class,
one to hold a reference to the new subclass,
one to hole a reference to instance data (slots).

replaces the class-lookup in *class-table* to check
the length of a package vector, and if it's not equal
to the usual size (8) it grabs the class reference out
of the new slot. uses a plist to store slot data (for now)

|#


(defun jsn-alloc-pkg (pkg-name class extra)
  "allocate a package gvector with two extra
fields at the end: the new class, and some slot storage"
  (gvector :package
           (%new-package-hashtable 60)
           (%new-package-hashtable 10)
           nil nil
           (list pkg-name)
           nil
           (make-read-write-lock)
           nil
           ;; the two new slots
           class extra))

;; new class lookup function for packages.
;; very hacky. just check if the uvsize is
;; the old size, or return our new class object
(defun jsn-class-of-pkg (p)
  (if (= (uvsize p) 8)
      #.(find-class 'package)
      (uvref p 8)))

;; install the hacky function in ccl's primitive
;; class-table.
;; for primitive objects with type-keyword-code's
;; in a certain range (<= 255 IIUC), cl:class-of
;; will use the type-code of the object as an index
;; into this table, which may contain either an actual
;; class object, or a function of one argument.
;; so here we install our function at the proper index
;; table. (previously there was just a class object)
(defun jsn-install-pkg-class-fn ()
  (let ((ofs (type-keyword-code :package)))
    (setf (svref *class-table* ofs)
          #'jsn-class-of-pkg)
    ))

(defclass jsn-pkg-metaclass (standard-class) ())

;;; TODO: restrict this to the package class etc.
(defmethod validate-superclass ((is jsn-pkg-metaclass) other)
  (declare (ignore is other))
  t)

(defmethod validate-superclass (other (is jsn-pkg-metaclass))
  (declare (ignore is other))
  t)


;;basic package subclass
(defclass jsn-pkg-class (package) ()
  (:metaclass jsn-pkg-metaclass))

;;package subclass with a slot POC
(defclass jsn-pkg-2 (jsn-pkg-class)
  ((hmmm))
  (:metaclass jsn-pkg-metaclass))


;;plug our custom allocator into CLOS via the new metaclass.
;;we pass the current class in, so it can be stored in the
;;primitive vector and referenced by the function we stored in *class-table*
;;the final argument, (list), is storage for the slots of custom
;;instances. a thorough implementation would have more concern for
;;effeciency here.
;; also worth noting that setting the name in allocate-instance
;; is a bit out of place. could just move it down...
(defmethod allocate-instance ((this jsn-pkg-metaclass) &rest args)
  (jsn-alloc-pkg (string (or (getf args :name)
                             (gensym "anonymous")))
                 this ;; the class
                 (list) ;; should eventually be a slot vector ?
                 ))

;; declare that we need a keyword argument :name to make-instance,
;; (though it's actually used in allocate instance for now)
;; note that we don't actually plug our new package object into
;; the global package list (which is done in make-package etc),
;; so (find-package xxx) won't work on our new objects yet.
(defmethod initialize-instance ((this jsn-pkg-class) &key name &allow-other-keys)
  (declare (ignore name)))

;; override the primitive print-object method for packages
;; so we can see our instances properly.
(defmethod print-object ((a jsn-pkg-class) stream)
  (print-unreadable-object (a stream :type t)
    (prin1 (package-name a) stream)))

;; hacky hideous plist as a slot vector, so we can
;; use slots in our new class.
(defun jsn-pkg-slot-list (jsn-pkg)
  (uvref jsn-pkg 9))

(defmethod slot-value-using-class ((meta jsn-pkg-metaclass)
                                   (pkg  jsn-pkg-class)
                                   slotd)
  (let ((slots (jsn-pkg-slot-list pkg))
        (name  (slot-definition-name slotd)))
    (getf slots name)))

(defmethod (setf slot-value-using-class) (value
                                          (meta jsn-pkg-metaclass)
                                          (pkg  jsn-pkg-class)
                                          slotd)
  (let ((name  (slot-definition-name slotd)))
    (setf (getf (uvref pkg 9) name) value)))

;;; don't actually obsolete instances.
(defmethod make-instances-obsolete ((class jsn-pkg-metaclass))
  class)
