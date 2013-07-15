(in-package :ccl)

(error "Do not load this file")


;;;; internal functions that require redefinition for the metaclass stuff to work.

#|

  In the current implementation of user-package classes I'm working to support
  some forms of class redefinition, similar to that of standard-object. this
  may be more trouble than it's worth in the end, and it's not needed to support
  package-local-nicknames, but it's a nice-to-have, and I'm having fun doing it.

  once things get far enough along, I'll provide to build paths for the project,
  one including the fancier clos integration (which requires the redefs in this
  file) and one which does not (more in the style of defstruct)

  about this file:

  This file hooks into a few places in CCL internals to allow the user-package
  objects to take advantage of the class-redefinition machinery already in place
  for standard-object. Essentially requires hooking into a few places where a
  standard instance layout is assumed, and providing support for the user-package
  layout.

  how to use this file:

  This requires patching your CCL sources and calling (ccl:rebuild-ccl), and restarting.
  Easiest way for now is just M-. on the fn's below, and replace their definitions.
  I've been using 1.9 release darwin x8632.

|#



(defun non-standard-instance-class-wrapper (instance)
  (let* ((typecode (typecode instance)))
    (declare (type (unsigned-byte 8) typecode))
    (cond ((eql typecode target::subtag-struct)
           (%class.own-wrapper
            (class-cell-class (car (%svref instance 0)))))
          ((eql typecode target::subtag-istruct)
           (istruct-cell-info (%svref instance 0)))
          ((eql typecode target::subtag-basic-stream)
           (basic-stream.wrapper instance))
          ((typep instance 'funcallable-standard-object)
           (gf.instance.class-wrapper instance))
          ((eql typecode target::subtag-macptr) (foreign-instance-class-wrapper instance))

          ;; need to insert way to grab wrapper from instance, not class, as wrapper
          ;; of class will be nulled on redefinition.
          ;; would be good to have a more general solution than this, perhaps similar
          ;; to how class-of works, with a dispatch table, but this function
          ;; is in enough critical paths that seems better to just follow
          ;; the above methods and special-case it.

          ((and (eql typecode target::subtag-package) (> (uvsize instance) 8))
           (%svref instance 8))
          
          (t (%class.own-wrapper (class-of instance))))))


;; this defstatic isn't being redefined, just needs to be included above
;; the redef of update-obsolete-instance.

(defstatic *update-obsolete-non-standard-instance-function* nil)

(defun update-obsolete-instance (instance)

  ;; changed to check if instance is a standard-instance, and if
  ;; not, funcall the fn in the static var defined above if it exists.
  ;; a value for the above is provided in ccl-user-package-clos-integration.lisp
  
  (if (%standard-instance-p instance)
      (let* ((added ())
             (discarded ())
             (plist ()))
        (without-interrupts             ; Not -close- to being correct
          (let* ((old-wrapper (standard-object-p instance)))
            (unless old-wrapper
              (when (typep instance 'funcallable-standard-object)
                (setq old-wrapper (gf.instance.class-wrapper instance)))
              (unless old-wrapper
                (report-bad-arg instance '(or standard-object funcallable-standard-object))))
            (when (eql 0 (%wrapper-instance-slots old-wrapper)) ; is it really obsolete?
              (let* ((class (%wrapper-class old-wrapper))
                     (new-wrapper (or (%class.own-wrapper class)
                                      (progn
                                        (update-class class t)
                                        (%class.own-wrapper class))))
                     (forwarding-info (%wrapper-forwarding-info old-wrapper))
                     (old-class-slots (%forwarding-class-slots forwarding-info))
                     (old-instance-slots (%forwarding-instance-slots forwarding-info))
                     (new-instance-slots (%wrapper-instance-slots new-wrapper))
                     (new-class-slots (%wrapper-class-slots new-wrapper))
                     (new-instance (allocate-instance class))
                     (old-slot-vector (instance-slots instance))
                     (new-slot-vector (instance-slots new-instance)))
                ;; Lots to do.  Hold onto your hat.
                (let* ((old-size (uvsize old-instance-slots))
                       (new-size (uvsize new-instance-slots)))
                  (declare (fixnum old-size new-size))
                  (dotimes (i old-size)
                    (declare (fixnum i))
                    (let* ((slot-name (%svref old-instance-slots i))
                           (pos (%vector-member slot-name new-instance-slots))
                           (val (%svref old-slot-vector (%i+ i 1))))
                      (if pos
                          (setf (%svref new-slot-vector (%i+ pos 1)) val)
                          (progn
                            (push slot-name discarded)
                            (unless (eq val (%slot-unbound-marker))
                              (setf (getf plist slot-name) val))))))
                  ;; Go through old class slots
                  (dolist (pair old-class-slots)
                    (let* ((slot-name (%car pair))
                           (val (%cdr pair))
                           (pos (%vector-member slot-name new-instance-slots)))
                      (if pos
                          (setf (%svref new-slot-vector (%i+ pos 1)) val)
                          (progn
                            (push slot-name discarded)
                            (unless (eq val (%slot-unbound-marker))
                              (setf (getf plist slot-name) val))))))
                                        ; Go through new instance slots
                  (dotimes (i new-size)
                    (declare (fixnum i))
                    (let* ((slot-name (%svref new-instance-slots i)))
                      (unless (or (%vector-member slot-name old-instance-slots)
                                  (assoc slot-name old-class-slots))
                        (push slot-name added))))
                  ;; Go through new class slots
                  (dolist (pair new-class-slots)
                    (let ((slot-name (%car pair)))
                      (unless (or (%vector-member slot-name old-instance-slots)
                                  (assoc slot-name old-class-slots))
                        (push slot-name added))))
                  (exchange-slot-vectors-and-wrappers new-instance instance))))))
        ;; run user code with interrupts enabled.
        (update-instance-for-redefined-class instance added discarded plist))

      ;;JSN: XXX
      ;;otherwise:

      ;; at the point where this is defined in the clos boot process, generic
      ;; functions are not yet available, and during runtime gf dispatch is
      ;; not available for obsolete instances (unsurpisingly)
      ;; a function for *u-o-n-s-i-f* is included in ccl-user-package-clos-integration.lisp
      ;; which dispatches on the class of the instance
      
      (if *update-obsolete-non-standard-instance-function*
          (funcall *update-obsolete-non-standard-instance-function* instance)
          (error "Can't update non-standard instance. need to finish booting.")))
  instance)


;; to take advantage of the redef machinery, need to be able to fetch the class
;; wrapper from an instance, which in our case is not in the same place as a
;; standard instance. slot-value and slot-boundp, set-slot-value

(defun slot-value (instance slot-name)
  (let* ((wrapper
          (let* ((w (instance-class-wrapper instance)))
            (if (eql 0 (%wrapper-hash-index w))
                ;;check for location of wrapper
                (if (%standard-instance-p instance)
                    (instance.class-wrapper (update-obsolete-instance instance))
                    (instance-class-wrapper (update-obsolete-instance instance)))
              w)))
         (class (%wrapper-class wrapper))
         (slotd (find-slotd slot-name (if (%standard-instance-p class)
                                        (%class.slots class)
                                        (class-slots class)))))
    (if slotd
      (%maybe-std-slot-value-using-class class instance slotd)
      (if (typep slot-name 'symbol)
        (restart-case
         (values (slot-missing class instance slot-name 'slot-value))
         (continue ()
                   :report "Try accessing the slot again"
                   (slot-value instance slot-name))
         (use-value (value)
                    :report "Return a value"
                    :interactive (lambda ()
                                   (format *query-io* "~&Value to use: ")
                                   (list (read *query-io*)))
                    value))
        (report-bad-arg slot-name 'symbol)))))

(defun slot-boundp (instance name)
  (let* ((wrapper
          (let* ((w (instance-class-wrapper instance)))
            (if (eql 0 (%wrapper-hash-index w))
                ;;check for location of wrapper
                (if (%standard-instance-p instance)
                    (instance.class-wrapper (update-obsolete-instance instance))
                    (instance-class-wrapper (update-obsolete-instance instance)))
              w)))
         (class (%wrapper-class wrapper))
         (slotd (find-slotd name (if (%standard-instance-p class)
                                   (%class.slots class)
                                   (class-slots class)))))
    (if slotd
      (%maybe-std-slot-boundp-using-class class instance slotd)
      (if (typep name 'symbol)
        (values (slot-missing class instance name 'slot-boundp))
        (report-bad-arg name 'symbol)))))

(defun set-slot-value (instance name value)
  (let* ((wrapper
          (let* ((w (instance-class-wrapper instance)))
            (if (eql 0 (%wrapper-hash-index w))
                ;;check for location of wrapper
                (if (%standard-instance-p instance)
                    (instance.class-wrapper (update-obsolete-instance instance))
                    (instance-class-wrapper (update-obsolete-instance instance)))
              w)))
         (class (%wrapper-class wrapper))
         (slotd (find-slotd name (if (%standard-instance-p class)
                                   (%class.slots class)
                                   (class-slots class)))))
    (if slotd
      (%maybe-std-setf-slot-value-using-class class instance slotd value)
      (if (typep name 'symbol)
        (progn	    
          (slot-missing class instance name 'setf value)
          value)
        (report-bad-arg name 'symbol)))))

;;;;;;; slime-xref bug in my current combo of ccl/slime (not a redef)
;;;;;;; was throwing errors for definition-type-instance eql compiler-macro-function,
;;;;;;; workaround to let those specific cases slide for now.

(defmethod definition-type-instance ((name (eql 'compiler-macro-function)) &key (if-does-not-exist :error))
  (or (cdr (assq name *definition-types*))
      (ecase if-does-not-exist
        ((nil) nil)
        ((:error) nil ;;eeww
         )
        ((:create) (auto-create-definition-type name)))))

