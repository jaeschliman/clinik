(in-package :ccl)


#|

  support for redefining user-package classes.  works by taking
  advantage of the existing machinery for standard-object, this file
  is basically just copy-pasta of the functions supporting standard
  instances with a few key changed to support the non-standard layout
  of the user-package class.

  requires patching and rebuilding CCL (see ccl-redefs.lisp)

  haven't looked into support for change-class yet.

|#

;; this is where it all starts. redefined from ccl-user-package.lisp
;; to support obsoleting instances. 
(defmethod make-instances-obsolete ((class user-package-class))
  (let ((wrapper (%class-own-wrapper class)))
    (when wrapper
      (setf (%class-own-wrapper class) nil)
      (%make-wrapper-obsolete-for-user-package wrapper)))
  class)

(defun %slot-id-lookup-obsolete-non-std (instance slot-id)
  (update-obsolete-instance instance)
  (funcall (%wrapper-slot-id->slotd (non-standard-instance-class-wrapper instance))
           instance slot-id))

(defun %slot-id-ref-obsolete-non-std (instance slot-id)
  (update-obsolete-instance instance)
  (funcall (%wrapper-slot-id-value (non-standard-instance-class-wrapper instance))
           instance slot-id))

(defun %slot-id-set-obsolete-non-std (instance slot-id new-value)
  (update-obsolete-instance instance)
  (funcall (%wrapper-set-slot-id-value (non-standard-instance-class-wrapper instance))
           instance slot-id new-value))

(defun %make-wrapper-obsolete-for-user-package (wrapper)
  (without-interrupts
   (let ((forwarding-info
          (unless (eql 0 (%wrapper-instance-slots wrapper))   ; already forwarded or obsolete?
            (%cons-forwarding-info (%wrapper-instance-slots wrapper)
                                   (%wrapper-class-slots wrapper)))))
     (when forwarding-info
       (setf (%wrapper-hash-index wrapper) 0
             (%wrapper-cpl wrapper) nil
             (%wrapper-cpl-bits wrapper) nil
             (%wrapper-instance-slots wrapper) 0
             (%wrapper-forwarding-info wrapper) forwarding-info
	     (%wrapper-slot-id->slotd wrapper) #'%slot-id-lookup-obsolete-non-std
	     (%wrapper-slot-id-value wrapper) #'%slot-id-ref-obsolete-non-std
	     (%wrapper-set-slot-id-value wrapper) #'%slot-id-set-obsolete-non-std
             ))))
  wrapper)

(defmethod update-instance-for-redefined-class ((instance user-package)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (declare (ignore discarded-slots property-list))
  (when initargs
    (check-initargs
     instance nil initargs t
     #'update-instance-for-redefined-class #'shared-initialize))
  (apply #'shared-initialize instance added-slots initargs))


;;cribbing from standard-object & struct-class
(defmethod make-instance ((class user-package-class) &rest initargs)
  (declare (dynamic-extent initargs))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))


(defmethod shared-initialize ((instance user-package) slot-names &rest initargs)
  (declare (dynamic-extent initargs))
  (%user-package-shared-initialize instance slot-names initargs))

(defun %user-package-shared-initialize (instance slot-names initargs)
  ;; adapting from %shared-initialize.
  (let* ((wrapper (instance-class-wrapper instance))
         (class (%wrapper-class wrapper)))

    (when (eql 0 (%wrapper-hash-index wrapper))
      (%update-obsolete-user-package-instance instance))
    
    (dolist (slotd (class-slots class))
      (let* ((predicate (slot-definition-predicate slotd)))
        (multiple-value-bind (ignore new-value foundp)
            (get-properties initargs (slot-definition-initargs slotd))
          (declare (ignore ignore))
          (cond (foundp
                 ;; an initarg for the slot was passed to this function
                 ;; Typecheck the new-value, then call
                 ;; (SETF SLOT-VALUE-USING-CLASS)
                 (unless (or (null predicate)
                             (funcall predicate new-value))
                   (error 'bad-slot-type-from-initarg
                          :slot-definition slotd
                          :instance instance
                          :datum new-value
                          :expected-type  (slot-definition-type slotd)
                          :initarg-name (car foundp)))
                 (setf (slot-value-using-class class instance slotd) new-value))
                ((and (or t (eq slot-names t) ;; initial call via initialize-instance

                          ;; FIXME: value of slot-names being passed in
                          ;;        doesn't seem to be correct, not
                          ;;        including slots with new initforms ?
                          
                          ;; (member (slot-definition-name slotd)
                          ;;         slot-names
                          ;;         :test #'eq)
                          )
                      (not (slot-boundp-using-class class instance slotd)))
                 ;; If the slot name is among the specified slot names, or
                 ;; we're reinitializing all slots, and the slot is currently
                 ;; unbound in the instance, set the slot's value based
                 ;; on the initfunction (which captures the :INITFORM).
                 (let* ((initfunction (slot-definition-initfunction slotd)))
                   (if initfunction
                       (let* ((newval (funcall initfunction)))
                         (unless (or (null predicate)
                                     (funcall predicate newval))
                           (error 'bad-slot-type-from-initform
                                  :slot-definition slotd
                                  :expected-type (slot-definition-type slotd)
                                  :datum newval
                                  :instance instance))
                         (setf (slot-value-using-class class instance slotd)
                               newval)))))))))
    instance))


(defgeneric update-obsolete-non-standard-instance-for-class (class instance))

(defmethod update-obsolete-non-standard-instance-for-class ((class user-package-class)
                                                            instance)
  (%update-obsolete-user-package-instance instance))

;;hook defined in ccl-redefs.lisp
(setf *update-obsolete-non-standard-instance-function*
      (lambda (i)
        (update-obsolete-non-standard-instance-for-class (class-of i) i)))




(defun %update-obsolete-user-package-instance (instance)
  (let* ((added ())
	 (discarded ())
	 (plist ()))
    (without-interrupts			; Not -close- to being correct
      (let* ((old-wrapper (%user-package-class-wrapper instance)))
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

                 ;;temporary instance to swap fields with at the end.
                 (new-instance (allocate-instance class))
                 (old-slot-vector (%user-package-slots instance))
                 (new-slot-vector (%user-package-slots new-instance)))
            
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
              (%exchange-user-package-slot-vectors-and-wrappers new-instance instance))))))
    ;; run user code with interrupts enabled.
    (update-instance-for-redefined-class instance added discarded plist))
  instance)

(defun %exchange-user-package-slot-vectors-and-wrappers (a b)
  (let* ((temp-wrapper (%user-package-class-wrapper a))
         (orig-a-slots (%user-package-slots a))
         (orig-b-slots (%user-package-slots b)))
    (setf (%user-package-class-wrapper a) (%user-package-class-wrapper b)
          (%user-package-class-wrapper b) temp-wrapper
          (%user-package-slots a) orig-b-slots
          (%user-package-slots b) orig-a-slots
          (slot-vector.instance orig-a-slots) b
          (slot-vector.instance orig-b-slots) a)))


