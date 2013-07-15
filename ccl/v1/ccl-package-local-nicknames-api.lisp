(in-package :ccl)

;;  implement some of the :package-local-nicknames api using the user-package
;;  class, as seen in
;;  https://github.com/sbcl/sbcl/commit/b0b221088b889b6d3ae67e551b93fe1a6cfec878
;;
;;  differs from sbcl in that primitive package objects won't be able
;;  to have package-local-nicknames added to them.
;;  this could be changed by redefining the built-in package class
;;  to support nicknames as well.
;;
;;  this just implemenents the supporting functions for nickname handling,
;;  altering the internal find-package, defpackage etc will be done in another
;;  file.
;;
;; TODO: some sbcl-isms in the original source I'm not familiar with,
;;       like (sane-package), (with-package-graph () ..) and
;;       (with-single-package-locked-error ...)


(defvar *package-nicknamed-by-table* (make-hash-table))
(defun package-locally-nicknamed-by-list (pkg)
  (let ((p (pkg-arg pkg)))
    (copy-list (gethash p *package-nicknamed-by-table*))))

(defun package-%locally-nicknamed-by (package)
  (gethash package *package-nicknamed-by-table*))

(defun (setf package-%locally-nicknamed-by) (list package)
  (setf (gethash package *package-nicknamed-by-table*) list))

(defmacro package-%local-nicknames (package)
  `(%svref ,package 10))

(defun add-package-local-nickname (local-nickname actual-package
                                   &optional (package-designator *package*))
  (let ((package (pkg-arg package-designator)))

    ;; differ from sbcl here, can't add local nickname to
    ;; the primitive package type.
    
    (when (%simple-package-p package)
      (error "Cannot add package-local nickname to a primitive package."))
    
    (let* ((actual (pkg-arg actual-package))
           (nick (string local-nickname))
           (existing (package-%local-nicknames package))
           (cell (assoc nick existing :test #'string=)))

      (when (member nick '("CL" "COMMON-LISP" "KEYWORD") :test #'string=)
        (cerror "Continue, use is as local nickname anyways."
                "Attempt to use ~A as a package local nickname." nick))
      
      (when (and cell (neq actual (cdr cell)))
        (restart-case
            (error "~@<Cannot add ~A as local nickname for ~A in ~S: already nickname for ~A.~:@>"
                   nick actual package (cdr cell))
          (keep-old ()
            :report (lambda (s)
                      (format s "Keep ~A as local nickname for ~A"
                              nick (cdr cell))))
          (change-nick ()
            :report (lambda (s)
                      (format s "Use ~A as local nickname for ~A instead."
                              nick package))
            (let ((old (cdr cell)))
              ;;TODO with-package-graph,
              (setf (package-%locally-nicknamed-by old)
                    (delete package (package-%locally-nicknamed-by old)))
              (push package (package-%locally-nicknamed-by actual))
              (setf (cdr cell) actual))))
        (return-from add-package-local-nickname package))
      
      (unless cell
        ;;TODO with-package-graph
        (push (cons nick actual) (package-%local-nicknames package))
        (push package (package-%locally-nicknamed-by actual)))
      package)))

(defun remove-package-local-nickname (old-nickname
                                      &optional (package-designator *package*))
  (let ((nick (string old-nickname))
        (package (pkg-arg package-designator)))
    (unless (%simple-package-p package)
      (let* ((existing (package-%local-nicknames package))
             (cell (assoc nick existing :test #'string=)))
        (when cell
          ;;TODO with-single-package lock
          ;;TODO with-package-graph
          (let ((old (cdr cell)))
            (setf (package-%local-nicknames package) (delete cell existing))
            (setf (package-%locally-nicknamed-by old)
                  (delete package (package-%locally-nicknamed-by old))))
          t)))))

(defun package-local-nicknames (pkg)
  (let ((p (pkg-arg pkg)))
    (unless (%simple-package-p p)
      (copy-tree (%user-package-local-nicknames p)))))

