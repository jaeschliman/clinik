
(map nil (lambda (p) (load (compile-file p)))
     '("ccl-user-package.lisp"
       "ccl-package-local-nicknames-api.lisp"))
