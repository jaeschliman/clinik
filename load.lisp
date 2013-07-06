
(map nil (lambda (p) (load (compile-file p)))
     '("ccl-user-package.lisp"
       "ccl-package-local-nicknames-api.lisp"
       ;;depends on a build including ccl-redefs patched in.
       "ccl-user-package-clos-integration.lisp"))
