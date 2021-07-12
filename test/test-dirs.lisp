
(in-package :cl-user)
(ql:quickload '(:prove :compdb/dirs))
(defpackage :compdb/test/test-dirs
  (:USE :common-lisp :prove :compdb/dirs))
(in-package :compdb/test/test-dirs)


;; ========================================================================== ;;

(subtest "Test `directory-component' type."
  NIL)


;; -------------------------------------------------------------------------- ;;

(subtest "Test `list-of-directory-component' type."
  )

;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
