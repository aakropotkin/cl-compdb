
(in-package :cl-user)
(ql:quickload '(:prove :compdb/struct-utils))
(defpackage :compdb/test/test-struct-utils
  (:USE :common-lisp :prove :compdb/struct-utils))
(in-package :compdb/test/test-struct-utils)


;; ========================================================================== ;;

(defstruct foo
  (x 0 :TYPE fixnum)
  y)

(subtest "Test `update-struct' function."
  (let* ((f1 (make-foo :X 5 :Y "Hello"))
         (f2 (update-struct f1 'y "World")))
    (is (foo-x f1) (foo-x f2))
    (isnt (foo-y f1) (foo-y f2) :TEST #'equal)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
