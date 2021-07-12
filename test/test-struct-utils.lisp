
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

(subtest "Test `struct-slots' function."
  (let ((f1 (make-foo :X 5 :Y "Hello"))
        (f2 (make-foo :X 0 :Y 8)))
    (let ((slots (struct-slots f1)))
      (is-type (caar slots) 'symbol)
      (is-type (caadr slots) 'symbol)
      (is (caar slots) 'X)
      (is (caadr slots) 'Y)
      (is-type (cadar slots) 'compiled-function)
      (is-type (cadadr slots) 'compiled-function))
    (let ((slots (struct-slots f2)))
      (is-type (caar slots) 'symbol)
      (is-type (caadr slots) 'symbol)
      (is (caar slots) 'X)
      (is (caadr slots) 'Y)
      (is-type (cadar slots) 'compiled-function)
      (is-type (cadadr slots) 'compiled-function))))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `struct-values' function."
  (let ((f1 (make-foo :X 5 :Y "Hello"))
        (f2 (make-foo :X 0 :Y 8)))
    (let ((vals (struct-values f1)))
      (is-type vals 'list)
      (is-type (car vals) 'symbol)
      (is (car vals) 'foo)
      (is (caadr vals) 'x)
      (is (cdadr vals) 5)
      (is (caaddr vals) 'y)
      (is (cdaddr vals) "Hello" :TEST #'equal))
    (let ((vals (struct-values f2)))
      (is-type vals 'list)
      (is-type (car vals) 'symbol)
      (is (car vals) 'foo)
      (is (caadr vals) 'x)
      (is (cdadr vals) 0)
      (is (caaddr vals) 'y)
      (is (cdaddr vals) 8))))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
