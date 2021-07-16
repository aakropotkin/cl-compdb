
(in-package :cl-user)
(ql:quickload '(:prove :compdb/struct-utils))
(defpackage :compdb/test/test-struct-utils
  (:USE :common-lisp :prove :compdb/struct-utils)
  (:EXPORT
   #:run-test-stuct-utils
   #:test-update-struct
   #:test-struct-slots
   #:test-struct-values
   #:test-struct-to-list
   #:test-struct-p
   #:run-test-struct-utils))
(in-package :compdb/test/test-struct-utils)


;; ========================================================================== ;;

(defstruct foo
  (x 0 :TYPE fixnum)
  y)

(defun test-update-struct ()
    (subtest "Test `update-struct' function."
      (let* ((f1 (make-foo :X 5 :Y "Hello"))
             (f2 (update-struct f1 'y "World")))
        (is (foo-x f1) (foo-x f2))
        (isnt (foo-y f1) (foo-y f2) :TEST #'equal))))


;; -------------------------------------------------------------------------- ;;

(defun test-struct-slots ()
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
          (is-type (cadadr slots) 'compiled-function)))))


;; -------------------------------------------------------------------------- ;;

(defun test-struct-values ()
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
          (is (cdaddr vals) 8)))))


;; -------------------------------------------------------------------------- ;;

(defun test-struct-p ()
    (subtest "Test `struct-p' function."
      (let ((f1 (make-foo :X 5 :Y 8)))
        (ok (struct-p f1))
        (ok (not (struct-p 5))))))


;; -------------------------------------------------------------------------- ;;

(defun test-struct-to-list ()
    (subtest "Test `struct-to-list' function."
      (let ((f1 (make-foo :X 5 :Y 8))
            (f2 (make-foo :X 0 :Y NIL))
            (f3 (make-foo :X 8 :Y "Hello")))
        (ok (equalp (struct-to-list f1) (struct-values f1)))
        (ok (equalp (struct-to-list f2) (struct-values f2)))
        (ok (equalp (struct-to-list f3) (struct-values f3)))
        (let* ((f4   (make-foo :X 1 :Y f1))
               (f1-l (struct-to-list f1))
               (f4-l (struct-to-list f4))
               (f4-y (cdaddr f4-l)))
          (ok (not (equalp (struct-values f4) f4-l)))))))


;; -------------------------------------------------------------------------- ;;

(defun run-test-stuct-utils ()
  (test-update-struct)
  (test-struct-slots)
  (test-struct-values)
  (test-struct-to-list)
  (test-struct-p))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
