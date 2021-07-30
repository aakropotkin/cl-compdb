
(in-package :cl-user)
(ql:quickload '(:prove :compdb/alias))
(defpackage :compdb/test/test-alias
  (:USE :common-lisp :prove :compdb/alias)
  (:EXPORT
   #:test-defalias
   ;#:test-defalias-replace-first
   ;#:test-defaliases-replace-first
   #:run-test-alias))

(in-package :compdb/test/test-alias)


;; ========================================================================== ;;

(defparameter *x* 1)
(defalias '*x*  '*x1*)
(defparameter *y* 2)
(defalias '*y*  '*y1*)
(defun add1 (x) (+ 1 x))
(defalias 'add1 'adder)

(defun test-defalias ()
  (subtest "Test `test-defalias' function."
    (ok (equal *x* *x1*))
    (ok (equal *y* *y1*))
    (ok (equal (add1 1) (adder 1)))))


;; -------------------------------------------------------------------------- ;;

(defparameter var-x *x*)
(defalias-replace-first "var" "bar" 'var-x)
(defparameter var-y *y*)
(defalias-replace-first "var" "bar" 'var-y)
(defun foo-x () *x*)
(defalias-replace-first "foo" "baz" 'foo-x)
(defun foo-y () *y*)
(defalias-replace-first "foo" "baz" 'foo-y)

;(defun test-defalias-replace-first ()
;  (subtest "Test `test-defalias-replace-first' function."
;    ;(ok (equal var-x bar-x))
;    ;(ok (equal var-y bar-y))
;    (ok (equal (foo-x) (baz-x)))
;    (ok (equal (foo-y) (baz-y)))))


;; -------------------------------------------------------------------------- ;;

;(defun test-defaliases-replace-first ()
;  (subtest "Test `test-defaliases-replace-first' function."
;    (ok T)))


;; -------------------------------------------------------------------------- ;;

(defun run-test-alias ()
  (test-defalias)
  ;(test-defalias-replace-first)
  ;(test-defaliases-replace-first)
  )


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
