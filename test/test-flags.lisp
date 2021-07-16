
(in-package :cl-user)
(ql:quickload '(:prove :compdb/flags))
(defpackage :compdb/test/test-flags
  (:USE :common-lisp :prove :compdb/flags)
  (:EXPORT
   #:test-flag-pair-t
   #:test-flag-t
   #:test-list-of-flags
   #:run-test-flags))
(in-package :compdb/test/test-flags)


;; ========================================================================== ;;

(defun test-flag-pair-t ()
    (subtest "Test `flag-pair' type."
      (ok (flag-pair-p (cons "foo" "bar")))
      (ok (flag-pair-p (cons "foo" #P"bar")))
      (ok (not (flag-pair-p (cons #P"bar" "foo"))))
      (ok (not (flag-pair-p (cons "foo" 4))))
      (is-type (cons "foo" "bar") 'flag-pair)
      (is-type (cons "foo" #P"bar") 'flag-pair)
      (ok (typep (cons "foo" "bar") 'flag-pair))
      (ok (typep (cons "foo" #P"bar") 'flag-pair))
      (ok (not (typep (cons #P"bar" "foo") 'flag-pair)))
      (ok (not (typep (cons "foo" 4) 'flag-pair)))))


;; -------------------------------------------------------------------------- ;;

(defun test-flag-t ()
    (subtest "Test `flag' type."
      (ok (flag-p "foo"))
      (ok (flag-p (cons "foo" "bar")))
      (ok (flag-p (cons "foo" #P"bar")))
      (ok (not (flag-p (cons "foo" 4))))
      (ok (not (flag-p (cons #P"foo" 4))))
      (ok (not (flag-p 4)))
      (ok (not (flag-p 'foo)))
      (is-type "foo" 'flag)
      (is-type (cons "foo" "bar") 'flag)
      (is-type (cons "foo" #P"bar") 'flag)
      (ok (typep "foo" 'flag))
      (ok (typep (cons "foo" "bar") 'flag))
      (ok (typep (cons "foo" #P"bar") 'flag))
      (ok (not (typep (cons "foo" 4) 'flag)))
      (ok (not (typep (cons #P"foo" 4) 'flag)))
      (ok (not (typep 4 'flag)))
      (ok (not (typep 'foo 'flag)))))


;; -------------------------------------------------------------------------- ;;

(defun test-list-of-flags ()
    (subtest "Test `list-of-flags' type."
      (ok (list-of-flags-p (list "foo" (cons "bar" "baz"))))
      (ok (list-of-flags-p (list)))
      (ok (not (list-of-flags-p (list 4))))
      (ok (not (list-of-flags-p 4)))
      (ok (not (list-of-flags-p (list "foo" 4))))
      (is-type (list "foo" (cons "bar" "baz") (cons "qux" #P"sal"))
               'list-of-flags)
      (is-type (list) 'list-of-flags)
      (ok (not (typep (list 4) 'list-of-flags)))
      (ok (typep (list "foo" (cons "bar" "baz")) 'list-of-flags))))


;; -------------------------------------------------------------------------- ;;

;(defun test-scoped-flag ()
;    (subtest "Test `scoped-flag' struct."
;      (let ((sf1 (make-scoped-flag))
;            (sf2 (make-scoped-flag :FLAG "foo"))
;            (sf3 (make-scoped-flag :FLAG "bar"))
;            (sf4 (make-scoped-flag :FLAG "foo" :LOCAL T))
;            (sf5 (make-scoped-flag :FLAG "bar" :LOCAL T))
;            (sf6 (make-scoped-flag :FLAG "bar" :LOCAL T)))
;
;        (ok (scoped-flag-p sf1))
;        (ok (typep sf1 'scoped-flag))
;
;        (is (scoped-flag-flag sf1) ""    :TEST #'equal)
;        (is (scoped-flag-flag sf2) "foo" :TEST #'equal)
;        (is (scoped-flag-flag sf3) "bar" :TEST #'equal)
;        (is (scoped-flag-flag sf4) "foo" :TEST #'equal)
;        (is (scoped-flag-flag sf5) "bar" :TEST #'equal)
;
;        (is (scoped-flag-local sf1) NIL)
;        (is (scoped-flag-local sf2) NIL)
;        (is (scoped-flag-local sf3) NIL)
;        (is (scoped-flag-local sf4) T)
;        (is (scoped-flag-local sf5) T)
;
;        (ok (not (equal sf5 sf6)))
;        (ok (equalp sf5 sf6)))))


;; -------------------------------------------------------------------------- ;;

(defun run-test-flags ()
  (test-flag-pair-t)
  (test-flag-t)
  (test-list-of-flags)
  ;(test-scoped-flag)
  )


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
