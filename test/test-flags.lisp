
(in-package :cl-user)
(ql:quickload '(:prove :compdb/flags))
(defpackage :compdb/test/test-flags
  (:USE :common-lisp :prove :compdb/flags)
  (:EXPORT
   #:test-flag-pair-t
   #:test-flaggable-t
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

(defun test-flaggable-t ()
    (subtest "Test `flaggable' type."
      (ok (flaggable-p "foo"))
      (ok (flaggable-p (cons "foo" "bar")))
      (ok (flaggable-p (cons "foo" #P"bar")))
      (ok (not (flaggable-p (cons "foo" 4))))
      (ok (not (flaggable-p (cons #P"foo" 4))))
      (ok (not (flaggable-p 4)))
      (ok (not (flaggable-p 'foo)))
      (is-type "foo" 'flaggable)
      (is-type (cons "foo" "bar") 'flaggable)
      (is-type (cons "foo" #P"bar") 'flaggable)
      (ok (typep "foo" 'flaggable))
      (ok (typep (cons "foo" "bar") 'flaggable))
      (ok (typep (cons "foo" #P"bar") 'flaggable))
      (ok (not (typep (cons "foo" 4) 'flaggable)))
      (ok (not (typep (cons #P"foo" 4) 'flaggable)))
      (ok (not (typep 4 'flaggable)))
      (ok (not (typep 'foo 'flaggable)))))


;; -------------------------------------------------------------------------- ;;

(defun test-list-of-flaggables ()
    (subtest "Test `list-of-flaggables' type."
      (ok (list-of-flaggables-p (list "foo" (cons "bar" "baz"))))
      (ok (list-of-flaggables-p (list)))
      (ok (not (list-of-flaggables-p (list 4))))
      (ok (not (list-of-flaggables-p 4)))
      (ok (not (list-of-flaggables-p (list "foo" 4))))
      (is-type (list "foo" (cons "bar" "baz") (cons "qux" #P"sal"))
               'list-of-flaggables)
      (is-type (list) 'list-of-flaggables)
      (ok (not (typep (list 4) 'list-of-flaggables)))
      (ok (typep (list "foo" (cons "bar" "baz")) 'list-of-flaggables))))


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
  (test-flaggable-t)
  (test-list-of-flaggables)
  ;(test-scoped-flag)
  )


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
