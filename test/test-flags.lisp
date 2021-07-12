
(in-package :cl-user)
(ql:quickload '(:prove :compdb/flags))
(defpackage :compdb/test/test-flags
  (:USE :common-lisp :prove :compdb/flags))
(in-package :compdb/test/test-flags)


;; ========================================================================== ;;

(subtest "Test `scoped-flag' struct."
  (let ((sf1 (make-scoped-flag))
        (sf2 (make-scoped-flag :FLAG "foo"))
        (sf3 (make-scoped-flag :FLAG "bar"))
        (sf4 (make-scoped-flag :FLAG "foo" :LOCAL T))
        (sf5 (make-scoped-flag :FLAG "bar" :LOCAL T))
        (sf6 (make-scoped-flag :FLAG "bar" :LOCAL T)))

    (ok (scoped-flag-p sf1))
    (ok (typep sf1 'scoped-flag))

    (is (scoped-flag-flag sf1) ""    :TEST #'equal)
    (is (scoped-flag-flag sf2) "foo" :TEST #'equal)
    (is (scoped-flag-flag sf3) "bar" :TEST #'equal)
    (is (scoped-flag-flag sf4) "foo" :TEST #'equal)
    (is (scoped-flag-flag sf5) "bar" :TEST #'equal)

    (is (scoped-flag-local sf1) NIL)
    (is (scoped-flag-local sf2) NIL)
    (is (scoped-flag-local sf3) NIL)
    (is (scoped-flag-local sf4) T)
    (is (scoped-flag-local sf5) T)

    (ok (not (equal sf5 sf6)))
    (ok (equalp sf5 sf6))))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
