
(in-package :cl-user)
(ql:quickload '(:prove :compdb/flags))
(defpackage :compdb/test/test-flags
  (:USE :common-lisp :prove :compdb/flags)
  (:EXPORT
   #:test-raw-flag-pair-t
   #:test-flag-pair-t
   #:test-raw-flaggable-t
   #:test-raw-list-of-flaggables
   #:run-test-flags))
(in-package :compdb/test/test-flags)


;; ========================================================================== ;;

(defun test-raw-flag-pair-t ()
  (subtest "Test `flag-pair' type on actual pairs."
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

(defparameter *raw-flag-pairs*
  (list (cons "foo" "bar")
        (cons "foo" #P"bar")))

(defparameter *not-raw-flag-pairs*
  (list (cons #P"bar" "foo")
        (cons "foo" 4)))


;; -------------------------------------------------------------------------- ;;

(defun test-flag-pair-t ()
  (subtest "Test `flag-pair' type."
    (ok (every #'flaggable-p *raw-flag-pairs*))
    (ok (notany #'flaggable-p *not-raw-flag-pairs*))
    (ok (flag-pair-p (make-flag :OPT "foo" :ARG "bar")))
    (ok (flag-pair-p (make-flag :OPT "foo" :ARG #P"bar")))
    (ok (not (flag-pair-p (make-flag :OPT "foo"))))
    (ok (not (flag-pair-p (make-flag :OPT "foo" :SCOPE :COMMON))))
    (is-type (make-flag :OPT "foo" :ARG "bar") 'flag-pair)
    (is-type (make-flag :OPT "foo" :ARG #P"bar") 'flag-pair)))


;; -------------------------------------------------------------------------- ;;

(defparameter *flag-pairs*
  (append *raw-flag-pairs*
          (list (make-flag :OPT "foo" :ARG "bar")
                (make-flag :OPT "foo" :ARG #P"bar"))))

(defparameter *not-flag-pairs*
  (append *not-raw-flag-pairs*
          (list (make-flag :OPT "foo")
                (make-flag :OPT "foo" :SCOPE :COMMON))))


;; -------------------------------------------------------------------------- ;;

(defun test-raw-flaggable-t ()
  (subtest "Test `flaggable' type on `string's and `raw-flar-pair's."
    (ok (flaggable-p "foo"))
    (is-type "foo" 'flaggable)
    (ok (typep "foo" 'flaggable))
    (ok (every #'flaggable-p *raw-flag-pairs*))
    (ok (notany #'flaggable-p *not-raw-flag-pairs*))
    (ok (not (flaggable-p 4)))
    (ok (not (typep 4 'flaggable)))
    (ok (not (flaggable-p 'foo)))
    (ok (not (typep 'foo 'flaggable)))
    (is-type (cons "foo" "bar") 'flaggable)
    (ok (typep (cons "foo" "bar") 'flaggable))
    (is-type (cons "foo" #P"bar") 'flaggable)
    (ok (typep (cons "foo" #P"bar") 'flaggable))))


;; -------------------------------------------------------------------------- ;;

(defun test-raw-list-of-flaggables ()
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

(defun run-test-flags ()
  (test-raw-flag-pair-t)
  (test-flag-pair-t)
  (test-raw-flaggable-t)
  (test-raw-list-of-flaggables))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
