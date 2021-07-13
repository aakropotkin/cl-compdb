
(in-package :cl-user)
(defpackage :compdb/test/test-types
  (:USE :common-lisp :prove :compdb/types))
(in-package :compdb/test/test-types)


;; ========================================================================== ;;

(subtest "Test `list-of-strings' type."
  (ok (list-of-strings-p (list "foo" "bar")))
  (ok (list-of-strings-p (list)))
  (ok (not (list-of-strings-p (list "foo" 4))))
  (ok (not (list-of-strings-p (list 'foo 'bar))))
  (is-type (list "foo" "bar") 'list-of-strings)
  (is-type (list) 'list-of-strings)
  (ok (typep (list "foo" "bar") 'list-of-strings))
  (ok (not (typep (list "foo" 4) 'list-of-strings)))
  (ok (not (typep (list 'foo 'bar) 'list-of-strings))))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `list-of-pathnames' type."
  (ok (list-of-pathnames-p (list #P"foo")))
  (ok (list-of-pathnames-p (list #P"foo" #P"bar")))
  (ok (not (list-of-pathnames-p (list "foo" "bar"))))
  (ok (not (list-of-pathnames-p (list #P"foo" "bar"))))
  (is-type (list #P"foo" #P"bar") 'list-of-pathnames)
  (ok (typep (list #P"foo" #P"bar") 'list-of-pathnames))
  (ok (not (typep (list "foo" "bar") 'list-of-pathnames)))
  (ok (not (typep (list #P"foo" "bar") 'list-of-pathnames))))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `string-pair' type."
  (ok (string-pair-p (cons "foo" "bar")))
  (ok (not (string-pair-p (cons "foo" 4))))
  (is-type (cons "foo" "bar") 'string-pair)
  (ok (typep (cons "foo" "bar") 'string-pair))
  (ok (not (typep (cons "foo" 4) 'string-pair))))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `list-of-symbols' type."
  (ok (list-of-symbols-p (list 'a 'b 'c)))
  (ok (list-of-symbols-p (list 'a 'b :C)))
  (ok (list-of-symbols-p (list)))
  (ok (not (list-of-symbols-p 4)))
  (ok (not (list-of-symbols-p (list 'a 4))))
  (is-type (list 'a 'b) 'list-of-symbols)
  (is-type (list 'a :B) 'list-of-symbols)
  (ok (typep (list 'a 'b) 'list-of-symbols))
  (ok (not (typep (list 'a 'b 4) 'list-of-symbols))))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
