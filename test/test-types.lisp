
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
  (ok (not (typep (cons "foo" 4) 'flag-pair))))


;; -------------------------------------------------------------------------- ;;

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
  (ok (not (typep 'foo 'flag))))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `list-of-flags' type."
  (ok (list-of-flags-p (list "foo" (cons "bar" "baz"))))
  (ok (list-of-flags-p (list)))
  (ok (not (list-of-flags-p (list 4))))
  (ok (not (list-of-flags-p 4)))
  (ok (not (list-of-flags-p (list "foo" 4))))
  (is-type (list "foo" (cons "bar" "baz") (cons "qux" #P"sal")) 'list-of-flags)
  (is-type (list) 'list-of-flags)
  (ok (not (typep (list 4) 'list-of-flags)))
  (ok (typep (list "foo" (cons "bar" "baz")) 'list-of-flags)))


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
