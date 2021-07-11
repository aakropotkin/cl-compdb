
(in-package :cl-user)
(ql:quickload '(:prove :compdb/string-utils))
(defpackage :compdb/test/test-string-utils
  (:USE :common-lisp :prove :compdb/string-utils))
(in-package :compdb/test/test-string-utils)


;; ========================================================================== ;;

(subtest "Test `strs-equal' function."
  (let ((lst-a (list "foo" "bar" "baz"))
        (lst-b (list "foo" "bar" "baz"))
        (lst-c (list "foo" "bar" "quux"))
        (lst-d (list "foo" "bar"))
        (lst-e (list)))
    (ok (strs-equal lst-a lst-a))
    (ok (strs-equal lst-a lst-b))
    (ok (strs-equal lst-b lst-a))
    (ok (strs-equal lst-e lst-e))
    (ok (not (strs-equal lst-a lst-d)))
    (ok (not (strs-equal lst-a lst-e)))
    (ok (not (strs-equal lst-a lst-c)))))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `strs-intersect' function."
  (let ((lst-a (list "foo" "bar" "baz"))
        (lst-b (list "foo" "bar"))
        (lst-c (list "foo" "bar" "quux"))
        (lst-d (list)))
    (is (strs-intersect lst-a lst-b) (list "foo" "bar") :TEST #'strs-equal)
    (is (strs-intersect lst-b lst-c) (list "foo" "bar") :TEST #'strs-equal)
    (is (strs-intersect lst-a lst-d) (list))) :TEST #'strs-equal)


;; -------------------------------------------------------------------------- ;;

(subtest "Test `strs-difference' function."
  (let ((lst-a (list "foo" "bar" "baz"))
        (lst-b (list "foo" "bar"))
        (lst-c (list "foo" "bar" "quux"))
        (lst-d (list)))
    (is (strs-difference lst-a lst-b) (list "baz") :TEST #'strs-equal)
    (is (strs-difference lst-b lst-c) (list) :TEST #'strs-equal)
    (is (strs-difference lst-a lst-d) lst-a :TEST #'strs-equal)
    (isnt (strs-difference lst-b lst-a) (list "baz") :TEST #'strs-equal)))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `strs-union' function."
  (let ((lst-a (list "foo" "bar" "baz"))
        (lst-b (list "foo" "bar"))
        (lst-c (list "foo" "bar" "quux"))
        (lst-d (list)))
    (is (strs-union lst-a lst-b) lst-a :TEST #'strs-equal)
    (is (strs-union lst-b lst-c) lst-c :TEST #'strs-equal)
    (is (strs-union lst-a lst-c)
        (list "foo" "bar" "baz" "quux")
        :TEST #'strs-equal)
    (is (strs-union lst-a lst-d) lst-a :TEST #'strs-equal)))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `strs-uniq' function."
  (is (strs-uniq (list "foo" "bar" "baz"))
      (list "foo" "bar" "baz")
      :TEST #'strs-equal)
  (is (strs-uniq (list)) (list) :TEST #'strs-equal)
  (is (strs-uniq (list "foo" "bar" "foo"))
      (list "foo" "bar")
      :TEST #'strs-equal))


;; -------------------------------------------------------------------------- ;;

(subtest "Test `str-append-char' function."
  (is (str-append-char "foo" #\/) "foo/" :TEST #'equal))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
