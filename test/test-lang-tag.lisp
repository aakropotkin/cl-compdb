
(in-package :cl-user)
(ql:quickload '(:prove :compdb/lang-tag))
(defpackage :compdb/test/test-lang-tag
  (:USE :common-lisp :prove :compdb/lang-tag)
  (:EXPORT
   #:test-lang-tag-t
   #:test-c-lang-tag-t
   #:test-gen-lang-tag-t
   #:test-list-of-lang-tags-t
   #:test-lang-tag-from-flags
   #:test-lang-tag-from-compiler
   #:run-test-lang-tag))

(in-package :compdb/test/test-lang-tag)


;; ========================================================================== ;;

(defun test-lang-tag-t ()
  (subtest "Test `lang-tag' type."
    (ok (lang-tag-p :CC))
    (ok (lang-tag-p :CXX))
    (ok (lang-tag-p :YACC))
    (ok (lang-tag-p :LEX))
    (ok (lang-tag-p :OTHER))
    (ok (not (lang-tag-p :HOWDY)))
    (is-type :CC 'lang-tag)
    (is-type :CXX 'lang-tag)
    (is-type :YACC 'lang-tag)
    (is-type :LEX 'lang-tag)
    (is-type :OTHER 'lang-tag)
    (ok (not (typep :HOWDY 'lang-tag)))))


;; -------------------------------------------------------------------------- ;;

(defun test-c-lang-tag-t ()
  (subtest "Test `c-lang-tag' type."
    (ok (c-lang-tag-p :CC))
    (ok (c-lang-tag-p :CXX))
    (is-type :CC 'c-lang-tag)
    (is-type :CXX 'c-lang-tag)
    (ok (not (c-lang-tag-p :YACC)))
    (ok (not (c-lang-tag-p :LEX)))
    (ok (not (c-lang-tag-p :OTHER)))
    (ok (not (c-lang-tag-p :HOWDY)))))


;; -------------------------------------------------------------------------- ;;

(defun test-gen-lang-tag-t ()
  (subtest "Test `gen-lang-tag' type."
    (ok (gen-lang-tag-p :YACC))
    (ok (gen-lang-tag-p :LEX))
    (ok (not (gen-lang-tag-p :OTHER)))
    (ok (not (gen-lang-tag-p :HOWDY)))
    (is-type :YACC 'gen-lang-tag)
    (is-type :LEX 'gen-lang-tag)))


;; -------------------------------------------------------------------------- ;;

(defun test-list-of-lang-tags-t ()
  (subtest "Test `list-of-lang-tag' type."
    T))


;; -------------------------------------------------------------------------- ;;

(defun test-lang-tag-from-flags ()
  (subtest "Test `lang-tag-from-flags' function."
    T))


;; -------------------------------------------------------------------------- ;;

(defun test-lang-tag-from-compiler ()
  (subtest "Test `lang-tag-from-compiler' function."
    T))


;; -------------------------------------------------------------------------- ;;

(defun run-test-lang-tag ()
  (test-lang-tag-t)
  (test-c-lang-tag-t)
  (test-gen-lang-tag-t)
  (test-list-of-lang-tags-t)
  (test-lang-tag-from-flags)
  (test-lang-tag-from-compiler))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
