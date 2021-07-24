
(in-package :cl-user)
(ql:quickload '(:prove :compdb/dirs))
(defpackage :compdb/test/test-dirs
  (:USE :common-lisp :prove :compdb/dirs)
  (:EXPORT
   #:test-directory-component-t
   #:test-list-of-directory-components-t
   #:test-directory-pathname-t
   #:test-list-of-directory-pathnames-t
   #:run-test-dirs))
(in-package :compdb/test/test-dirs)


;; ========================================================================== ;;

(defun test-directory-component-t ()
  (subtest "Test `directory-component' type."
    (is-type (list :RELATIVE "foo") 'directory-component)
    (directory-component-p (list :RELATIVE "foo"))))


;; -------------------------------------------------------------------------- ;;

(defun test-list-of-directory-components-t ()
  (subtest "Test `list-of-directory-component' type."
    (is-type (list (list :RELATIVE "foo")) 'list-of-directory-components)
    (list-of-directory-components-p (list (list :RELATIVE "foo")))))


;; -------------------------------------------------------------------------- ;;

(defun test-directory-pathname-t ()
  (subtest "Test `directory-pathname' type."
    (is-type (parse-namestring "foo/") 'directory-pathname))
    (directory-pathname-p (parse-namestring "foo/")))


;; -------------------------------------------------------------------------- ;;

(defun test-list-of-directory-pathnames-t ()
  (subtest "Test `list-of-directory-pathnames' type."
    (is-type (list (parse-namestring "foo/")) 'list-of-directory-pathnames))
    (list-of-directory-pathnames-p (list (parse-namestring "foo/"))))


;; -------------------------------------------------------------------------- ;;

(defun run-test-dirs ()
  (test-directory-component-t)
  (test-list-of-directory-components-t)
  (test-directory-pathname-t)
  (test-list-of-directory-pathnames-t))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
