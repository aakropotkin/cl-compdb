
(in-package :cl-user)
(ql:quickload '(:prove :compdb/dirs))
(defpackage :compdb/test/test-dirs
  (:USE :common-lisp :prove :compdb/dirs)
  (:EXPORT
   #:test-directory-component-t
   #:test-list-of-directory-components-t
   #:test-directory-pathname-t
   #:test-list-of-directory-pathnames-t
   #:test-dirpath-t
   #:test-list-of-dirpaths-t
   #:test-path-t
   #:test-list-of-paths-t
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

(defun test-dirpath-t ()
  (subtest "Test `dirpath' type."
    (is-type (list :RELATIVE "foo") 'dirpath)
    (is-type (parse-namestring "foo/") 'dirpath)
    (is-type "foo/" 'dirpath)
    (dirpath-p (list :RELATIVE "foo"))
    (dirpath-p (parse-namestring "foo/"))
    (dirpath-p "foo/")))


;; -------------------------------------------------------------------------- ;;

(defun test-list-of-dirpaths-t ()
  (subtest "Test `list-of-dirpaths' type."
    (is-type (list (list :RELATIVE "foo")) 'list-of-dirpaths)
    (is-type (list (parse-namestring "foo/")) 'list-of-dirpaths)
    (is-type (list "foo/") 'list-of-dirpaths)
    (list-of-dirpaths-p (list (list :RELATIVE "foo")))
    (list-of-dirpaths-p (list (parse-namestring "foo/")))
    (list-of-dirpaths-p (list "foo/"))))


;; -------------------------------------------------------------------------- ;;

(defun test-path-t ()
  (subtest "Test `path' type."
     (is-type "foo/" 'path)
     (is-type (list :RELATIVE "foo") 'path)
     (is-type (parse-namestring "foo/") 'path)
     (is-type (parse-namestring "foo/bar") 'path)
     (path-p "foo/")
     (path-p (list :RELATIVE "foo"))
     (path-p (parse-namestring "foo/"))
     (path-p (parse-namestring "foo/bar"))))


;; -------------------------------------------------------------------------- ;;

(defun test-list-of-paths-t ()
  (subtest "Test `list-of-paths' type."
     (is-type (list "foo/") 'list-of-paths)
     (is-type (list (list :RELATIVE "foo")) 'list-of-paths)
     (is-type (list (parse-namestring "foo/")) 'list-of-paths)
     (is-type (list (parse-namestring "foo/bar")) 'list-of-paths)
     (list-of-paths-p (list "foo/"))
     (list-of-paths-p (list (list :RELATIVE "foo")))
     (list-of-paths-p (list (parse-namestring "foo/")))
     (list-of-paths-p (list (parse-namestring "foo/bar")))))


;; -------------------------------------------------------------------------- ;;

(defun run-test-dirs ()
  (test-directory-component-t)
  (test-list-of-directory-components-t)
  (test-directory-pathname-t)
  (test-list-of-directory-pathnames-t)
  (test-dirpath-t)
  (test-list-of-dirpaths-t)
  (test-path-t)
  (test-list-of-paths-t))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
