
(in-package :cl-user)
(ql:quickload '(:prove :compdb/dirs))
(defpackage :compdb/test/test-dirs
  (:USE :common-lisp :prove :compdb/dirs)
  (:EXPORT
   #:test-directory-component-t
   #:test-list-of-directory-components-t
   #:test-directory-pathname-t
   #:test-list-of-directory-pathnames-t #:test-dirpath-t
   #:test-list-of-dirpaths-t
   #:test-path-t
   #:test-list-of-paths-t
   #:test-as-pathname
   #:test-as-directory-component
   #:test-simplify-path
   #:test-simplify-directory-component
   #:test-join-pathnames
   #:test-parse-dir-namestring
   #:test-subpath-p
   #:test-any-subpath-p
   #:test-get-paths-roots
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

(defun test-as-pathname ()
  (subtest "Test `as-pathname' function."
    (is-type (as-pathname "foo/") 'pathname)
    (is-type (as-pathname (parse-namestring "foo/")) 'pathname)
    (is-type (as-pathname (list :RELATIVE "foo")) 'pathname)
    ;; Note that `:BACK' is "simplified" while `:UP' is not.
    (ok (equal (as-pathname (list :RELATIVE "foo" "bar" :BACK "baz"))
               (as-pathname (list :RELATIVE "foo" "baz"))))
    (ok (not (equal (as-pathname (list :RELATIVE "foo" "bar" :UP "baz"))
                    (as-pathname (list :RELATIVE "foo" "baz")))))))


;; -------------------------------------------------------------------------- ;;

(defun test-as-directory-component ()
  (subtest "Test `as-directory-component' function."
    (is-type (as-directory-component "foo/") 'directory-component)
    (is-type (as-directory-component (list :RELATIVE "foo"))
             'directory-component)
    (is-type (as-directory-component (parse-namestring "foo/"))
             'directory-component)))


;; -------------------------------------------------------------------------- ;;

(defun test-simplify-directory-component ()
  (subtest "Test `simplify-directory-component' function."
    (ok (equal (simplify-directory-component
                (list :RELATIVE "foo" "bar" :BACK "baz"))
               (list :RELATIVE "foo" "baz")))
    (ok (equal (simplify-directory-component
                (list :RELATIVE "foo" "bar" :UP "baz"))
               (list :RELATIVE "foo" "baz")))))


;; -------------------------------------------------------------------------- ;;

(defun test-simplify-path ()
  (subtest "Test `simplify-path' function."
    (ok (equal (simplify-path "foo/bar/../baz") "foo/baz"))
    (ok (equal (simplify-path #P"foo/bar/../baz") #P"foo/baz"))
    (ok (equal (simplify-path (list :RELATIVE "foo" "bar" :BACK "baz"))
               (list :RELATIVE "foo" "baz")))
    (ok (equal (simplify-path (list :RELATIVE "foo" "bar" :UP "baz"))
               (list :RELATIVE "foo" "baz")))))


;; -------------------------------------------------------------------------- ;;

(defun test-join-pathnames ()
  (subtest "Test `join-pathnames' function."
    (is-type (join-pathnames "foo/" "bar") 'pathname)
    (ok (equal (parse-namestring "foo/bar") (join-pathnames "foo/" "bar")))))


;; -------------------------------------------------------------------------- ;;

(defun test-parse-dir-namestring ()
  (subtest "Test `parse-dir-namestring' function"
    (is-type (parse-dir-namestring "foo/bar/") 'pathname)
    (is-type (parse-dir-namestring "foo/bar") 'pathname)
    (is-type (parse-dir-namestring "foo/bar/") 'directory-pathname)
    (is-type (parse-dir-namestring "foo/bar") 'directory-pathname)
    (is-type (parse-dir-namestring "foo bar") 'directory-pathname)))


;; -------------------------------------------------------------------------- ;;

(defun test-subpath-p ()
  (subtest "Test `subpath-p' function."
    (ok (subpath-p "foo/" "foo/bar/baz"))
    (ok (not (subpath-p "foo/" "quux/bar/baz")))
    (ok (not (subpath-p "foo/" "foo/bar/baz" :DIRECT T)))
    (ok (subpath-p "foo/" "foo/bar" :DIRECT T))
    (ok (subpath-p "foo/" "foo/bar/" :DIRECT T))
    (let ((dir1 (list :RELATIVE "foo"))
          (dir2 (list :RELATIVE "foo" "bar"))
          (dir3 (list :RELATIVE "baz"))
          (dir4 (list :ABSOLUTE "foo"))
          (dir5 (list :RELATIVE "baz" "bakk" :BACK))
          (dir6 (list :RELATIVE "baz" "bakk" :UP))
          (dir7 (list :RELATIVE "baz" "bakk" :BACK "billy"))
          (dir8 (list :RELATIVE "baz" "bakk" :UP "sally")))
      (ok (subpath-p dir1 (as-pathname dir2)))
      (ok (not (subpath-p dir1 (as-pathname dir3))))
      (ok (not (subpath-p dir1 (as-pathname dir4))))
      (ok (not (subpath-p dir3 (as-pathname dir5))))
      (ok (not (subpath-p dir3 (as-pathname dir6))))
      (ok (subpath-p dir3 (as-pathname dir7)))
      (ok (subpath-p dir3 (as-pathname dir8)))
      (ok (subpath-p dir5 (as-pathname dir7)))
      (ok (subpath-p dir5 (as-pathname dir8)))
      (ok (subpath-p dir6 (as-pathname dir8)))
      (ok (subpath-p dir6 (as-pathname dir7))))))


;; -------------------------------------------------------------------------- ;;

(defun test-any-subpath-p ()
  (subtest "Test `any-subpath-p' function."
    (let ((dir1 (list :RELATIVE "foo"))
          (dir2 (list :RELATIVE "foo" "bar"))
          (dir3 (list :RELATIVE "baz"))
          (dir4 (list :ABSOLUTE "foo"))
          (dir5 (list :RELATIVE "baz" "bakk" :BACK))
          (dir6 (list :RELATIVE "baz" "bakk" :UP))
          (dir7 (list :RELATIVE "baz" "bakk" :BACK "billy"))
          (dir8 (list :RELATIVE "baz" "bakk" :UP "sally")))
      (ok (any-subpath-p (list dir1) (as-pathname dir2)))
      (ok (not (any-subpath-p (list dir1) (as-pathname dir3))))
      (ok (not (any-subpath-p (list dir1) (as-pathname dir4))))
      (ok (not (any-subpath-p (list dir3) (as-pathname dir5))))
      (ok (not (any-subpath-p (list dir3) (as-pathname dir6))))
      (ok (any-subpath-p (list dir3) (as-pathname dir7)))
      (ok (any-subpath-p (list dir3) (as-pathname dir8)))
      (ok (any-subpath-p (list dir5) (as-pathname dir7)))
      (ok (any-subpath-p (list dir5) (as-pathname dir8)))
      (ok (any-subpath-p (list dir6) (as-pathname dir8)))
      (ok (any-subpath-p (list dir6) (as-pathname dir7))))))


;; -------------------------------------------------------------------------- ;;

(defun test-get-paths-roots ()
  (subtest "Test `get-paths-roots' function."
    (let ((p1 #P"foo/bar/baz")
          (p2 #P"foo/bar/quux")
          (p3 #P"a/b/c")
          (p4 #P"a/b/d"))
      (ok (equal (list #P"foo/bar/" #P"a/b/")
                 (get-paths-roots (list p1 p2 p3 p4)))))))


;; -------------------------------------------------------------------------- ;;

(defun run-test-dirs ()
  (test-directory-component-t)
  (test-list-of-directory-components-t)
  (test-directory-pathname-t)
  (test-list-of-directory-pathnames-t)
  (test-dirpath-t)
  (test-list-of-dirpaths-t)
  (test-path-t)
  (test-list-of-paths-t)
  (test-as-pathname)
  (test-as-directory-component)
  (test-simplify-directory-component)
  (test-simplify-path)
  (test-join-pathnames)
  (test-parse-dir-namestring)
  (test-subpath-p)
  (test-any-subpath-p)
  (test-get-paths-roots))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
