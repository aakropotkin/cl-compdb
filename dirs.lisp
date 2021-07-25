
(in-package :cl-user)
(ql:quickload '(:compdb/types :uiop))
(defpackage :compdb/dirs
  (:USE :common-lisp :compdb/types)
  (:IMPORT-FROM :uiop #:directory-separator-for-host)
  (:EXPORT
   #:directory-component-p
   #:directory-component
   #:list-of-directory-components-p
   #:list-of-directory-components

   #:directory-pathname-p
   #:directory-pathname
   #:list-of-directory-pathnames-p
   #:list-of-directory-pathnames

   #:dirpath-p
   #:dirpath
   #:list-of-dirpaths-p
   #:list-of-dirpaths

   #:path-p
   #:path
   #:list-of-paths-p
   #:list-of-paths

   #:as-pathname
   #:as-directory-component

   #:simplify-directory-component

   #:join-pathnames
   #:parse-dir-namestring

   #:subpath-p
   #:any-subpath-p
   #:get-paths-roots
   ))

(in-package :compdb/dirs)


;; ========================================================================== ;;

(defparameter *dir-sep-str*
  (string (uiop:directory-separator-for-host)))


;; -------------------------------------------------------------------------- ;;

(defun directory-component-head-p (x)
  (and (keywordp x)
       (member x '(:RELATIVE :ABSOLUTE))))

(defun directory-compenent-keyword-p (x)
  (and (keywordp x)
       (or (directory-component-head-p x)
           (member x '(:UP :BACK :WILD :WILD-INFERIORS :UNSPECIFIC)))))

(defun directory-component-elem-p (x)
  (or (directory-compenent-keyword-p x)
      (stringp x)))

(defun directory-component-p (x)
  (and (listp x)
       (member (car x) '(:RELATIVE :ABSOLUTE))
       (every #'directory-component-elem-p (cdr x))))

(deftype directory-component ()
  `(satisfies directory-component-p))


;; -------------------------------------------------------------------------- ;;

(defun directory-component-ups-as-backs (dc)
  "Replace all `:UP' elements with `:BACK' so they can be simplified."
  (declare (type directory-component dc))
  (subst :BACK :UP dc))


;; -------------------------------------------------------------------------- ;;

(defun directory-pathname-p (x)
  (and (pathnamep x)
       (null (pathname-name x))
       (null (pathname-type x))))

(deftype directory-pathname ()
  `(satisfies directory-pathname-p))


;; -------------------------------------------------------------------------- ;;

(defun dirpath-p (x)
  (or (directory-component-p x)
      (directory-pathname-p x)
      (and (stringp x)
           (let ((p (parse-namestring x)))
             (and (not (null p))
                  (directory-pathname-p p))))))

(deftype dirpath ()
  `(satisfies dirpath-p))


;; -------------------------------------------------------------------------- ;;

(defun path-p (x)
  "A `pathname', `dirpath', or `string' which can be parsed as a `pathname'."
  (or (pathnamep x)
      (dirpath-p x)
      (and (stringp x)
           ;; This is a bit goofy honestly because it's incredibly hard to
           ;; make an invalid pathname.
           (not (null (parse-namestring x))))))

(deftype path ()
  `(satisfies path-p))


;; -------------------------------------------------------------------------- ;;

(def-list-type directory-components directory-component)
(def-list-type directory-pathnames  directory-pathname)
(def-list-type dirpaths             dirpath)
(def-list-type paths                path)


;; -------------------------------------------------------------------------- ;;

(defun as-pathname (x)
  (declare (type (or pathname string directory-component) x))
  (typecase x
    (pathname            x)
    (string              (parse-namestring x))
    (directory-component (make-pathname :DIRECTORY x))))


;; -------------------------------------------------------------------------- ;;

(defun as-directory-component (x)
  (declare (type (or pathname string dirpath) x))
  (typecase x
    (pathname            (pathname-directory x))
    (string              (pathname-directory (parse-namestring x)))
    (directory-component x)))


;; -------------------------------------------------------------------------- ;;

(defun simplify-directory-component (dc)
  (declare (type directory-component dc))
  (the directory-component
       (as-directory-component
        (as-pathname (directory-component-ups-as-backs dc)))))


;; -------------------------------------------------------------------------- ;;

(defun join-pathnames (&rest args)
  (declare (type list-of-paths args))
  ;;(assert (list-of-dirpaths-p (butlast args)))
  (let ((arg-pathnames (mapcar #'as-pathname args)))
    (reduce (lambda (p1 p2) (merge-pathnames p2 p1)) arg-pathnames)))


;; -------------------------------------------------------------------------- ;;

(defun parse-dir-namestring (ns)
  (declare (type string ns))
  (let* ((try      (parse-namestring ns))
         (try-name (pathname-name try)))
    (if (null try-name) try
         (make-pathname
          :HOST      (pathname-host try)
          :NAME      NIL
          :TYPE      NIL
          :DIRECTORY (append (or (pathname-directory try) (list :RELATIVE))
                             (list try-name))
          :DEVICE    (pathname-device try)
          :VERSION   (pathname-version try)))))


;; -------------------------------------------------------------------------- ;;

(defun subpath-p (dir sub &key (direct NIL))
  "Is `sub' a subdirectory of `dir'?
When `direct' is `T', detect direct children."
  (declare (type dirpath  dir))
  (declare (type path sub))
  (declare (type boolean  direct))
  (cond
    ((and direct (null (pathname-name sub)))
     (pathname-match-p sub (merge-pathnames (parse-dir-namestring "*/") dir)))
    ;; Is `sub' a file in `dir'?
    (direct (equal (as-directory-component dir) (as-directory-component sub)))
    ;; Is `sub' a file or directory in the subtree of `dir'?
    (T (pathname-match-p sub (uiop:wilden (as-pathname dir))))))


;; -------------------------------------------------------------------------- ;;

(defun any-subpath-p (dirs sub)
  (declare (type list-of-dirpaths dirs))
  (declare (type path sub))
  (some (lambda (d) (subpath-p d sub)) dirs))


;; -------------------------------------------------------------------------- ;;

(defun get-paths-roots (paths)
  (declare (type list-of-paths paths))
  (let* ((dir-comps  (remove-duplicates (mapcar #'as-directory-component
                                                paths)))
         (root-comps (remove-if (lambda (d) (any-subpath-p dir-comps d))
                                dir-comps)))
    (mapcar #'as-pathname root-comps)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
