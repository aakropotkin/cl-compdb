
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
   #:list-of-directory-pathname-p
   #:list-of-directory-pathname

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

(defun directory-compenent-head-p (x)
  (and (keywordp x)
       (member x '(:RELATIVE :ABSOLUTE))))

(defun directory-compenent-keyword-p (x)
  (and (keywordp x)
       (or (directory-component-head-p x)
           (member x '(:UP :BACK :WILD :WILD-INFERIORS :UNSPECIFIC)))))

(defun directory-compenent-elem-p (x)
  (or (directory-compenent-keyword-p x)
      (stringp x)))

(defun directory-component-p (x)
  (and (listp x)
       (member (car x) '(:RELATIVE :ABSOLUTE))
       (every #'directory-component-elem-p (cdr x))))

(deftype directory-component ()
  `(satisfies directory-component-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-directory-components-p (x)
  (and (listp x)
       (every #'directory-component-p x)))

(deftype list-of-directory-components ()
  `(satisfies list-of-directory-components-p))


;; -------------------------------------------------------------------------- ;;

(defun directory-pathname-p (x)
  (and (pathnamep x)
       (null (pathname-name x))
       (null (pathname-type x))))

(deftype directory-pathname ()
  `(satisfies directory-pathname-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-directory-pathnames-p (x)
  (and (listp x)
       (every #'directory-pathname-p x)))

(deftype list-of-directory-pathnames ()
  `(satisfies list-of-directory-pathnames-p))


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

(defun list-of-dirpaths-p (x)
  (and (listp x)
       (every #'dirpath-p x)))

(deftype list-of-dirpaths ()
  `(satisfies list-of-dirpaths-p))


;; -------------------------------------------------------------------------- ;;

(defun path-p (x)
  (or (pathnamep x)
      (dirpath-p x)
      (and (stringp x)
           ;; This is a bit goofy honestly because it's incredibly hard to
           ;; make an invalid pathname.
           (not (null (parse-namestring x))))))

(deftype path ()
  `(satisfies path-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-paths-p (x)
  (and (listp x)
       (every #'path-p x)))

(deftype list-of-paths ()
  `(satisfies list-of-paths-p))


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
          :DIRECTORY (append (pathname-directory try) (list try-name))
          :DEVICE    (pathname-device try)
          :VERSION   (pathname-version try)))))


;; -------------------------------------------------------------------------- ;;

(defun subpath-p (dir sub &key (direct NIL))
  (declare (type dirpath  dir))
  (declare (type path sub))
  (declare (type boolean  direct))
  (cond
    ;; Is `sub' a subdirectory of `dir'?
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


(defun get-paths-roots (paths)
  (declare (type list-of-paths paths))
  (let* ((dir-comps  (remove-duplicates (mapcar #'as-directory-component
                                                paths)))
         (root-comps (remove-if (lambda (d) (any-subpath-p dir-comps d))
                                dir-comps)))
    (mapcar #'as-pathname root-comps)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
