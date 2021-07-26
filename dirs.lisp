
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
   #:simplify-path

   #:join-pathnames
   #:parse-dir-namestring

   #:subpath-p
   #:any-subpath-p
   #:get-paths-roots
   ))

(in-package :compdb/dirs)


;; ========================================================================== ;;

(declaim
 (ftype (function (T) boolean)
        directory-component-head-p
        directory-compenent-keyword-p
        directory-component-elem-p
        directory-component-p
        directory-pathname-p
        dirpath-p
        path-p
        list-of-directory-components-p
        list-of-directory-pathnames-p
        list-of-dirpaths-p
        list-of-paths-p))


;; -------------------------------------------------------------------------- ;;

(defparameter *dir-sep-str*
  (string (uiop:directory-separator-for-host)))


;; -------------------------------------------------------------------------- ;;

(defun directory-component-head-p (x)
  (and (keywordp x)
       (member x '(:RELATIVE :ABSOLUTE))
       T))

(defun directory-compenent-keyword-p (x)
  (and (keywordp x)
       (or (directory-component-head-p x)
           (member x '(:UP :BACK :WILD :WILD-INFERIORS :UNSPECIFIC)))
       T))

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

(declaim
 (ftype (function (directory-component) directory-component))
 (ftype (function (path) directory-component) as-directory-component)
 (ftype (function (directory-component) directory-component)
        simplify-directory-component))


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

(declaim
 (ftype (function (path) pathname) as-pathname)
 (ftype (function (path) path) simplify-path)
 ;; FIXME: find out `ftype' for `&rest'
 ;(ftype (function (list-of-paths) pathname) join-pathnames)
 ; FIXME: parse-dir-namestring
 )


;; -------------------------------------------------------------------------- ;;

(defun as-pathname (x)
  (declare (type path x))
  (the pathname
       (etypecase x
         (pathname            x)
         (string              (parse-namestring x))
         (directory-component (make-pathname :DIRECTORY x)))))


;; -------------------------------------------------------------------------- ;;

(defun as-directory-component (x)
  (declare (type (path) x))
  (the directory-component
       (etypecase x
         (pathname            (pathname-directory x))
         (string              (pathname-directory (parse-namestring x)))
         (directory-component x))))


;; -------------------------------------------------------------------------- ;;

(defun simplify-directory-component (dc)
  (declare (type directory-component dc))
  (the directory-component
       (as-directory-component
        (as-pathname (directory-component-ups-as-backs dc)))))


;; -------------------------------------------------------------------------- ;;

(defun simplify-path (p)
  (declare (type path p))
  (the path
       (etypecase p
         (pathname (make-pathname
                    :DIRECTORY (directory-component-ups-as-backs
                                (pathname-directory p))
                    :DEFAULTS p))
         (directory-component (simplify-directory-component p))
         (string   (let* ((pname  (parse-namestring p))
                          (dc     (pathname-directory pname))
                          (sdc    (simplify-directory-component dc))
                          (spname (make-pathname
                                   :DIRECTORY sdc
                                   :DEFAULTS pname)))
                     (namestring spname))))))


;; -------------------------------------------------------------------------- ;;

(defun join-pathnames (&rest args)
  (declare (type list-of-paths args))
  ;;(assert (list-of-dirpaths-p (butlast args)))
  (let ((arg-pathnames (mapcar #'as-pathname args)))
    (the pathname
         (reduce (lambda (p1 p2) (merge-pathnames p2 p1)) arg-pathnames))))


;; -------------------------------------------------------------------------- ;;

(defun parse-dir-namestring (ns &key (simple T))
  (declare (type string ns))
  (declare (type boolean simple))
  (let* ((try      (parse-namestring ns))
         (try-name (pathname-name try)))
    (if (null try-name) try
        (let ((dc (append (or (pathname-directory try) (list :RELATIVE))
                          (list try-name))))
         (make-pathname
          :NAME      NIL
          ;:TYPE      NIL
          :DIRECTORY (if simple
                         (simplify-directory-component dc)
                         dc)
          :DEFAULTS  try)))))


;; -------------------------------------------------------------------------- ;;

(defun subpath-p (dir sub &key (direct NIL))
  "Is `sub' a subdirectory of `dir'?
When `direct' is `T', detect direct children."
  (declare (type dirpath  dir))
  (declare (type path sub))
  (declare (type boolean  direct))
  (let ((pdir (simplify-path (as-pathname dir)))
        (psub (simplify-path (as-pathname sub))))
    (if direct
        (if (null (pathname-name psub))
            (pathname-match-p psub (merge-pathnames (parse-dir-namestring "*/")
                                                    pdir))
            (equal (pathname-directory pdir) (pathname-directory psub)))
        (pathname-match-p psub (uiop:wilden pdir)))))


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
