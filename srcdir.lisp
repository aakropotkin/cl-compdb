
(in-package :cl-user)
(defpackage :compdb/srcdir
  (:USE
   :common-lisp
   :compdb/types
   :compdb/flag-collection)
  (:IMPORT-FROM :compdb/lang-tag        #:list-of-lang-tags)
  (:IMPORT-FROM :compdb/cunit           #:cunit
                                        #:list-of-cunits
                                        #:cunit-srcpath-equal-p)
  (:IMPORT-FROM :compdb/dirs            #:subpath-p)
  (:IMPORT-FROM :uiop                   #:pathname-equal)
  (:EXPORT
   #:srcdir
   #:srcdir-p
   #:make-srcdir
   #:srcdir-dirpath
   #:srcdir-parent-dir
   #:srcdir-sources
   #:srcdir-flags
   #:srcdir-local-flags
   #:srcdir-languages
   #:srcdir-compilers
   #:srcdir-child-dirs

   #:list-of-srcdirs-p
   #:list-of-srcdirs

   #:srcdir-lookup-srcdir
   #:srcdir-lookup-src))

(in-package :compdb/srcdir)


;; ========================================================================== ;;

;; Forward Declare
(declaim (type compdb))

;; -------------------------------------------------------------------------- ;;
(defstruct srcdir
  (dirpath     #P"/" :TYPE pathname)
  (parent-dir  NIL   :TYPE (or srcdir null))
  (sources     '()   :TYPE list-of-cunits)
  ;;(flags       NIL   :TYPE (or flag-collection null))
  (flags       NIL   :TYPE (or lang-flag-set null))
  (languages   '()   :TYPE list-of-lang-tags)
  (compilers   '()   :TYPE list-of-strings)
  (child-dirs  '()   :TYPE list-of-srcdirs))


;; -------------------------------------------------------------------------- ;;

(defun list-of-srcdirs-p (lst)
  (and (listp lst)
       (every (lambda (x) (typep x 'srcdir)) lst)))

(deftype list-of-srcdirs ()
  `(satisfies list-of-srcdirs-p))


;; -------------------------------------------------------------------------- ;;

(defun srcdir-lookup-srcdir (csd path)
  (declare (type srcdir csd))
  (declare (type pathname path))
  (cond
    ;; Exact match
    ((uiop:pathname-equal path (srcdir-dirpath csd)) csd)
    ;; Source in `csd'
    ((equal (pathname-directory path)
            (pathname-directory (srcdir-dirpath csd)))
     csd)
    ;; Check that filepaths match glob before recursively searching
    ((not (subpath-p (srcdir-dirpath csd) path)) NIL)
    ;; Recursively descend
    (T (let ((s (find path (srcdir-child-dirs csd)
                      :TEST (lambda (p d) (subpath-p d p)))))
         (if (null s) NIL (srcdir-lookup-srcdir s path))))))


(defun srcdir-lookup-src (csd path)
  (declare (type srcdir csd))
  (declare (type pathname path))
  (if (not (subpath-p (srcdir-dirpath csd) path)) NIL
      (let ((d (srcdir-lookup-srcdir csd path)))
        (find path (srcdir-sources d)
              :TEST (lambda (p s) (cunit-srcpath-equal-p s p))))))


;; -------------------------------------------------------------------------- ;;

;;(defun srcdir-set-cu-flags (csd cu)
;;  (declare (type srcdir csd))
;;  (declare (type cunit cu))
;;  (setf (slot-value cu 'local-flags)
;;        (flag-collection-difference (cunit-flags cu) (srcdir-local-flags csd))))


;; -------------------------------------------------------------------------- ;;
;;
;; When adding a single compilation unit we have a few cases:
;;
;; 1. The CU has the same flags as every other sibling. Do Nothing.
;; 2. The CU is a different language as other siblings. ???
;; 3. The CU is missing flags that were common among siblings.
;;    Remove flag from `srcdir' and add it back to all siblings.
;;    If the `srcdir' has child directories add the flag to child directories
;;    as well.
;; 4. The CU is missing flags that were common among CC or CXX siblings.
;;    Basically the same as (3), but don't worry about siblings for different
;;    languages.
;; 5. The CU has flags that other siblings lack, same as (1), but if you have a
;;    mechanism to explicitly track individual files with "unique" flags
;;    ( perhaps for `automake' builds using `libtool' ) be sure to "mark" it.
;;
;; -------------------------------------------------------------------------- ;;

(defun srcdir-update-dir-flags (csd cu)
  (declare (type srcdir csd))
  (declare (type cunit cu))
  ;;(let* ((sd-l-flags (srcdir-local-flags csd))
  ;;       (sd-flags   (srcdir-flags csd))
  ;;       (cu-flags   (cunit-flags cu))
  ;;       (l-isect    (flag-collection-intersect sd-l-flags cu-flags))
  ;;       (l-sc-diff  )
  ;;       ))
  NIL)


;; -------------------------------------------------------------------------- ;;

(defun srcdir-update-cus-flags (csd)
  (declare (type srcdir csd))
  (declare (type cunit cu))
  ;; FIXME
  NIL)


;; -------------------------------------------------------------------------- ;;

;;(defun srcdir-add-cunit
;;    (csd cu :&key (update-dir-flags NIL)
;;                  (update-cus-flags NIL)
;;                  (set-cu-flags     NIL))
;;  (declare (type srcdir csd))
;;  (declare (type cunit cu))
;;  (declare (type boolean update-dir-flags update-cus-flags set-cu-flags))
;;  ;; FIXME
;;  NIL)


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
