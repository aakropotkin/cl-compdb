
(defpackage :compdb/cunit
  (:USE
   :common-lisp
   :compdb/types)
  (:IMPORT-FROM :compdb/flags           #:list-of-flags)
  (:IMPORT-FROM :compdb/flag-collection #:mk-flag-collection-json)
  (:IMPORT-FROM :compdb/lang-tag        #:lang-tag
                                        #:generated-lang-p)
  (:IMPORT-FROM :uiop                   #:pathname-equal)
  (:IMPORT-FROM :compdb/json-cdb        #:get-jcu-compiler
                                        #:get-jcu-lang-tag
                                        #:get-jcu-src
                                        #:get-jcu-output)
  (:EXPORT
   #:cunit
   #:cunit-p
   #:make-cunit
   ;;#:cunit-cdb
   #:cunit-json-obj
   ;;#:cunit-srcdir
   #:cunit-srcpath
   #:cunit-output
   #:cunit-language
   #:cunit-compiler
   #:cunit-is-generated
   #:cunit-flags
   ;;#:cunit-local-flags

   #:mk-cunit-json
   ;;#:update-cunit-fields

   #:list-of-cunits-p
   #:list-of-cunits

   #:cunit-srcpath-equal-p
   ))

(in-package :compdb/cunit)


;; ========================================================================== ;;

(declaim (type compdb))
(declaim (type srcdir))


;; -------------------------------------------------------------------------- ;;

(defstruct cunit
  ;;(cdb          NIL   :TYPE (or compdb null))
  (json-obj     '()   :TYPE list)
  ;;(srcdir       NIL   :TYPE (or srcdir null))
  (srcpath      NIL   :TYPE (or pathname string null))
  (output       NIL   :TYPE (or pathname string null))
  (language     :CC   :TYPE lang-tag)
  (compiler     "cc"  :TYPE string)
  (is-generated NIL   :TYPE boolean)
  (flags        NIL   :TYPE (or flag-collection null))
  ;;(local-flags  NIL   :TYPE (or flag-collection null))
  )


;; -------------------------------------------------------------------------- ;;

(defun list-of-cunits-p (lst)
  (and (listp lst)
       (every (lambda (x) (typep x 'cunit)) lst)))

(deftype list-of-cunits ()
  `(satisfies list-of-cunits-p))


;; -------------------------------------------------------------------------- ;;

(defun cunit-srcpath-equal-p (cu path)
  (declare (type cunit cu))
  (declare (type pathname path))
  (uiop:pathname-equal (cunit-srcpath cu) path))


;; -------------------------------------------------------------------------- ;;

(defun mk-cunit-json (j)
    ;;(j &key (cdb       NIL)
    ;;        (src       NIL)
    ;;        (src-flags NIL))

  (declare (type cons j))
  ;;(declare (type (or compdb null) cdb))
  ;;(declare (type (or srcdir null) src))
  ;;(declare (type (or list-of-flags null) src-flags))

  (let* ((compiler     (get-jcu-compiler j))
         (ltag         (get-jcu-lang-tag j))
         (all-flags    (mk-flag-collection-json j :LANG-TAG ltag))
         ;;(local-flags  (if (null src-flags) NIL
         ;;                  (mk-flag-collection-json
         ;;                   (set-difference j src-flags :TEST #'equal)
         ;;                   :LANG-TAG ltag)))
         )
    (make-cunit
     ;;:CDB          cdb
     :JSON-OBJ     j
     ;;:SRCDIR       src
     :SRCPATH      (get-jcu-src j)
     :OUTPUT       (get-jcu-output j)
     :LANGUAGE     ltag
     :COMPILER     compiler
     :IS-GENERATED (generated-lang-p ltag)
     :FLAGS        all-flags
     ;;:LOCAL-FLAGS  local-flags
     )))


;; -------------------------------------------------------------------------- ;;

;;(defun update-cunit-fields
;;    (cu &key (cdb NIL)
;;             (src NIL)
;;             (src-flags NIL))
;;
;;  (declare (type cunit cu))
;;  (declare (type (or compdb null) cdb))
;;  (declare (type (or srcdir null) src))
;;  (declare (type (or list-of-flags null) src-flags))
;;
;;  (let ((new-cdb (if (null cdb) (cunit-cdb cu) cdb))
;;        (new-src (if (null src) (cunit-srcdir cu) src))
;;        (new-local-flags (if (null src-flags) (cunit-local-flags cu)
;;                             (mk-flag-collection-json
;;                              (set-difference (cunit-flags cu) src-flags
;;                                              :TEST #'equal)
;;                              :LANG_TAG (cunit-language cu)))))
;;    (update-struct cu
;;                   'cdb         new-cdb
;;                   'srcdir      new-src
;;                   'local-flags new-local-flags)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
