
(defpackage :compdb/cunit
  (:USE
   :common-lisp
   :compdb/types)
  (:IMPORT-FROM :compdb/flags           #:list-of-flags)
  ;;(:IMPORT-FROM :compdb/flag-collection #:mk-flag-collection-json)
  (:IMPORT-FROM :compdb/flag-collection #:mk-lang-flag-set
                                        #:lang-flag-set)
  (:IMPORT-FROM :compdb/lang-tag        #:lang-tag
                                        #:gen-lang-tag-p)
  (:IMPORT-FROM :uiop                   #:pathname-equal)
  (:IMPORT-FROM :compdb/json-cdb        #:get-jcu-compiler
                                        #:get-jcu-lang-tag
                                        #:get-jcu-src
                                        #:get-jcu-output)
  (:EXPORT
   #:cunit
   #:cunit-p
   #:make-cunit
   #:cunit-json-obj
   #:cunit-srcpath
   #:cunit-output
   #:cunit-language
   #:cunit-compiler
   #:cunit-is-generated
   #:cunit-flags
   #:cunit-local-flags

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
  (json-obj     '()   :TYPE list)
  (srcpath      NIL   :TYPE (or path null))
  (output       NIL   :TYPE (or path null))
  (language     :CC   :TYPE lang-tag)
  (compiler     "cc"  :TYPE string)
  (is-generated NIL   :TYPE boolean)
  (flags        NIL   :TYPE (or lang-flag-set null))
  ;;(flags        NIL   :TYPE (or flag-collection null))
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
  (declare (type cons j))
  (let* ((compiler     (get-jcu-compiler j))
         (ltag         (get-jcu-lang-tag j))
         ;;(all-flags    (mk-flag-collection-json j :LANG-TAG ltag))
         (all-flags    (mk-lang-flag-set j :LANG-TAG ltag))
         )
    (make-cunit
     :JSON-OBJ     j
     :SRCPATH      (get-jcu-src j)
     :OUTPUT       (get-jcu-output j)
     :LANGUAGE     ltag
     :COMPILER     compiler
     :IS-GENERATED (gen-lang-tag-p ltag)
     :FLAGS        all-flags
     :LOCAL-FLAGS  NIL)))


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
