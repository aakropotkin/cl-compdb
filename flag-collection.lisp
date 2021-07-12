
(in-package :cl-user)
(ql:quickload
 '(:compdb/types :compdb/flags :compdb/alias :compdb/json-cdb :compdb/lang-tag))
(defpackage :compdb/flag-collection
  (:USE
   :common-lisp
   :compdb/types
   :compdb/flags
   :compdb/alias)
  (:IMPORT-FROM :compdb/json-cdb #:get-jcu-args
                                 #:get-jcu-compiler
                                 #:get-jcu-lang-tag)
  (:IMPORT-FROM :compdb/lang-tag #:lang-tag
                                 #:c-lang-tag
                                 #:c-lang-tag-p)
  (:EXPORT
   #:flag-set-kind-p
   #:flag-set-kind
   #:flag-set-p
   #:flag-set
   #:flag-set-kind
   #:flag-set-flags

   #:list-of-flag-sets-p
   #:list-of-flag-sets

   #:flag-set-has-kind-p
   #:flag-set-inc-p
   #:flag-set-def-p
   #:flag-set-comp-p
   #:flag-set-other-p
   #:flag-set-kinds-eq-p

   #:flag-set-mark-scopes
   #:sibling-flag-sets-mark-scopes

   ;; ----------------------------------------------------------------------- ;;

   #:lang-flag-set
   #:lang-flag-set-p
   #:lang-flag-set-ltag
   #:lang-flag-set-flag-sets

   #:lang-flag-set-has-lang-p
   #:lang-flag-set-cc-p
   #:lang-flag-set-cxx-p

   #:lang-flag-set-get-all
   #:lang-flag-set-has-kind-p
   #:lang-flag-set-kinds

   #:lang-flag-set-get-kind
   #:lang-flag-set-get-inc
   #:lang-flag-set-get-def
   #:lang-flag-set-get-comp
   #:lang-flag-set-get-other

   #:lang-flag-set-mark-scopes

   #:lfs-ltag
   #:lfs-lang
   #:lfs-flag-sets
   #:lfs-flags
   #:lfs-has-lang-p
   #:lfs-ccp
   #:lfs-cxxp
   #:lfs-get-all
   #:lfs-has-kind-p
   #:lfs-kinds
   #:lfs-get-kind
   #:lfs-get-inc
   #:lfs-get-def
   #:lfs-get-comp
   #:lfs-get-other
   #:lfs-mark-scopes

   #:list-of-lang-flag-sets-p
   #:list-of-lang-flag-sets
   #:list-of-lfs-p
   #:list-of-lfs

   #:mk-lang-flag-set
   ))

(in-package :compdb/flag-collection)


;; ========================================================================== ;;

(defun flag-set-kind-p (x)
  (member x (list :INC :DEF :COMP :OTHER)))

(deftype flag-set-kind ()
  `(satisfies flag-set-kind-p))


;; -------------------------------------------------------------------------- ;;

(defstruct flag-set
  (kind  :OTHER :TYPE flag-set-kind)
  (flags '()    :TYPE list-of-scoped-flags))


;; -------------------------------------------------------------------------- ;;

(defun list-of-flag-sets-p (x)
  (and (listp x)
       (every #'flag-set-p x)))

(deftype list-of-flag-sets ()
  `(satisfies list-of-flag-sets-p))


;; -------------------------------------------------------------------------- ;;

(defun flag-set-has-kind-p (fs kind)
  (declare (type flag-set fs))
  (declare (type flag-set-kind kind))
  (equal (flag-set-kind fs) kind))

(defun flag-set-inc-p (fs)
  (flag-set-has-kind-p fs :INC))

(defun flag-set-def-p (fs)
  (flag-set-has-kind-p fs :DEF))

(defun flag-set-comp-p (fs)
  (flag-set-has-kind-p fs :COMP))

(defun flag-set-other-p (fs)
  (flag-set-has-kind-p fs :OTHER))


(defun flag-set-kinds-eq-p (a b)
  (declare (type flag-set a b))
  (equal (flag-set-kind a) (flag-set-kind b)))


;; -------------------------------------------------------------------------- ;;

(defun flag-set-mark-scopes (common-flags fs)
  (declare (type (or list-of-scoped-flags flag-set list-of-flag-sets)
                 common-flags))
  (declare (type flag-set fs))
  (typecase common-flags
    ;; Filter matching `kind' and recursively map.
    ;; There should almost never be multiple matches, but I won't assume.
    (list-of-flag-sets
     (mapc (lambda (c-fs) (flag-set-mark-scopes c-fs fs))
           (remove (flag-set-kind fs) common-flags :TEST-NOT #'equal
                                                   :KEY      #'flag-set-kind)))
    ;; When types match between `flag-sets'.
    (flag-set (when (flag-set-kinds-eq-p common-flags fs)
                (scoped-flags-mark-scopes (flag-set-flags common-flags)
                                          (slot-value fs 'flags))))
    ;; No need to unpack the scoped flags.
    (list-of-scoped-flags (scoped-flags-mark-scopes common-flags
                                                    (slot-value fs 'flags)))))


;; -------------------------------------------------------------------------- ;;

(defun sibling-flag-sets-mark-scopes (fsets)
  (declare (type list-of-flag-sets fsets))
  ;; Collect by `kind' and pipe into `lolo-scoped-flags-mark-scopes'.
  (flet
      ((fskind-mark-scopes (k)
         (cons k
               (lolo-scoped-flags-mark-scopes
                (mapcar
                 (lambda (fs) (slot-value fs 'flags))
                 (remove k fsets :TEST-NOT #'equal :KEY #'flag-set-kind))))))
    (mapcar #'fskind-mark-scopes
            (remove-duplicates (mapcar #'flag-set-kind
                                       fsets)))))


;; -------------------------------------------------------------------------- ;;

(defstruct lang-flag-set
  (ltag      NIL :TYPE (or lang-tag null))
  (flag-sets NIL :TYPE (or list-of-flag-sets null)))

(defalias 'lang-flag-set-ltag      'lfs-lang)
(defalias 'lang-flag-set-flag-sets 'lfs-flags)


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-has-lang-p (lfs ltag)
  (declare (type lang-flag-set lfs))
  (declare (type lang-tag ltag))
  (equal (lang-flag-set-ltag lfs) ltag))

(defun lang-flag-set-langs-eq-p (a b)
  (declare (type lang-flag-set a b))
  (equal (lang-flag-set-ltag a) (lang-flag-set-ltag b)))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-cc-p (lfs)
  (lang-flag-set-has-lang-p lfs :CC))

(defun lang-flag-set-cxx-p (lfs)
  (lang-flag-set-has-lang-p lfs :CXX))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-get-all (lfs)
  (mapcan #'lang-flag-set-flag-sets lfs))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-get-kind (lfs kind)
  (declare (type lang-flag-set lfs))
  (declare (type flag-set-kind kind))
  (find-if (lambda (fs) (flag-set-has-kind-p fs kind))
           (lang-flag-set-flag-sets lfs)))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-has-kind-p (lfs kind)
  (declare (type lang-flag-set lfs))
  (declare (type flag-set-kind kind))
  (any (lambda (fs) (flag-set-has-kind-p fs kind))
       (lang-flag-set-flag-sets lfs)))

(defun lang-flag-set-kinds (lfs)
  (mapcar #'flag-set-kind (lang-flag-set-flag-sets lfs)))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-get-inc (lfs)
  (lang-flag-set-get-kind lfs :INC))

(defun lang-flag-set-get-def (lfs)
  (lang-flag-set-get-kind lfs :DEF))

(defun lang-flag-set-get-comp (lfs)
  (lang-flag-set-get-kind lfs :COMP))

(defun lang-flag-set-get-other (lfs)
  (lang-flag-set-get-kind lfs :OTHER))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-mark-scopes (common-flags lfs)
  (declare (type (or list-of-scoped-flags
                     flag-set
                     list-of-flag-sets
                     lang-flag-set)
                 common-flags))
  (declare (type lang-flag-set lfs))
  (if (not (lang-flag-set-p common-flags))
      (flag-set-mark-scopes common-flags (slot-value lfs 'flag-sets))
      (when (lang-flag-set-langs-eq-p common-flags lfs)
        (let ((c-fss (lang-flag-set-flag-sets common-flags)))
          (mapc (lambda (fs) (flag-set-mark-scopes c-fss fs))
                (slot-value lfs 'flag-sets))))))


;; -------------------------------------------------------------------------- ;;

(defaliases-replace-first
    "lang-flag-set" "lfs"
  '(lang-flag-set-ltag
    lang-flag-set-flag-sets
    lang-flag-set-has-lang-p
    lang-flag-set-langs-eq-p
    lang-flag-set-cxx-p
    lang-flag-set-cc-p
    lang-flag-set-get-all
    lang-flag-set-get-kind
    lang-flag-set-has-kind-p
    lang-flag-set-kinds
    lang-flag-set-get-inc
    lang-flag-set-get-def
    lang-flag-set-get-comp
    lang-flag-set-get-other
    lang-flag-set-mark-scopes))


;; -------------------------------------------------------------------------- ;;

(defun list-of-lang-flag-sets-p (x)
  (and (listp x)
       (every #'lang-flag-set-p x)))

(deftype list-of-lang-flag-sets ()
  `(satisfies list-of-lang-flag-sets-p))


(defalias 'list-of-lang-flag-sets-p 'list-of-lfs-p)

(deftype list-of-lfs ()
  `(satisfies list-of-lfs-p))


;; -------------------------------------------------------------------------- ;;

(defun mk-lang-flag-set
    (jcu &key (builddir NIL fixup-includes-p)
              (lang-tag NIL))

  (declare (type list jcu))
  (declare (type (or dirpath null) builddir))
  (declare (type (or lang-tag null) ltag))

  (let* ((flags     (cds (get-jcu-args jcu)))
         (compiler  (get-jcu-compiler jcu))
         (ltag      (or lang-tag (get-jcu-lang-tag jcu :COMPILER compiler)))
         (inc-flags (remove-if-not #'inc-flag-p flags))
         (defs      (remove-if-not #'def-flag-p flags))
         (defs-sf   (mapcar (lambda (f) (make-scoped-flag :FLAG f)) defs))
         (defs-set  (make-flag-set :KIND :DEF :FLAGS defs-sf))
         (oflags    (remove-if (lambda (f) (or (inc-flag-p f)
                                               (def-flag-p f)))
                               flags))
         (oflags-sf (mapcar (lambda (f) (make-scoped-flag :FLAG f)) oflags))
         (comp-set  (if ltag (make-flag-set :KIND :COMP :FLAGS oflags-sf) NIL))
         (other-set (if (null ltag)
                        (make-flag-set :KIND :OTHER :FLAGS oflags-sf)
                        NIL))
         (fixincs   (and fixup-includes-p
                         (mapcar (lambda (c) (fixup-inc-flag-pair builddir c))
                                 inc-flags)))
         (incs-sf   (mapcar (lambda (f) (make-scoped-flag :FLAG f))
                            (or fixincs inc-flags)))
         (incs-set  (make-flag-set :KIND :INC :FLAGS incs-sf))
         (flag-sets (remove-if #'null (list (or comp-set other-set)
                                            defs-set
                                            incs-set))))
    (make-lang-flag-sets :LTAG ltag :FLAG-SETS flag-sets)))


;; -------------------------------------------------------------------------- ;;




;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
