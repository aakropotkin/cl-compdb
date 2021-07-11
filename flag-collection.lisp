
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

   #:flag-set-has-kind-p
   #:flag-set-inc-p
   #:flag-set-def-p
   #:flag-set-comp-p
   #:flag-set-other-p

   #:list-of-flag-sets-p
   #:list-of-flag-sets

    ;; ---------------------------------------------------------------------- ;;

   #:lang-flag-set
   #:lang-flag-set-p
   #:lang-flag-set-ltag
   #:lang-flag-set-flag-sets

   #:lfs-flag-sets

   #:lang-flag-set-has-lang-p
   #:lfs-has-lang-p
   #:lang-flag-set-cc-p
   #:lfs-ccp
   #:lang-flag-set-cxx-p
   #:lfs-cxxp

   #:lang-flag-set-get-all
   #:lfs-get-all
   #:lang-flag-set-has-kind-p
   #:lfs-has-kind-p
   #:lang-flag-set-kinds
   #:lfs-kinds

   #:lang-flag-set-get-kind
   #:lfs-get-kind
   #:lang-flag-set-get-inc
   #:lfs-get-inc
   #:lang-flag-set-get-def
   #:lfs-get-def
   #:lang-flag-set-get-comp
   #:lfs-get-comp
   #:lang-flag-set-get-other
   #:lfs-get-other

   #:list-of-lang-flag-sets-p
   #:list-of-lang-flag-sets

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


;; -------------------------------------------------------------------------- ;;

(defstruct lang-flag-set
  (ltag      NIL :TYPE (or lang-tag null))
  (flag-sets NIL :TYPE (or list-of-flag-sets null)))

(defmacro lfs-flag-sets (lfs)
  (list 'lang-flag-set-flag-sets lfs))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-has-lang-p (lfs ltag)
  (declare (type lang-flag-set lfs))
  (declare (type lang-tag ltag))
  (equal (lang-flag-set-ltag lfs) ltag))

(defmacro lfs-has-lang-p (lfs ltag)
  (list 'lang-flag-set-has-lang-p lfs ltag))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-cc-p (lfs)
  (lang-flag-set-has-lang-p lfs :CC))

(defmacro lfs-cc-p (lfs)
  (list 'lang-flag-set-cc-p lfs))


(defun lang-flag-set-cxx-p (lfs)
  (lang-flag-set-has-lang-p lfs :CXX))

(defmacro lfs-cxx-p (lfs)
  (list 'lang-flag-set-cxx-p lfs))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-get-all (lfs)
  (mapcan #'lang-flag-set-flag-sets lfs))

(defmacro lfs-get-all (lfs)
  (list 'lang-flag-set-get-all lfs))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-get-kind (lfs kind)
  (declare (type lang-flag-set lfs))
  (declare (type flag-set-kind kind))
  (find-if (lambda (fs) (flag-set-has-kind-p fs kind))
           (lang-flag-set-flag-sets lfs)))

(defmacro lfs-get-kind (lfs kind)
  (list 'lang-flag-set-get-kind lfs kind))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-has-kind-p (lfs kind)
  (declare (type lang-flag-set lfs))
  (declare (type flag-set-kind kind))
  (any (lambda (fs) (flag-set-has-kind-p fs kind))
       (lang-flag-set-flag-sets lfs)))

(defmacro lfs-has-kind-p (lfs kind)
  (list 'lang-flag-set-has-kind lfs kind))


(defun lang-flag-set-kinds (lfs)
  (mapcar #'flag-set-kind (lang-flag-set-flag-sets lfs)))

(defmacro lfs-kinds (lfs)
  (list 'lang-flag-set-kinds lfs))


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-get-inc (lfs)
  (lang-flag-set-get-kind lfs :INC))

(setf (symbol-function 'lfs-get-inc) #'lang-flag-set-get-inc)


;; -------------------------------------------------------------------------- ;;

(defun lang-flag-set-get-def (lfs)
  (lang-flag-set-get-kind lfs :DEF))

(defmacro lfs-get-def (lfs)
  (list 'lang-flag-set-get-def lfs))


(defun lang-flag-set-get-comp (lfs)
  (lang-flag-set-get-kind lfs :COMP))

(defmacro lfs-get-comp (lfs)
  (list 'lang-flag-set-get-comp lfs))


(defun lang-flag-set-get-other (lfs)
  (lang-flag-set-get-kind lfs :OTHER))

(defmacro lfs-get-other (lfs)
  (list 'lang-flag-set-get-other lfs))


;; -------------------------------------------------------------------------- ;;

(defun list-of-lang-flag-sets-p (x)
  (and (listp x)
       (every #'lang-flag-set-p x)))

(deftype list-of-lang-flag-sets ()
  `(satisfies list-of-lang-flag-sets-p))


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

(defun lang-flag-set-union
    (a b &key (ignore-scope T))
  (declare (type lang-flag-set a b))
  (declare (type boolean ignore-scope))
  (assert (equal (lang-flag-set-ltag a) (lang-flag-set-ltag b)))
  (let* ((fss-a    (flag-sets (lang-flag-set-flag-sets a)))
         (fss-b    (flag-sets (lang-flag-set-flag-sets b)))
         (fs-kinds (remove-duplicates
                    (append (lfs-kinds fss-a) (lfs-kinds fss-b))))
         )
    ))



;; -------------------------------------------------------------------------- ;;
;;(defun flag-collection-union (a b)
;;  (declare (type flag-collection a b))
;;  (make-flag-collection
;;   :ALL-FLAGS (union a b :TEST #'equal)
;;   :CC-FLAGS  (union a b :TEST #'equal)
;;   :CXX-FLAGS (union a b :TEST #'equal)
;;   :DEF-FLAGS (union a b :TEST #'equal)
;;   :INC-FLAGS (union a b :TEST #'equal)))


;; -------------------------------------------------------------------------- ;;

;;(defun flag-collection-intersect (a b &key (ltag NIL))
;;  "Remove members of `b' from `a'.
;;when `ltag' is given and is `:CC' or `:CXX', only remove flags for the
;;relevant language."
;;  (declare (type flag-collection a b))
;;  (declare (type (or lang-tag null) ltag))
;;  (let* ((a-cc-flags (flag-collection-cc-flags a))
;;         (b-cc-flags (if (or (null ltag) (equal ltag :CC))
;;                         (flag-collection-cc-flags b)
;;                         NIL))
;;         (cc-flags   (if (or (null ltag) (equal ltag :CC))
;;                         (intersection a-cc-flags b-cc-flags :TEST #'equal)
;;                         a-cc-flags))
;;
;;         (a-cxx-flags (flag-collection-cxx-flags a))
;;         (b-cxx-flags (if (or (null ltag) (equal ltag :CXX))
;;                          (flag-collection-cxx-flags b)
;;                          NIL))
;;         (cxx-flags   (if (or (null ltag) (equal ltag :CXX))
;;                         (intersection a-cxx-flags b-cxx-flags :TEST #'equal)
;;                         a-cxx-flags))
;;
;;         (a-def-flags (flag-collection-def-flags a))
;;         (b-def-flags (flag-collection-def-flags b))
;;         (def-flags   (intersection a-cdef-flags b-def-flags) :TEST #'equal)
;;
;;         (a-inc-flags (flag-collection-inc-flags a))
;;         (b-inc-flags (flag-collection-inc-flags b))
;;         (inc-flags   (intersection a-cinc-flags b-inc-flags) :TEST #'equal)
;;
;;         (a-misc-flags (flag-collection-uncategorized-flags a))
;;         (b-misc-flags (flag-collection-uncategorized-flags b))
;;         (misc-flags   (intersection a-cmisc-flags b-misc-flags) :TEST #'equal)
;;
;;         (all-flags    (remove-duplicates (append cc-flags
;;                                                  cxx-flags
;;                                                  def-flags
;;                                                  inc-flags
;;                                                  misc-flags)
;;                                          :TEST #'equal)))
;;    (make-flag-collection :ALL-FLAGS all-flags
;;                          :CC-FLAGS  cc-flags
;;                          :CXX-FLAGS cxx-flags
;;                          :DEF-FLAGS def-flags
;;                          :INC-FLAGS inc-flags)))


;; -------------------------------------------------------------------------- ;;

;;(defun flag-collection-difference (a b &key (ltag NIL))
;;  "Remove members of `b' from `a'.
;;when `ltag' is given and is `:CC' or `:CXX', only remove flags for the
;;relevant language."
;;  (declare (type flag-collection a b))
;;  (declare (type (or lang-tag null) ltag))
;;  (let* ((a-cc-flags (flag-collection-cc-flags a))
;;         (b-cc-flags (if (or (null ltag) (equal ltag :CC))
;;                         (flag-collection-cc-flags b)
;;                         NIL))
;;         (cc-flags   (if (or (null ltag) (equal ltag :CC))
;;                         (set-difference a-cc-flags b-cc-flags :TEST #'equal)
;;                         a-cc-flags))
;;
;;         (a-cxx-flags (flag-collection-cxx-flags a))
;;         (b-cxx-flags (if (or (null ltag) (equal ltag :CXX))
;;                          (flag-collection-cxx-flags b)
;;                          NIL))
;;         (cxx-flags   (if (or (null ltag) (equal ltag :CXX))
;;                         (set-difference a-cxx-flags b-cxx-flags :TEST #'equal)
;;                         a-cxx-flags))
;;
;;         (a-def-flags (flag-collection-def-flags a))
;;         (b-def-flags (flag-collection-def-flags b))
;;         (def-flags   (set-difference a-cdef-flags b-def-flags) :TEST #'equal)
;;
;;         (a-inc-flags (flag-collection-inc-flags a))
;;         (b-inc-flags (flag-collection-inc-flags b))
;;         (inc-flags   (set-difference a-cinc-flags b-inc-flags) :TEST #'equal)
;;
;;         (all-flags    (set-difference (flag-collection-all-flags a)
;;                                       (flag-collection-all-flags b)
;;                                       :TEST #'equal)))
;;    (make-flag-collection :ALL-FLAGS all-flags
;;                          :CC-FLAGS  cc-flags
;;                          :CXX-FLAGS cxx-flags
;;                          :DEF-FLAGS def-flags
;;                          :INC-FLAGS inc-flags)))


;; -------------------------------------------------------------------------- ;;

;;(defun flag-collection-subset (a b &key (ltag NIL))
;;  "All members in `b' are in `a'.
;;When `ltag' is given and is `:CC' or `:CXX', only the relevant language will
;;be checked."
;;  (declare (type flag-collection a b))
;;  (declare (type (or lang-tag null) ltag))
;;  (and
;;   (subsetp (flag-collection-inc-flags a)
;;            (flag-collection-inc-flags b)
;;            :TEST #'equal)
;;   (subsetp (flag-collection-def-flags a)
;;            (flag-collection-def-flags b)
;;            :TEST #'equal)
;;   ;; FIXME
;;   )
;;  (if (or (null ltag) (not (c-lang-tag-p ltag)))
;;      ;;
;;      )
;;  )


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
