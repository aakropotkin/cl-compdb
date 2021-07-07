
(defpackage :compdb/flag-collection
  (:USE
   :common-lisp
   :compdb/types)
  (:IMPORT-FROM :compdb/json-cdb #:get-jcu-args
                                 #:get-jcu-compiler
                                 #:get-jcu-lang-tag)
  (:IMPORT-FROM :compdb/lang-tag #:lang-tag)
  (:IMPORT-FROM :compdb/flags    #:inc-flag-p
                                 #:def-flag-p
                                 #:fixup-inc-flag-pair)
  (:EXPORT
   #:flag-collection
   #:flag-collection-p
   #:make-flag-collection
   #:flag-collection-all-flags
   #:flag-collection-cc-flags
   #:flag-collection-cxx-flags
   #:flag-collection-def-flags
   #:flag-collection-inc-flags
   #:mk-flag-collection-json

   #:flag-collection-uncategorized-flags
   #:flag-collection-intersect
   #:flag-collection-difference
   #:flag-collection-union
   ))

(in-package :compdb/flag-collection)


;; ========================================================================== ;;

(defstruct flag-collection
  (all-flags NIL :TYPE (or list-of-flags null))
  (cc-flags  NIL :TYPE (or list-of-flags null))
  (cxx-flags NIL :TYPE (or list-of-flags null))
  (def-flags NIL :TYPE (or list-of-flags null))
  (inc-flags NIL :TYPE (or list-of-flags null)))


;; -------------------------------------------------------------------------- ;;

(defun mk-flag-collection-json
    (jcu &key (builddir NIL fixup-includes-p)
              (lang-tag NIL))
  (let* ((flags    (cdr (get-jcu-args jcu)))  ;; Drop compiler
         (compiler (get-jcu-compiler jcu))
         (ltag     (or lang-tag (get-jcu-lang-tag jcu :COMPILER compiler)))
         (incs     (remove-if-not #'inc-flag-p flags))
         (defs     (remove-if-not #'def-flag-p flags))
         (lflags   (remove-if (lambda (f) (or (inc-flag-p f)
                                              (def-flag-p f)))
                              flags))
         (fixincs  (and fixup-includes-p
                        (mapcar (lambda (c) (fixup-inc-flag-pair builddir c))
                         incs))))
    (make-flag-collection
     :ALL-FLAGS flags
     :CC-FLAGS  (and (equal :CC  ltag) lflags)
     :CXX-FLAGS (and (equal :CXX ltag) lflags)
     :DEF-FLAGS defs
     :INC-FLAGS (or fixincs incs)
     )))


;; -------------------------------------------------------------------------- ;;

(defun flag-collection-uncategorized-flags (fc)
  (declare (type flag-collection fc))
  (reduce (lambda (a b) (set-difference a b :TEST #'equal))
          (list (flag-collection-all-flags fc)
                (flag-collection-cc-flags  fc)
                (flag-collection-cxx-flags fc)
                (flag-collection-def-flags fc)
                (flag-collection-inc-flags fc))))


;; -------------------------------------------------------------------------- ;;

(defun flag-collection-union (a b)
  (declare (type flag-collection a b))
  (make-flag-collection
   :ALL-FLAGS (union a b :TEST #'equal)
   :CC-FLAGS  (union a b :TEST #'equal)
   :CXX-FLAGS (union a b :TEST #'equal)
   :DEF-FLAGS (union a b :TEST #'equal)
   :INC-FLAGS (union a b :TEST #'equal)))


;; -------------------------------------------------------------------------- ;;

(defun flag-collection-intersect (a b)
  (declare (type flag-collection a b))
  (make-flag-collection
   :ALL-FLAGS (intersection a b :TEST #'equal)
   :CC-FLAGS  (intersection a b :TEST #'equal)
   :CXX-FLAGS (intersection a b :TEST #'equal)
   :DEF-FLAGS (intersection a b :TEST #'equal)
   :INC-FLAGS (intersection a b :TEST #'equal)))


;; -------------------------------------------------------------------------- ;;

(defun flag-collection-difference (a b)
  (declare (type flag-collection a b))
  (make-flag-collection
   :ALL-FLAGS (set-difference a b :TEST #'equal)
   :CC-FLAGS  (set-difference a b :TEST #'equal)
   :CXX-FLAGS (set-difference a b :TEST #'equal)
   :DEF-FLAGS (set-difference a b :TEST #'equal)
   :INC-FLAGS (set-difference a b :TEST #'equal)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
