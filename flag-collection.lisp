
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
   ))

(in-package :compdb/flag-collection)


;; ========================================================================== ;;

;; -------------------------------------------------------------------------- ;;

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



;; ========================================================================== ;;
