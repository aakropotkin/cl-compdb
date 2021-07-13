
(in-package :cl-user)
(defpackage :compdb/types
  (:USE :common-lisp)
  (:EXPORT
   #:def-pair-type
   #:def-list-type
   #:list-of-strings-p
   #:list-of-strings
   #:list-of-pathnames-p
   #:list-of-pathnames
   #:pair-of-strings-p
   #:pair-of-strings
   #:list-of-symbols-p
   #:list-of-symbols))

(in-package :compdb/types)


;; ========================================================================== ;;

(declaim
  (ftype (function (T) boolean)
         list-of-strings-p
         list-of-strings
         list-of-pathnames-p
         list-of-pathnames
         pair-of-strings-p
         pair-of-strings
         list-of-symbols-p
         list-of-symbols))


;; -------------------------------------------------------------------------- ;;

(defmacro def-pair-type (name t1 t2)
  (let ((name-t (intern
                 (concatenate 'string "PAIR-OF-" (symbol-name name) "-P")))
        (name-p (intern (concatenate 'string (symbol-name name) "-P"))))
    `(progn
       (defun ,name-p (x)
         (and (consp x)
              (typep (car x) (quote ,t1))
              (typep (cdr x) (quote ,t2))
              T))
       (deftype ,name-t ()
         (quote (satisfies ,name-p))))))


;; -------------------------------------------------------------------------- ;;

(defmacro def-list-type (name tp)
  (let ((name-t (intern (concatenate 'string "LIST-OF-" (symbol-name name))))
        (name-p (intern (concatenate 'string "LIST-OF-"
                                             (symbol-name name)
                                             "-P"))))
    `(progn
       (defun ,name-p (x)
         (the boolean (and (listp x)
                           (every (lambda (e) (typep e (quote ,tp))) x)
                           T)))
       (deftype ,name-t ()
         (quote (satisfies ,name-p))))))


;; -------------------------------------------------------------------------- ;;

(def-list-type strings   string)
(def-list-type pathnames pathname)
(def-pair-type strings   string)
(def-list-type symbols   symbol)


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
