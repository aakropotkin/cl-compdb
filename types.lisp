
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
   #:string-pair-p
   #:string-pair
   #:list-of-symbols-p
   #:list-of-symbols))

(in-package :compdb/types)


;; ========================================================================== ;;

(declaim
  (ftype (function (T) boolean)
         list-of-strings-p
         list-of-pathnames-p
         string-pair-p
         list-of-symbols-p))


;; -------------------------------------------------------------------------- ;;

(defmacro def-pair-type (name t1 t2)
  (let ((name-t (intern
                 (concatenate 'string "PAIR-OF-" (symbol-name name) "-P")))
        (name-p (intern (concatenate 'string (symbol-name name) "-P"))))
    `(progn
       (defun ,name-p (x)
         (and (consp x)
              (typep (car x) (quote ,t1))
              (typep (cdr x) (quote ,t2))))
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
         (and (listp x)
              (every (lambda (e) (typep e (quote ,tp))) x)))
       (deftype ,name-t ()
         (quote (satisfies ,name-p))))))


;; -------------------------------------------------------------------------- ;;

(defun list-of-strings-p (lst)
  (the boolean (and (listp lst)
                    (every #'stringp lst)
                    T)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-pathnames-p (lst)
  (the boolean (and (listp lst)
                    (every #'pathnamep lst)
                    T)))

(deftype list-of-pathnames ()
  `(satisfies list-of-pathnames-p))


;; -------------------------------------------------------------------------- ;;

(defun string-pair-p (cell)
  (the boolean (and (consp cell)
                    (stringp (car cell))
                    (stringp (cdr cell))
                    T)))

(deftype string-pair ()
  `(satisfies string-pair-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-symbols-p (x)
  (the boolean (and (listp x)
                    (every #'symbolp x)
                    T)))

(deftype list-of-symbols ()
  `(satisfies list-of-symbols-p))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
