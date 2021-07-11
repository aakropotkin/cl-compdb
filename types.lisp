
(in-package :cl-user)
(defpackage :compdb/types
  (:USE :common-lisp)
  (:EXPORT
   #:list-of-strings-p
   #:list-of-strings
   #:list-of-pathnames-p
   #:list-of-pathnames
   #:string-pair-p
   #:string-pair
   #:flag-pair-p
   #:flag-pair
   #:flag-p
   #:flag
   #:list-of-flags-p
   #:list-of-flags
   #:list-of-symbols-p
   #:list-of-symbols))

(in-package :compdb/types)


;; ========================================================================== ;;

(defun list-of-strings-p (lst)
  (and (listp lst)
       (every #'stringp lst)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-pathnames-p (lst)
  (and (listp lst)
       (every #'pathnamep lst)))

(deftype list-of-pathnames ()
  `(satisfies list-of-pathnames-p))


;; -------------------------------------------------------------------------- ;;

(defun string-pair-p (cell)
  (and (consp cell)
       (stringp (car cell))
       (stringp (cdr cell))))

(deftype string-pair ()
  `(satisfies string-pair-p))


;; -------------------------------------------------------------------------- ;;

(defun flag-pair-p (x)
  (and (consp x)
       (stringp (car x))
       (let ((d (cdr x)))
         (or (stringp d)
             (pathnamep d)))))

(deftype flag-pair ()
  `(satisfies flag-pair-p))


;; -------------------------------------------------------------------------- ;;

(defun flag-p (x)
  (or (stringp x)
      (flag-pair-p x)))

(deftype flag ()
  `(satisfies flag-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-flags-p (lst)
  (and (listp lst)
       (every #'flag-p lst)))

(deftype list-of-flags ()
  `(satisfies list-of-flags-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-symbols-p (x)
  (and (listp x)
       (every #'symbolp x)))

(deftype list-of-symbols ()
  `(satisfies list-of-symbols-p))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
