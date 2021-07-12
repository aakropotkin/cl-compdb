
(in-package :cl-user)
(defpackage :compdb/struct-utils
  (:USE :common-lisp)
  (:EXPORT
   #:update-struct
   #:struct-slots
   #:struct-values
   #:struct-p
   #:struct-to-list))

(in-package :compdb/struct-utils)


;; ========================================================================== ;;

(declaim
 (ftype (function (structure-object &REST T) structure-object) update-struct)
 (ftype (function (structure-object) list) struct-slots)
 (ftype (function (structure-object) cons) struct-values)
 (ftype (function (T) boolean) struct-p)
 (ftype (function (T) T) struct-to-list))


;; -------------------------------------------------------------------------- ;;

(defun update-struct (struct &rest bindings)
  "Copy `struct' with modified fields provided as `(quote FIELD) VALUE'
bindings in `&rest' parameters."
  (loop with copy = (copy-structure struct)
        for (slot value) on bindings by #'cddr
        do (setf (slot-value copy slot) value)
        finally (return copy)))


;; -------------------------------------------------------------------------- ;;
;;
;; Example Usage:
;;
;;  CL-USER> (defstruct foo bar baz)
;;  FOO
;;  CL-USER> (defparameter *first* (make-foo :bar 3))
;;  *FIRST*
;;  CL-USER> (defparameter *second* (update-struct *first* 'baz 2))
;;  *SECOND*
;;  CL-USER> (values *first* *second*)
;;  #S(FOO :BAR 3 :BAZ NIL)
;;  #S(FOO :BAR 3 :BAZ 2)
;;
;;
;; -------------------------------------------------------------------------- ;;

(defun struct-slots (struct)
  (declare (type structure-object struct))
  (loop for sl in (sb-mop::class-direct-slots (class-of struct))
        collect (list
                 (sb-mop:slot-definition-name sl)
                 (slot-value sl 'sb-pcl::internal-reader-function))))


;; -------------------------------------------------------------------------- ;;

(defun struct-values (st)
  (declare (type structure-object st))
  (cons (type-of st)
        (loop for np in (struct-slots st)
              collect (cons (car np)
                            (funcall (cadr np) st)))))


;; -------------------------------------------------------------------------- ;;

(defun struct-p (x)
  (equalp 'STRUCTURE-OBJECT
          (sb-mop:class-name
           (car (sb-mop:class-direct-superclasses (class-of x))))))

(deftype struct-t ()
  `(satisfies struct-p))


;; -------------------------------------------------------------------------- ;;

(defun struct-to-list (x)
  (typecase x
    (struct-t (loop for v in (struct-values x)
                    collect (struct-to-list v)))
    (cons     (cons (struct-to-list (car x))
                    (struct-to-list (cdr x))))
    (T        x)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
