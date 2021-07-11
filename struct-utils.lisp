
(in-package :cl-user)
(defpackage :compdb/struct-utils
  (:USE :common-lisp)
  (:EXPORT #:update-struct))

(in-package :compdb/string-utils)


;; ========================================================================== ;;

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



;; ========================================================================== ;;
