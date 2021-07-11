
(in-package :cl-user)
(defpackage :compdb/string-utils
  (:USE
   :common-lisp
   :compdb/types)
  (:EXPORT
   #:strs-intersect
   #:strs-difference
   #:strs-union
   #:strs-uniq
   #:str-append-char
   ))

(in-package :compdb/string-utils)


;; ========================================================================== ;;

(defun strs-intersect (a b)
  (declare (type list-of-strings a b))
  (intersection a b :TEST #'equal))

(defun strs-difference (a b)
  (declare (type list-of-strings a b))
  (set-difference a b :TEST #'equal))

(defun strs-union (a b)
  (declare (type list-of-strings a b))
  (union a b :TEST #'equal))

(defun strs-uniq (lst)
  (declare (type list-of-strings lst))
  (remove-duplicates lst :TEST #'equal))


;; -------------------------------------------------------------------------- ;;

(defun str-append-char (str ch)
  (declare (type string str))
  (declare (type character ch))
  (concatenate 'string str (string ch)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
