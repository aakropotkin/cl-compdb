
(in-package :cl-user)
(defpackage :compdb/string-utils
  (:USE
   :common-lisp
   :compdb/types)
  (:EXPORT
   #:strs-equal
   #:strs-intersect
   #:strs-difference
   #:strs-union
   #:strs-uniq
   #:str-append-char
   ))

(in-package :compdb/string-utils)


;; ========================================================================== ;;

(declaim (ftype (function (list-of-strings list-of-strings) boolean)
                strs-equal)
         (ftype (function (list-of-strings list-of-strings) list-of-strings)
                strs-intersect
                strs-difference
                strs-union)
         (ftype (function (list-of-strings) list-of-strings) strs-uniq)
         (ftype (function (string character) string)))


;; -------------------------------------------------------------------------- ;;

(defun strs-equal (a b)
  "Set equality for lists of strings."
  (declare (type list-of-strings a b))
  (the boolean (null (set-exclusive-or a b :TEST #'equal))))


;; -------------------------------------------------------------------------- ;;

(defun strs-intersect (a b)
  (declare (type list-of-strings a b))
  (the list-of-strings (intersection a b :TEST #'equal)))

(defun strs-difference (a b)
  (declare (type list-of-strings a b))
  (the list-of-strings (set-difference a b :TEST #'equal)))

(defun strs-union (a b)
  (declare (type list-of-strings a b))
  (the list-of-strings (union a b :TEST #'equal)))


;; -------------------------------------------------------------------------- ;;

(defun strs-uniq (lst)
  (declare (type list-of-strings lst))
  (the list-of-strings (remove-duplicates lst :TEST #'equal)))


;; -------------------------------------------------------------------------- ;;

(defun str-append-char (str ch)
  (declare (type string str))
  (declare (type character ch))
  (the string (concatenate 'string str (string ch))))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
