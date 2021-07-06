
(defpackage :compdb/lang-tag
  (:USE
   :common-lisp
   :compdb/types)
  (:EXPORT
   #:lang-tag-p
   #:lang-tag
   #:list-of-lang-tags-p
   #:list-of-lang-tags
   #:lang-tag-from-flags
   #:lang-tag-from-compiler
   #:generated-lang-p
   ))

(in-package :compdb/lang-tag)


;; ========================================================================== ;;

(defun lang-tag-p (x)
  (member x (list :CC :CXX :YACC :LEX)))

(deftype lang-tag ()
  `(satisfies lang-tag-p))


;; -------------------------------------------------------------------------- ;;

(defun list-of-lang-tags-p (lst)
  (and (listp lst)
       (every (lambda (x) (typep x 'lang-tag)) lst)))

(deftype list-of-lang-tags ()
  `(satisfies list-of-lang-tags-p))


;; -------------------------------------------------------------------------- ;;

(defun lang-tag-from-flags (flags)
  (declare (type list-of-flags flags))
  (let ((fl (cadr (member "-x" flags :TEST #'equal))))
    (cond ((null fl) NIL)
          ((equal fl "c++") :CXX)
          ((equal fl "c")   :CC)
          (T                NIL))))


;; -------------------------------------------------------------------------- ;;

(defun lang-tag-from-compiler (compiler)
  (declare (type string compiler))
  (let ((c (file-namestring compiler)))
    (cond ((find c '("gcc" "cc" "icc" "clang") :TEST #'equal) :CC)
          ((find c '("g++" "clang++")          :TEST #'equal) :CXX)
          ((find c '("yacc" "bison")           :TEST #'equal) :YACC)
          ((find c '("lex" "flex")             :TEST #'equal) :LEX))))


;; -------------------------------------------------------------------------- ;;

(defun generated-lang-p (ltag)
  (declare (type lang-tag ltag))
  (if (member ltag '(:YACC :LEX)) T NIL))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
