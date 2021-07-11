
(in-package :cl-user)
(defpackage :compdb/alias
  (:USE :common-lisp)
  (:IMPORT-FROM :str          #:replace-first)
  (:IMPORT-FROM :compdb/types #:list-of-symbols)
  (:EXPORT
   #:defalias
   #:defalias-replace-first
   #:defaliases-replace-first
   ))

(in-package :compdb/alias)


;; ========================================================================== ;;


(defun defun-alias (old new)
  (declare (type symbol old new))
  (assert (fboundp old))
  (setf (symbol-function new) (symbol-function old)))

(defun defvar-alias (old new)
  (declare (type symbol old new))
  (assert (boundp old))
  (setf (symbol-value new) (symbol-value old)))

(defun defalias (old new)
  (declare (type symbol old new))
  (if (fboundp old) (defun-alias old new) (defvar-alias old new)))


;; -------------------------------------------------------------------------- ;;

(defun defalias-replace-first (str-old str-new sym)
  (declare (type string str-old str-new))
  (declare (type symbol sym))
  (defalias sym (make-symbol
                 (str:replace-first str-old str-new (symbol-name sym)))))

(defun defaliases-replace-first (str-old str-new syms)
  (declare (type string str-old str-new))
  (declare (type list-of-symbols syms))
  (mapc (lambda (sym) (defalias-replace-first str-old str-new sym)) syms))


;; -------------------------------------------------------------------------- ;;

;;(defun defstruct-aliases (struct-sym newname)
;;  (declare (type symbol struct-sym))
;;  (declare (type string newname))
;;  (let* ((sname      (symbol-name struct-sym))
;;         (sclass     (find-class struct-sym))
;;         (slot-syms  (mapcar
;;                      (lambda (s)
;;                        (cons
;;                         (slot-value s 'sb-pcl::name)
;;                         (slot-value s 'sb-pcl::defstruct-accessor-symbol)))
;;                      (sb-mop:class-direct-slots sclass)))
;;         (ctor-sym   (slot-value sclass 'sb-pcl::defstruct-constructor))
;;         )
;;    (flet ((def-slot-alias (slot-syms)
;;             (let* ((slot-name    (symbol-name (car slot-syms)))
;;                    (new-acc-name (concatenate 'string newname "-" slot-name)))
;;               (setf (symbol-function (make-symbol new-acc-name)) (cadr slot-syms))))
;;           (def-ctor-alias (ctor-sym)
;;             (setf (symbol-function (make-symbol (concatenate 'string "make-" newname)))
;;                   ctor-sym))
;;           (def-pred-alias ()
;;             (setf (symbol-function
;;                    (make-symbol (concatenate 'string newname "-p")))
;;                   (lambda (x) (typep x sclass)))))
;;      ()
;;             )))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
