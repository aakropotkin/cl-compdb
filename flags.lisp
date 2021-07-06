
(defpackage :compdb/flags
  (:USE
   :common-lisp
   :compdb/types)
  (:IMPORT-FROM :compdb/string-utils #:str-append-char
                                     #:strs-union)
  (:IMPORT-FROM :compdb/dirs         #:join-pathnames)
  (:IMPORT-FROM :str                 #:starts-with-p)
  (:EXPORT
   #:spaceless-opt-arg-p
   #:inc-flag-p
   #:fixup-inc-flag-pair
   #:opt-with-arg-p
   #:join-opt-args
   #:def-flag-p
   #:split-spaceless-flag-arg
   ))

(in-package :compdb/flags)


;; ========================================================================== ;;

(defparameter *spaceless-opt-args*
  (list "-I" "-D" "-l" "-L" "-z" "-u" "-U" "-B" "-W" "-g" "-O"))


(defun spaceless-opt-arg-p (f &key (nospace NIL))
  (declare (type flag f))
  (declare (type boolean nospace))
  (if (listp f) (and (null nospace)
                     (find (car f) *spaceless-opt-args* :TEST #'equal))
      (and (stringp f)
           (< 2 (length f))
           (or (null nospace) (not (equal #\SPACE (char f 2))))
           (find f *spaceless-opt-args*
                 :TEST (lambda (f o) (str:starts-with-p o f))))))


;; -------------------------------------------------------------------------- ;;

(defparameter *include-args*
  (list
    "-I"
    "-idirafter"
    "-imacros"
    "-imultilib"
    "-include"
    "-iprefix"
    "-iquote"
    "-isysroot"
    "-isystem"
    "-iwithprefix"
    "-iwithprefixbefore"
    ;; FIXME: Technically `--sysroot=DIR' is an "include" option
   ))

(defun inc-flag-p (f)
  (declare (type flag f))
  (str:starts-with-p "-i" (if (stringp f) f (car f)) :IGNORE-CASE T))


;; -------------------------------------------------------------------------- ;;

(defun fixup-inc-flag-pair (builddir fpair)
  "Given a `(FLAG . RELATIVE-DIR)' pair, make directory absolute
using `builddir' as the parent directory."
  (declare (type pathnamep builddir))
  (declare (type flag-pair fpair))
  (cons (car fpair) (join-pathnames builddir (cdr fpair))))


;; -------------------------------------------------------------------------- ;;

(defparameter *opts-with-args*
  (concatenate 'list
   (strs-union *spaceless-opt-args* *include-args*)
   (list
    "--param"
    "-A"
    "-G"
    "-MF"
    "-MQ"
    "-MT"
    "-T"
    "-Xassembler"
    "-Xlinker"
    "-Xpreprocessor"
    "-allowable_client"
    "-aux-info"
    "-bundle_loader"
    "-e"
    "-o"
    "-x"
    ;; FIXME: This does not include many options of the form `--OPT=ARG'
    )))


(defun str-space-starts-with-p (prefix str)
  (let ((spre (str-append-char (prefix #\SPACE))))
    (str:starts-with-p spre str)))

(defun opt-with-arg-p (f)
  (declare (type flag f))
  (cond
    ;; Flag Pair
    ((listp f) (find (car f) *opts-with-args* :TEST #'equal))
    ;; Spaceless option with argument
    ((spaceless-opt-arg-p f) f)
    ;; Standalone option ( argument is not in the same string )
    ((find f *opts-with-args* :TEST #'equal) f)
    ;; Flag and argument are in the same string, space separated
    ((find f (lambda (fl a) (str-space-starts-with-p a fl)) *opts-with-args*) f)
    ;; Fail
    (T NIL)))


;; -------------------------------------------------------------------------- ;;

(defun join-opt-args (args &key (join-char NIL))
  (declare (type list-of-flags args))
  (declare (type (or character string null) join-char))
  (let ((jc-str (if (and join-char (characterp join-char)) (string join-char))))
    (loop for a in args
          for argp = (find a *opts-with-args* :TEST #'equal)
          and joinp = NIL then argp
          when (null argp)
            collect (if (null joinp) a
                        (if join-char (concatenate 'string joinp jc-str a)
                            (cons joinp a))))))


;; -------------------------------------------------------------------------- ;;

(defun def-flag-p (f)
  (declare (type flag f))
  (if (stringp f) (str:starts-with-p "-D" f) (equal "-D" (car f))))


;; -------------------------------------------------------------------------- ;;

(defun split-spaceless-flag-arg (f)
  (declare (type flag f))
  ;; If `f' isn't a string or an include flag don't change it.
  (if (not (and (stringp f) (spaceless-opt-arg-p f))) f
      ;; If there is a space just split there.
      (let ((space-pos (position #\SPACE f)))
        (if space-pos (cons (subseq f 0 space-pos)
                            (subseq f (+ space-pos 1) (length f)))
            ;; Split immediately after option.
            (let ((opt (find f *spaceless-opt-args*
                             :TEST (lambda (f o) (str:starts-with-p o f)))))
              (if (null opt) NIL  ;; This shouldn't happen
                  (cons opt (subseq f (length opt) (length f)))))))))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
