
(in-package :cl-user)
(ql:quickload
 '(:compdb/types :compdb/string-utils :compdb/dirs :compdb/lang-tag :str))
(defpackage :compdb/flags
  (:USE :common-lisp :compdb/types :compdb/lang-tag)
  (:IMPORT-FROM :compdb/string-utils #:str-append-char
                                     #:strs-union)
  (:IMPORT-FROM :compdb/dirs         #:join-pathnames
                                     #:path-p
                                     #:path)
  (:IMPORT-FROM :str                 #:starts-with-p)
  (:EXPORT
   #:flag-pair-p
   #:flag-pair

   #:make-flag
   #:copy-flag
   #:flag-p
   #:flag
   #:flag-opt
   #:flag-arg
   #:flag-lang
   #:flag-scope
   #:list-of-flags-p
   #:list-of-flags
   #:list-of-list-of-flags-p
   #:list-of-list-of-flags

   #:flag-local-p
   #:flag-common-p

   #:flaggable-p
   #:flaggable
   #:list-of-flaggables-p
   #:list-of-flaggables

   #:as-flag
   #:as-raw-flag-pair
   #:as-flag-string

   #:spaceless-opt-arg-p
   #:inc-flag-p
   #:fixup-inc-flag-pair
   #:opt-with-arg-p
   #:join-opt-args
   #:def-flag-p
   #:split-spaceless-flag-arg

   #:flag-mark-scope
   #:flags-mark-scopes
   #:lolo-flags-mark-scopes
   ))

(in-package :compdb/flags)


;; ========================================================================== ;;

(declaim
 (ftype (function (flag) boolean)
        flag-local-p
        flag-common-p)
 (ftype (function (T) boolean)
        flag-pair-p
        flag-p
        list-of-flags-p
        list-of-list-of-flags-p)
 (ftype (function (flaggable &key (:NOSPACE boolean)) boolean)
        spaceless-opt-arg-p
        def-flag-p)
 (ftype (function (flag) boolean) inc-flag-p opt-with-arg-p)
 (ftype (function (pathname flag-pair) (or flag-pair flag))
        fixup-inc-flag-pair)
 (ftype (function (list-of-flags flag) boolean) flag-mark-scope)
 (ftype (function (list-of-flags list-of-flag) T) flag-mark-scopes)
 (ftype (function (list-of-list-of-flags) list-of-flags)
        lolo-flags-mark-scopes))


;; -------------------------------------------------------------------------- ;;

(defun flag-scope-p (x)
  (and (member x (list :LOCAL :COMMON))
       T))

(deftype flag-scope ()
  `(satisfies flag-scope-p))


;; -------------------------------------------------------------------------- ;;

(defstruct flag
  (opt   NIL :TYPE (or string null))
  (arg   NIL :TYPE (or string path null))
  (lang  NIL :TYPE (or lang-tag null))
  (scope NIL :TYPE (or flag-scope null)))

(def-list-type flags flag)
(def-list-type list-of-flags list-of-flags)


;; -------------------------------------------------------------------------- ;;

(defun raw-flag-pair-p (x)
  (the boolean (and (consp x)
                    (stringp (car x))
                    (or (stringp (cdr x))
                        (path-p  (cdr x)))
                    T)))

(deftype raw-flag-pair ()
  `(satisfies raw-flag-pair-p))


;; -------------------------------------------------------------------------- ;;

(defun flag-pair-p (x)
  (the boolean (or (raw-flag-pair-p x)
                   (and (flag-p x)
                        (not (null (flag-arg x)))
                        T))))

(deftype flag-pair ()
  `(satisfies flag-pair-p))


;; -------------------------------------------------------------------------- ;;

(defun flaggable-p (x)
  (the boolean (and (or (stringp x)
                        (flag-p x)
                        (raw-flag-pair-p x))
                    T)))

(deftype flaggable ()
  `(satisfies flaggable-p))

(def-list-type flaggables flaggable)


;; -------------------------------------------------------------------------- ;;

(declaim
 (ftype (function
         (list-of-flaggables &key (:JOIN-CHAR (or character string null)))
         list-of-flaggables)
        join-opt-args)
 (ftype (function (flaggable) flaggable) split-spaceless-flag-arg)
 (ftype (function (flaggable &key (:LANG-FB (or lang-tag null))
                                  (:SCOPE-FB (or flag-scope null)))
                  flag) as-flag)
 (ftype (function (flaggable) raw-flag-pair) as-raw-flag-pair)
 (ftype (function (flaggable) string) as-flag-string))


;; -------------------------------------------------------------------------- ;;

(defun as-flag (x &key (lang-fb NIL) (scope-fb NIL))
  "Convert a `flaggable' argument to a `flag', using the optional fall-back keys
`lang-fb' and `scope-fb' for `:LANG' and `:SCOPE' values respectively.
An error is thrown when conversion is not possible."
  (declare (type flaggable x))
  (declare (type (or lang-tag null) lang-fb))
  (declare (type (or flag-scope null) scope-fb))
  (the flag
       (etypecase x
         (flag          x)
         (raw-flag-pair (make-flag :OPT   (car x)
                                   :ARG   (cdr x)
                                   :LANG  lang-fb
                                   :SCOPE scope-fb))
         (string        (make-flag :OPT   x
                                   :LANG  lang-fb
                                   :SCOPE scope-fb)))))


;; -------------------------------------------------------------------------- ;;

(defun as-raw-flag-pair (x)
  "Convert a `flaggable' argument to a raw flag pair, throwing an error in cases
where conversion is not possible."
  (declare (type flaggable x))
  (the raw-flag-pair
       (etypecase x
         (raw-flag-pair  x)
         (flag           (cons (the string (flag-opt x))
                               (the string (flag-arg x))))
         (string
          (let ((maybe-split (split-spaceless-flag-arg x)))
            (etypecase maybe-split
              (raw-flag-pair maybe-split)
              (flag          (cons (the string (flag-opt maybe-split))
                                   (the string (flag-arg maybe-split))))))))))


;; -------------------------------------------------------------------------- ;;

(defun as-flag-string (x)
  "Convert a `flaggable' argument to a string.
In cases where arguments are present a space separator will be used to create a
SINGLE string output.
Note that this will not reproduce the original argument list, which separates
options and arguments as separate strings.
An error is thrown when conversion is not possible."
  (declare (type flaggable x))
  (the string
       (etypecase x
         (string        x)
         (flag          (concatenate 'string (flag-opt x) " " (flag-arg x)))
         (raw-flag-pair (if (null (cdr x)) (car x)
                            (concatenate 'string (car x) " " (cadr x)))))))


;; -------------------------------------------------------------------------- ;;

(defun flag-local-p (f)
  (declare (type flag f))
  (let ((s (flag-scope f)))
    (the boolean (and s (eq s :LOCAL)))))


(defun flag-common-p (f)
  (declare (type flag f))
  (let ((s (flag-scope f)))
    (the boolean (and s (eq s :COMMON)))))


;; -------------------------------------------------------------------------- ;;

(defparameter *spaceless-opt-args*
  (list "-I" "-D" "-l" "-L" "-z" "-u" "-U" "-B" "-W" "-g" "-O"))


(defun spaceless-opt-arg-p (f &key (nospace NIL))
  (declare (type flaggable f))
  (declare (type boolean nospace))
  (the boolean
       (typecase f
         (flag (and (not nospace)
                    (not (null (flag-arg f)))
                    (find (flag-opt f) *spaceless-opt-args* :TEST #'equal)
                    T))
         (raw-flag-pair (and (not nospace)
                             (find (car f) *spaceless-opt-args* :TEST #'equal)
                             T))
         (string (and (< 2 (length f))
                      (or (null nospace) (not (equal #\SPACE (char f 2))))
                      (find-if (lambda (o) (str:starts-with-p o f))
                               *spaceless-opt-args*)
                      T)))))


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
  (declare (type flaggable f))
  (let ((optstr (the string (typecase f
                              (flag          (flag-opt f))
                              (raw-flag-pair (car f))
                              (string        f)))))
    (the boolean (str:starts-with-p "-i" optstr :IGNORE-CASE T))))


;; -------------------------------------------------------------------------- ;;

;; FIXME: Use dirpath, use `flag' instead of `scoped-flag'
(defun fixup-inc-flag-pair (builddir fpair)
  "Given a `(FLAG . RELATIVE-DIR)' pair, make directory absolute
using `builddir' as the parent directory."
  (declare (type pathname builddir))
  (declare (type (or flag-pair scoped-flag) fpair))
  (let ((new-pair (the flag-pair (cons (car fpair)
                                       (join-pathnames
                                        builddir
                                        (cdr (as-flag fpair)))))))
    (if (scoped-flag-p fpair)
        (make-scoped-flag :LOCAL (scoped-flag-local-p fpair) :FLAG new-pair)
        new-pair)))


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
  (declare (type string prefix str))
  (let ((spre (str-append-char prefix #\SPACE)))
    (the boolean (str:starts-with-p spre str))))

(defun opt-with-arg-p (f)
  (declare (type flag f))
  (the boolean
       (cond
         ;; Flag Pair
         ((listp f) (and (find (car f) *opts-with-args* :TEST #'equal) T))
         ;; Spaceless option with argument
         ((spaceless-opt-arg-p f) T)
         ;; Standalone option ( argument is not in the same string )
         ((find f *opts-with-args* :TEST #'equal) T)
         ;; Flag and argument are in the same string, space separated
         ((find-if (lambda (a) (str-space-starts-with-p a f))
                   *opts-with-args*)
          T)
         ;; Fail
         (T NIL))))


;; -------------------------------------------------------------------------- ;;

(defun join-opt-args (args &key (join-char NIL))
  (declare (type list-of-flaggables args))
  (declare (type (or character string null) join-char))
  (the list-of-flaggables
       (let ((jc-str (if (and join-char (characterp join-char))
                         (string join-char)
                         join-char)))
         (loop for a in args
               for argp = (find a *opts-with-args* :TEST #'equal)
               and joinp = NIL then argp
               when (null argp)
                 collect (the flaggable
                              (if (null joinp) a
                                  (if join-char
                                      (concatenate 'string joinp jc-str a)
                                      (cons joinp a))))))))


;; -------------------------------------------------------------------------- ;;

(defun def-flag-p (x)
  (declare (type flaggable x))
  (the boolean
       (typecase x
         (string        (str:starts-with-p "-D" x))
         (raw-flag-pair (equal "-D" (car x)))
         (flag          (equal "-E" (flag-opt x))))))


;; -------------------------------------------------------------------------- ;;

(defun split-spaceless-flag-arg (f)
  (declare (type flaggable f))
  ;; If `f' isn't a string or an include flag don't change it.
  (the flaggable
       (if (not (and (stringp f) (spaceless-opt-arg-p f))) f
           ;; If there is a space just split there.
           (let ((space-pos (the (or fixnum null) (position #\SPACE f))))
             (if space-pos
                 (cons (subseq f 0 space-pos)
                       (subseq f (+ space-pos 1) (length f)))
                 ;; Split immediately after option.
                 (let ((opt (find-if (lambda (o) (str:starts-with-p o f))
                                     *spaceless-opt-args*)))
                   (or (and opt
                            (cons opt
                                  (subseq f (length opt) (length f))))
                       f)))))))


;; -------------------------------------------------------------------------- ;;

(defun scoped-flag-mark-scope (common-sflags sf)
  "The ``scope'' of flags in `common-flags' is actually irrelevant, since they
represent a hierarchy of ownership.
Local flags should be those that are not in any ancestors."
  (declare (type list-of-scoped-flags common-sflags))
  (declare (type scoped-flag sf))
  (setf (scoped-flag-local sf)
        (null (find (scoped-flag-flag sf) common-sflags :KEY  #'scoped-flag-flag
                                                        :TEST #'equal))))


;; -------------------------------------------------------------------------- ;;

(defun scoped-flags-mark-scopes (common-sflags sflags)
  (declare (type list-of-scoped-flags common-sflags sflags))
  (mapc (lambda (sf) (scoped-flag-mark-scope common-sflags sf)) sflags))


;; -------------------------------------------------------------------------- ;;

(defun lolo-scoped-flags-mark-scopes (sflagss)
  "From a collection of siblings, discover common flags, and set scopes.
Returns a `list-of-scoped-flags' of ``common'' flags
( initialized with default scope )."
  (declare (type list-of-list-of-scoped-flags sflagss))
  (let* ((flagss  (mapcar (lambda (sfs) (mapcar #'as-flag sfs)) sflagss))
         (common  (reduce (lambda (a b) (intersection a b :TEST #'equal))
                          flagss))
         (sf-comm (mapcar (lambda (f) (make-scoped-flag :FLAG f)) common)))
    (mapc (lambda (sf) (scoped-flags-mark-scopes sf-comm sf)) sflagss)
    sf-comm))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
