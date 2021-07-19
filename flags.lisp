
(in-package :cl-user)
(ql:quickload
 '(:compdb/types :compdb/string-utils :compdb/dirs :compdb/lang-tag :str))
(defpackage :compdb/flags
  (:USE :common-lisp :compdb/types :compdb/lang-tag)
  (:IMPORT-FROM :compdb/string-utils #:str-append-char
                                     #:strs-union)
  (:IMPORT-FROM :compdb/dirs         #:join-pathnames
                                     #:dirpath
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

   #:def-flag-p

   #:inc-flag-p
   #:fixup-inc-flag-pair

   #:opt-with-arg-p
   #:join-opt-args

   #:spaceless-opt-arg-p
   #:split-spaceless-flag-arg

   #:flag-mark-scope
   #:flags-mark-scopes
   #:lolo-flags-get-common
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
        spaceless-opt-arg-p)
 (ftype (function (flaggable) boolean) def-flag-p inc-flag-p opt-with-arg-p)
 (ftype (function (dirpath flag-pair) flag-pair) fixup-inc-flag-pair)
 (ftype (function (list-of-flags flag) flag-scope) flag-mark-scope)
 (ftype (function (list-of-flags list-of-flags) T) flag-mark-scopes)
 (ftype (function (list-of-list-of-flags) list-of-flags) lolo-flags-get-common)
 (ftype (function (list-of-list-of-flags) T) lolo-flags-mark-scopes))


;; -------------------------------------------------------------------------- ;;

(defun flag-scope-p (x)
  "The argument `X' is a flag scope keyword?"
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
  "Argument `X' is a `cons' cell with a string `car', and either a `string' or
`path' as its `cdr'?"
  (the boolean (and (consp x)
                    (stringp (car x))
                    (or (stringp (cdr x))
                        (path-p  (cdr x)))
                    T)))

(deftype raw-flag-pair ()
  `(satisfies raw-flag-pair-p))


;; -------------------------------------------------------------------------- ;;

(defun flag-pair-p (x)
  "Argument `X' is of type `flag', or is explicitly a `raw-flag-pair'?"
  (the boolean (or (raw-flag-pair-p x)
                   (and (flag-p x)
                        (not (null (flag-arg x)))
                        T))))

(deftype flag-pair ()
  `(satisfies flag-pair-p))


;; -------------------------------------------------------------------------- ;;

(defun flaggable-p (x)
  "It is possible to interpret `X' as a flag?
More explicitly `X' is already a `flag', or `X' may be converted to `flag' by
the `as-flag' function.
Notably no checking is performed to see if `string' inputs are actually valid
compiler or linker flags, only type checking is performed."
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
  "A flag `F' is marked as `:LOCAL' scoped.
This indicated this flag differs from siblings - being other `cunit' members in
the same the same subdir, or other `subdir' members who share a parent dir."
  (declare (type flag f))
  (let ((s (flag-scope f)))
    (the boolean (and s (eq s :LOCAL)))))


(defun flag-common-p (f)
  "A flag `F' is marked as `:COMMON' scoped.
This indicated this flag is shared by siblings - being other `cunit' members in
the same the same subdir, or other `subdir' members who share a parent dir."
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
  "The ``flag like'' parameter `F' is an ``include style'' option.
This excludes the use of `--sysroot=<DIR>' options, but will recognize any other
GCC option which is prefixed with ``-i'' or ``-I''."
  (declare (type flaggable f))
  (let ((optstr (the string (etypecase f
                              (flag          (flag-opt f))
                              (raw-flag-pair (car f))
                              (string        f)))))
    (the boolean (and (str:starts-with-p "-i" optstr :IGNORE-CASE T)
                      T))))


;; -------------------------------------------------------------------------- ;;

(defun fixup-inc-flag-pair (builddir fpair)
  "Given a `(FLAG . RELATIVE-DIR)' pair, make directory absolute
using `builddir' as the parent directory.
Return the same type as the one provided, meaning when `fpair' is a
`raw-flag-pair' a modified `raw-flag-pair' is returned, while for a `flag' an
updated `flag' is returned."
  (declare (type dirpath builddir))
  (declare (type flag-pair fpair))

  (flet ((fix-inc-path (x) (join-pathnames builddir x)))
    (typecase fpair
      (flag  (the flag (make-flag :OPT   (flag-opt fpair)
                                  :ARG   (fix-inc-path (flag-arg fpair))
                                  :LANG  (flag-lang fpair)
                                  :SCOPE (flag-scope fpair))))
      (raw-flag-pair (the raw-flag-pair (cons (car fpair)
                                              (fix-inc-path (cdr fpair))))))))


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
  "The ``flaggable'' parameter `F' has an argument?
In cases where `F' is a string, the flag and argument must be space separated."
  (declare (type flaggable f))
  (the boolean
       (and (typecase f
              (flag          (find (flag-opt f) *opts-with-args* :TEST #'equal))
              (raw-flag-pair (find (car f) *opts-with-args* :TEST #'equal))
              (string
               (find-if (lambda (a) (str-space-starts-with-p a f))
                        *opts-with-args*)))
            T)))


;; -------------------------------------------------------------------------- ;;

(defun join-opt-args (args &key (join-char NIL))
  "Loop over a list of ``flaggable'' elements, joining any consecutive strings
which can be interpreted as `raw-flag-pair's into such pairs.
Other list members are left unmodified."
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

(defun def-flag-p (f)
  "The ``flaggable'' parameter `F' is a C-Family ``-D<NAME>'' or
``-D<NAME>=<VAL>'' option."
  (declare (type flaggable f))
  (the boolean
       (typecase f
         (string        (str:starts-with-p "-D" f))
         (raw-flag-pair (equal "-D" (car f)))
         (flag          (equal "-D" (flag-opt f))))))


;; -------------------------------------------------------------------------- ;;

(defun split-spaceless-flag-arg (f)
  "For a ``flaggable'' parameter `F' which satisfies ``spaceless-opt-arg-p'',
split `string' elements into `raw-flag-pair' cells.
Leave non-strings and strings which do not satisfy
``spaceless-opt-arg-p''unmodified."
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

(defun flag-mark-scope (common-flags f)
  "The ``scope'' of flags in `common-flags' is actually irrelevant, since they
represent a hierarchy of ownership.
Local flags should be those that are not in any ancestors."
  (declare (type list-of-flags common-flags))
  (declare (type flag f))
  (setf (flag-scope f)
        (if (null (find-if (lambda (o) (and
                                        (equal (flag-opt f) (flag-opt o))
                                        (equal (flag-arg f) (flag-arg o))
                                        (equal (flag-lang f) (flag-lang o))))
                           common-flags))
            :LOCAL
            :COMMON)))


;; -------------------------------------------------------------------------- ;;

(defun flags-mark-scopes (common-flags flags)
  (declare (type list-of-flags common-flags flags))
  (mapc (lambda (f) (flag-mark-scope common-flags f)) flags))


;; -------------------------------------------------------------------------- ;;

(defun lolo-flags-get-common (flagss)
  "Discover the collection of common flags from a group of flag lists."
  (flet ((unscoped-flags-eq (a b)
           (the boolean (and (equal (flag-opt a) (flag-opt b))
                             (equal (flag-arg a) (flag-arg b))
                             (equal (flag-lang a) (flag-lang b))))))
    (mapcar (lambda (f) (make-flag :OPT   (flag-opt f)
                                   :ARG   (flag-arg f)
                                   :LANG  (flag-lang f)
                                   :SCOPE :COMMON))
            (reduce (lambda (a b) (intersection a b :TEST #'unscoped-flags-eq))
                    flagss))))


;; -------------------------------------------------------------------------- ;;

(defun lolo-flags-mark-scopes (flagss)
  "From a collection of siblings, discover common flags, and set scopes.
Returns a `list-of-flags' of ``common'' flags
( initialized with default scope )."
  (declare (type list-of-list-of-flags flagss))
  (let ((commons (the list-of-flags (lolo-flags-get-common flagss))))
    (mapc (lambda (f) (flags-mark-scopes commons f)) flagss)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
