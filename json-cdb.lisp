
(in-package :cl-user)
(defpackage :compdb/json-cdb
  (:USE
   :common-lisp
   :compdb/types)
  (:IMPORT-FROM :cl-json         #:decode-json)
  (:IMPORT-FROM :compdb/dirs     #:parse-dir-namestring
                                 #:join-pathnames)
  (:IMPORT-FROM :compdb/lang-tag #:lang-tag-from-flags
                                 #:lang-tag-from-compiler)
  (:IMPORT-FROM :compdb/flags    #:join-opt-args
                                 #:split-spaceless-flag-arg)
  (:IMPORT-FROM :uiop            #:pathname-directory-pathname)
  (:IMPORT-FROM :str             #:starts-with-p)
  (:EXPORT
   #:get-jcu-args
   #:get-jcu-compiler
   #:get-jcu-lang-tag
   #:get-jcu-src
   #:get-jcu-output
   #:parse-compdb-json
   ))

(in-package :compdb/json-cdb)


;; ========================================================================== ;;

(defun get-jcu-member (member jcu)
  (declare (type cons jcu))
  (loop for x in jcu
        when (equal member (car x))
          return (cdr x)))

(defun get-jcu-args
    (jcu &key (raw NIL)
              (join-char NIL))
  (declare (type cons jcu))
  (declare (type boolean raw))
  (declare (type (or character string null) join-char))
  (let* ((r (get-jcu-member :ARGUMENTS cu)))
    (if raw r  ;; Don't join arguments.
        (if join-char (join-opt-args r :JOIN-CHAR join-char)
            ;; Split "spaceless argument" flags into `( FLAG . ARGUMENT )'.
            (mapcar #'split-spaceless-flag-arg (join-opt-args r))))))

(defun get-jcu-compiler (jcu)
  (declare (type cons jcu))
  (car (get-jcu-member :ARGUMENTS jcu)))


;; -------------------------------------------------------------------------- ;;

(defun get-jcu-lang-tag (jcu &key (compiler NIL))
  (declare (type cunit jcu))
  (declare (type (or string null) compiler))
  (or (lang-tag-from-flags (get-jcu-args jcu))
      (lang-tag-from-compiler (or compiler (get-jcu-compiler jcu)))))


;; -------------------------------------------------------------------------- ;;

(defun get-jcu-build-dir (jcu)
  (declare (type cons jcu))
  (parse-dir-namestring (get-jcu-member :DIRECTORY jcu)))

(defun get-jcu-src (jcu &key (relative NIL))
  (declare (type cons jcu))
  (declare (type boolean relative))
  (let ((frel (parse-namestring (get-jcu-member :FILE jcu))))
    (if relative frel (join-pathnames (get-jcu-build-dir jcu) frel))))

(defun get-jcu-src-dir (jcu &key (relative NIL))
  (declare (type cons jcu))
  (declare (type boolean relative))
  (uiop:pathname-directory-pathname (get-jcu-src jcu :RELATIVE relative)))


;; -------------------------------------------------------------------------- ;;

(defun get-output-from-args (jcu &key (relative NIL))
  (declare (type cons jcu))
  (declare (type boolean relative))
  (let* ((arglist (get-jcu-args jcu))
         (out-arg (find-if (lambda (a) (str:starts-with-p "-o " a)) arglist))
         (ofile   (if out-arg
                      (parse-namestring (subseq out-arg 3 (length out-arg)))
                      NIL)))
    (if (or (null ofile) relative) ofile
        (join-pathnames (get-jcu-build-dir jcu) ofile))))

(defun get-jcu-output (jcu &key (relative NIL))
  (declare (type cons jcu))
  (declare (type boolean relative))
  (let ((output (get-jcu-member :OUTPUT jcu)))
    (if output (join-pathnames (get-jcu-build-dir jcu)
                               (parse-namestring output))
        (get-output-from-args jcu :RELATIVE relative))))



;; -------------------------------------------------------------------------- ;;

(defun parse-compdb-json (fpath)
  (declare (type (or string pathname) fpath))
  (with-open-file (fjson fpath)
    (json:decode-json fjson)))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
