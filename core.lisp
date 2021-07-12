
(in-package :cl-user)
(ql:quickload '(:asdf :compdb/json-cdb))
(defpackage :compdb/core
  (:USE :common-lisp)
  (:IMPORT-FROM :compdb/json-cdb #:parse-compdb-json)
  (:IMPORT-FROM :asdf            #:system-relative-pathname)
  (:EXPORT #:*cdb*))

(in-package :compdb/core)


;; ========================================================================== ;;

;; FIXME: This if just for testing.
(defparameter *cdb*
 (parse-compdb-json (system-relative-pathname :COMPDB "compile_commands.json")))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
