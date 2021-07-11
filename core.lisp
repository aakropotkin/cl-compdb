
(in-package :cl-user)
(defpackage :compdb/core
  (:USE
   :common-lisp)
  (:IMPORT-FROM :compdb/json-cdb #:parse-compdb-json)
  (:EXPORT
   #:*cdb*))

(in-package :compdb/core)


;; ========================================================================== ;;

;; FIXME: This if just for testing.
(defparameter *cdb* (parse-compdb-json "compile_commands.json"))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
