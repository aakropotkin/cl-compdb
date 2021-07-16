
;; ========================================================================== ;;

(uiop/package:define-package :compdb/test/all
    (:NICKNAMES :compdb/test)
  (:USE-REEXPORT
   :compdb/test/test-alias
   :compdb/test/test-compdb
   :compdb/test/test-core
   :compdb/test/test-cunit
   :compdb/test/test-dirs
   :compdb/test/test-flags
   :compdb/test/test-flag-collection
   :compdb/test/test-json-cdb
   :compdb/test/test-lang-tag
   :compdb/test/test-srcdir
   :compdb/test/test-string-utils
   :compdb/test/test-struct-utils
   :compdb/test/test-types))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
