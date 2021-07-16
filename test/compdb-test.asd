
#-asdf3 (error "compdb requires ASDF 3")


;; ========================================================================== ;;

(asdf:defsystem :compdb-test
  :NAME                 "Common Lisp Compilation Database Parser - Tests"
  :DESCRIPTION          "compdb-test: parse compile_commands.json files."
  :VERSION              "0.0.1"
  :AUTHOR               "Alex Ameen <alex.ameen.tx@gmail.com>"
  :LICENSE              "Public Domain"
  :CLASS                :package-inferred-system
  :DEFSYSTEM-DEPENDS-ON (:asdf-package-system)
  :DEPENDS-ON           (:compdb/test/test-all))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
