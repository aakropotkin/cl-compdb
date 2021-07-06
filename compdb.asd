
#-asdf3 (error "compdb requires ASDF 3")


;; ========================================================================== ;;

(asdf:defsystem :compdb
  :NAME                 "Common Lisp Compilation Database Parser"
  :DESCRIPTION          "compdb: parse compile_commands.json files."
  :VERSION              "0.0.1"
  :AUTHOR               "Alex Ameen <alex.ameen.tx@gmail.com>"
  :LICENSE              "Public Domain"
  :CLASS                :package-inferred-system
  :DEFSYSTEM-DEPENDS-ON (:asdf-package-system)
  :DEPENDS-ON           (:compdb/all))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
