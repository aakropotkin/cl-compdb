
(defpackage :compdb/compdb
  (:USE
   :common-lisp
   :compdb/types)
  (:IMPORT-FROM :compdb/lang-tag        #:list-of-lang-tags)
  (:IMPORT-FROM :compdb/srcdir          #:list-of-srcdirs)
  (:IMPORT-FROM :compdb/flag-collection #:flag-collection)
  (:IMPORT-FROM :compdb/json-cdb        #:parse-compdb-json
                                        #:get-jcu-src-dir
                                        #:get-jcu-output
                                        #:get-jcu-build-dir)
  (:IMPORT-FROM :compdb/dirs            #:subpath-p
                                        #:as-pathname
                                        #:get-paths-roots)
  (:IMPORT-FROM :uiop                   #:pathname-directory-pathname)
  (:EXPORT
   #:compdb
   #:compdb-p
   #:make-compdb
   #:compdb-json-obj
   #:compdb-dbfile
   #:compdb-src-roots
   #:compdb-obj-roots
   #:compdb-build-roots
   #:compdb-languages
   #:compdb-compilers
   #:compdb-global-flags

   #:mk-compdb))

(in-package :compdb/compdb)


;; ========================================================================== ;;

(defstruct compdb
  (json-obj     '()   :TYPE list)
  (dbfile       #P"/" :TYPE path)
  (src-roots    '()   :TYPE list-of-srcdirs)
  (obj-roots    '()   :TYPE list-of-dirpaths)
  (build-roots  '()   :TYPE list-of-dirpaths)
  (languages    '()   :TYPE list-of-lang-tags)
  (compilers    '()   :TYPE list-of-strings)
  (global-flags NIL   :TYPE (or flag-collection null)))


;; -------------------------------------------------------------------------- ;;




;; -------------------------------------------------------------------------- ;;

(defun mk-compdb (cdb-path)
  (declare (type pathname cdb-path))
  (let* ((jcdb (parse-compdb-json cdb-path))
         (srcdirs (remove-duplicates (mapcar #'get-jcu-src-dir jcdb)))
         (srcroots (get-paths-roots srcdirs))
         (objdirs (remove-duplicates (mapcar #'uiop:pathname-directory-pathname
                                             (mapcar #'get-jcu-output jcdb))))
         (builddirs (remove-duplicates (mapcar #'get-jcu-build-dir jcdb))))
    (make-compdb
     :JSON-OBJ    jcdb
     :DBFILE      (as-pathname cdb-path)
     :SRC-ROOTS   (get-paths-roots srcdirs)
     :OBJ-ROOTS   (get-paths-roots objdirs)
     :BUILD-ROOTS (get-paths-roots builddirs)
     )))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
