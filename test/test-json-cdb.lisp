
(in-package :cl-user)
(ql:quickload '(:prove :compdb/json-cdb))
(defpackage :compdb/test/test-json-cdb
  (:USE :common-lisp :prove :compdb/json-cdb)
  (:IMPORT-FROM :asdf #:system-relative-pathname)
  (:EXPORT
   #:test-parse-compdb-json
   #:run-test-json-cdb))
(in-package :compdb/test/test-json-cdb)


;; ========================================================================== ;;

(defun test-parse-compdb-json ()
  (subtest "Test `parse-compdb-json' function."
    (let ((cdb (parse-compdb-json
                (system-relative-pathname :COMPDB
                                          "compile_commands.json"))))
      (is-type cdb 'list)
      (is-type (assoc :ARGUMENTS (car cdb)) 'list)
      (is-type (assoc :DIRECTORY (car cdb)) 'cons)
      (is-type (assoc :OUTPUT    (car cdb)) 'cons)
      (is-type (assoc :FILE      (car cdb)) 'cons))))


;; -------------------------------------------------------------------------- ;;

(defparameter *cdb1*
  (parse-compdb-json (system-relative-pathname :COMPDB
                                               "compile_commands.json")))


;; -------------------------------------------------------------------------- ;;

(defun test-get-jcu-args ()
  (subtest "Test `get-jcu-args' function."
    (let ((jcu1 (car *cdb1*))
          (jcu2 (cadr *cdb1*)))
      )))


;; -------------------------------------------------------------------------- ;;

(defun run-test-json-cdb ()
  (test-parse-compdb-json))


;; -------------------------------------------------------------------------- ;;



;; ========================================================================== ;;
