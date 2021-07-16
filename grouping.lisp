
(ql:quickload :group-by)
(use-package :group-by)

(group-by '((a 1 2)
            (a 3 4)
            (b 5 6)))
; => ((A (1 2) (3 4)) (B (5 6)))

(group-by '((a 1 2) (a 3 4) (b 5 6)) :value #'identity)
; => ((A (A 1 2) (A 3 4)) (B (B 5 6)))

(ql:quickload :cl-json)
(use-package :cl-json)

(defparameter *cdb*
  (with-open-file (s #P"compile_commands.json" :direction :input)
    (decode-json s)))

(group-by *cdb*
          :key    (lambda (e) (assoc :DIRECTORY e))
          ;:key-fn #'cdr
          ;:value  #'identity
          :value (lambda (e) (assoc :FILE e))
          )
