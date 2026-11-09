(defsystem :cl-faiss
  :name :cl-faiss
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A wrapper around faiss (k-NN search library)"
  :license "2-clause BSD"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "faiss"))
  :depends-on (:cffi :serapeum)
  :in-order-to ((test-op (load-op "cl-faiss/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-faiss/tests '#:run-tests)))

(defsystem :cl-faiss/tests
  :name :cl-faiss/tests
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "pqueue")
               (:file "tests"))
  :depends-on (:cl-faiss
               :alexandria
               :fiveam
               :approx))
