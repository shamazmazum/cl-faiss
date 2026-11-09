(defun do-all()
  (handler-case
      (asdf:load-system :cl-faiss/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "cl-faiss/tests:run-tests")
       0 1)))

(do-all)
